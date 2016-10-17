package services

import java.time.{Duration, ZonedDateTime}

import com.google.inject.{Inject, Singleton}
import model.Utils._
import model._
import play.api.db.slick.DatabaseConfigProvider
import play.api.{Environment, Logger}
import services.SlickPgPostgresDriver.api._
import slick.dbio.DBIOAction
import slick.dbio.Effect.Read
import slick.driver.JdbcProfile

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class FeedStore @Inject()(dbConfigProvider: DatabaseConfigProvider, env: Environment)(implicit exec: ExecutionContext) {
  val db = dbConfigProvider.get[JdbcProfile].db

  Logger.info("Schemas:\n" +
    (downloads.schema ++ sources.schema ++ feeds.schema ++ articles.schema).create.statements.mkString("\n"))

  val extractEpoch = SimpleExpression.unary[Duration, Long] { (zdt, qb) =>
    qb.sqlBuilder += "EXTRACT(EPOCH FROM "
    qb.expr(zdt)
    qb.sqlBuilder += ")"
  }

  val pow = SimpleFunction.binary[Double, Double, Double]("POWER")

  def updateSourceTimestamp(source: FeedSource) = db.run {
    val liveSourceTimestamp = for {
      liveSource <- sources.filter(_.id === source.id)
    } yield liveSource.timestamp
    liveSourceTimestamp.update(source.timestamp)
  }

  def resolvePermalink(section: String, permalink: Option[Permalink]): Future[Option[Permalink]] = {
    Futures.traverse(permalink) { permalink =>
      db.run {
        feeds.historic(section, permalink.timestamp).map(_.timestamp).max.result map { resolvedTimestamp =>
          resolvedTimestamp.filterNot(_.UTC == permalink.timestamp.UTC).map(ts => Permalink(ts, permalink.pageNum))
        }
      }
    } map (_.flatten)
  }

  def articlePageByFrecency(section: String, permalink: Option[Permalink])
                           (implicit now: ZonedDateTime): Future[ArticlesResult] = db.run {
    val searchPermalink = permalink getOrElse Permalink(now, 1)
    feeds.historic(section, searchPermalink.timestamp).map(_.timestamp).max.result.flatMap {
      _.fold {
        DBIO.successful(ArticlesResult(Seq.empty, None, permalink, None)): DBIOAction[ArticlesResult, NoStream, Read]
      } { resolvedTimestamp =>
        val articlesWithSourceAndFeed = for {
          s <- sources.bySection(section)
          f <- feeds if f.sourceId === s.id && f.timestamp <= resolvedTimestamp
          a <- articles if a.feedId === f.id
        } yield (s, f, a)

        val offset = (searchPermalink.pageNum - 1) * searchPermalink.pageSize
        val limit = searchPermalink.pageSize + 1 // one extra for next page detection

        val articlePage = articlesWithSourceAndFeed sortBy { case (s, f, a) =>
          val ageSeconds = extractEpoch(resolvedTimestamp.bind - a.pubDate).asColumnOf[Double] / 1000.0
          pow(ageSeconds, 3.0.bind) * f.groupFrequency
        } drop offset take limit

        articlePage.result.map(_.map(CachedArticle.tupled)).map { as =>
          val articlePage = as.take(searchPermalink.pageSize)
          val resolvedPermalink = Permalink(resolvedTimestamp, searchPermalink.pageNum)
          val nextPage = Some(searchPermalink.pageNum + 1).filter(_ => as.size > searchPermalink.pageSize)

          ArticlesResult(articlePage, Some(resolvedPermalink), permalink, nextPage)
        }
      }
    }
  }

  def loadDownload(downloadId: Long): Future[Option[Download]] = db.run {
    Logger.info("---> Loading download: " + downloadId)
    downloads.byId(Some(downloadId)).result.headOption
  }

  def loadIncompleteDownloadIds(sourceGroup: Seq[FeedSource], parseVersion: Int): Future[Seq[(FeedSource, Long)]] = db.run {
    (for {
      source <- sources if source.id inSet sourceGroup.map(_.id)
      download <- downloads if download.sourceId === source.id && !feeds.filter(f => f.downloadId === download.id && f.parseVersion >= parseVersion).exists
    } yield (source, download)).sortBy(_._2.timestamp).map(sd => (sd._1, sd._2.id.get)).result
  } tap { eventualDownloadIdsBySource =>
    for (downloadIdsBySource <- eventualDownloadIdsBySource if downloadIdsBySource.nonEmpty)
      Logger.info(s".l.> Loaded Ids of ${downloadIdsBySource.size} Unparsed or Out of Version Downloads: ${sourceGroup.map(_.url)}")
  }

  def saveDownload(source: FeedSource)(download: Download): Future[(FeedSource, Long, MetaData)] = db.run {
    (downloads.returningId += download) map { downloadId =>
      (source, downloadId, download.metaData)
    }
  } tap { eventualSaved =>
    for ((source, id, metaData) <- eventualSaved)
      Logger.info(s"s..> Saved Download $id: ${source.url}")
  }

  def loadLatestMetaData(source: FeedSource): Future[Option[MetaData]] = db.run {
    downloads
      .filter(_.sourceId === source.id)
      .sortBy(_.timestamp.desc)
      .result
      .headOption
      .map(_.map(_.metaData))
  }

  def saveCachedFeed(cachedFeed: CachedFeed): Future[Option[Long]] = db.run {
    Logger.info("---> Saving feed: " + cachedFeed.source.url)
    feeds.filter(_.downloadId === cachedFeed.record.downloadId).delete.flatMap { numDeletedFeeds =>
      val proposedArticles = cachedFeed.articles.map(_.record)

      val existingArticles = for {
        s <- sources.active if (s.group.isEmpty && s.url === cachedFeed.source.url.toString) || (s.group.isDefined && s.group === cachedFeed.source.group)
        f <- feeds if f.sourceId === s.id && f.timestamp <= cachedFeed.record.metaData.timestamp
        a <- articles if a.feedId === f.id && proposedArticles.map(pa => a.link === pa.link.toString || a.title === pa.title).foldLeft(false.bind)(_ || _)
      } yield a

      existingArticles.result.flatMap { existingArticles =>
        val prunedArticles = proposedArticles
          .filterNot(pa => existingArticles.exists(xa => xa.link == pa.link ||
            (xa.title == pa.title && xa.imageSource == pa.imageSource && xa.text == pa.text)))

        if (prunedArticles isEmpty) {
          downloads.byId(Some(cachedFeed.record.downloadId)).delete map { numDeleted =>
            if (numDeleted > 0)
              Logger.info(s"..x> Deleted download ${cachedFeed.record.downloadId} with no new articles: ${cachedFeed.source.url}")
            None
          }
        } else {
          val groupPubDates = for {
            s <- sources.active if cachedFeed.source.group.fold(s.url.? === cachedFeed.source.url.toString)(group => s.group === group)
            f <- feeds if f.sourceId === s.id && f.timestamp <= cachedFeed.record.metaData.timestamp
            a <- articles if a.feedId === f.id
          } yield a.pubDate

          groupPubDates.sortBy(_.desc).take(10).result.flatMap { latestGroupPubDates =>
            cachedFeed.record.groupFrequency = Feed.frequency(latestGroupPubDates ++ prunedArticles.map(_.pubDate))

            (for {
              feedId <- feeds.returningId += cachedFeed.record
              numSavedArticles <- articles ++= prunedArticles.map(_.copy(feedId = Some(feedId)))
            } yield {
              (feedId, numSavedArticles)
            }) map { case (feedId, numSavedArticles) =>
              Logger.info(s"s..> Saved Feed $feedId and $numSavedArticles/${prunedArticles.size} Articles for Download ${cachedFeed.record.downloadId}: ${cachedFeed.source.url}")
              Some(feedId)
            }
          }
        }
      }
    } transactionally
  } recover {
    case t: Throwable =>
      Logger.info("Failed to save feed: " + cachedFeed.source.url, t)
      None
  }

  def deleteUnparsedDownload(source: FeedSource, downloadId: Long): Future[Boolean] = db.run {
    downloads.byId(Some(downloadId)).delete map { numDeleted =>
      if (numDeleted > 0) Logger.info(s"..x> Deleted unparseable download $downloadId: ${source.url}")
      numDeleted > 0
    }
  }

  def loadSources: Future[Seq[FeedSource]] = db.run {
    sources.active.result
  }
}
