package services

import java.time.{Duration, ZonedDateTime}

import com.google.inject.{Inject, Singleton}
import model.Article.nonSimilar
import model.Feed.frequency
import model.Utils._
import model._
import play.api.db.slick.DatabaseConfigProvider
import play.api.{Environment, Logger}
import services.SlickPgPostgresDriver.api._
import slick.dbio.DBIOAction
import slick.dbio.Effect.Read
import slick.jdbc.JdbcProfile

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

  def loadDownload(source: FeedSource, downloadId: Long): Future[Option[Download]] = db.run {
    Logger.debug(s"${source.url}: Loading download: " + downloadId)
    downloads.byId(Some(downloadId)).result.headOption
  }

  def loadIncompleteDownloadIds(sourceGroup: Seq[FeedSource], parseVersion: Int): Future[Seq[(FeedSource, Long)]] = db.run {
    (for {
      source <- sources if source.id inSet sourceGroup.map(_.id)
      download <- downloads if download.sourceId === source.id && !feeds.filter(f => f.downloadId === download.id && f.parseVersion >= parseVersion).exists
    } yield (source, download)).sortBy(_._2.timestamp).map(sd => (sd._1, sd._2.id.get)).result
  } tap { eventualDownloadIdsBySource =>
    for (downloadIdsBySource <- eventualDownloadIdsBySource if downloadIdsBySource.nonEmpty)
      Logger.info(s"Loaded Ids of ${downloadIdsBySource.size} Unparsed or Out of Version Downloads: ${sourceGroup.map(_.url)}")
  }

  def saveDownload(source: FeedSource)(download: Download): Future[(FeedSource, Long, MetaData)] = db.run {
    (downloads.returningId += download) map { downloadId =>
      (source, downloadId, download.metaData)
    }
  } tap { eventualSaved =>
    for ((source, id, metaData) <- eventualSaved)
      Logger.info(s"${source.url}: Saved Download $id")
  }

  def loadLatestMetaData(source: FeedSource): Future[Option[MetaData]] = db.run {
    downloads
      .filter(_.sourceId === source.id)
      .sortBy(_.timestamp.desc)
      .result
      .headOption
      .map(_.map(_.metaData))
  }

  def saveCachedFeed(cachedFeed: CachedFeed): Future[Option[Long]] =
    db.run {
      feeds.filter(_.downloadId === cachedFeed.record.downloadId).delete.flatMap { _ =>
        val allArticles = nonSimilar(cachedFeed.articles.map(_.record).toList)
        DBIO.sequence(articles.similar(cachedFeed, allArticles).map(_.result)).map(_.flatten.toSet).flatMap { similarArticles =>
          val newOrUpdatedArticles = allArticles.filterNot(a => similarArticles.exists(a.same))
          val updatedArticles = similarArticles.filterNot(a => allArticles.exists(a.same))

          if (newOrUpdatedArticles isEmpty) {
            downloads.byCachedFeed(cachedFeed).delete map { numDeleted =>
              if (numDeleted > 0)
                Logger.debug(s"${cachedFeed.source.url}: Deleted download ${cachedFeed.record.downloadId} with no new articles")

              None
            }
          } else {
            articles.byIds(updatedArticles.flatMap(_.id)).delete.flatMap { numDeletedSimilarArticles =>
              if (numDeletedSimilarArticles > 0)
                Logger.debug(s"${cachedFeed.source.url}: Deleted $numDeletedSimilarArticles similar articles")

              Logger.debug(s"${cachedFeed.source.url}: Saving feed for Download ${cachedFeed.record.downloadId}")

              Logger.debug(s"${cachedFeed.source.url}: ${allArticles.size} articles: ${allArticles.map(_.title)}")
              Logger.debug(s"${cachedFeed.source.url}: ${similarArticles.size} similar articles: ${similarArticles.map(_.title)}")
              Logger.debug(s"${cachedFeed.source.url}: ${newOrUpdatedArticles.size} new or updated articles: ${newOrUpdatedArticles.map(_.title)}")

              articles.lastTenInGroup(cachedFeed).map(_.pubDate).result.flatMap { topTenGroupPubDates =>
                cachedFeed.record.groupFrequency = frequency(topTenGroupPubDates ++ newOrUpdatedArticles.map(_.pubDate))

                for {
                  feedId <- feeds.returningId += cachedFeed.record
                  articleIds <- articles.returningId ++= newOrUpdatedArticles.map(_.copy(feedId = Some(feedId)))
                } yield {
                  Logger.info(s"${cachedFeed.source.url}: " +
                    s"Saved Feed $feedId and ${articleIds.size}/${newOrUpdatedArticles.size} Articles for " +
                    s"Download ${cachedFeed.record.downloadId}")

                  Some(feedId)
                }
              }
            }
          }
        }
      } transactionally
    } recover {
      case t: Throwable =>
        Logger.warn(s"${cachedFeed.source.url}: Failed to save feed for Download ${cachedFeed.record.downloadId}", t)
        None
    }

  def deleteUnparsedDownload(source: FeedSource, downloadId: Long): Future[Boolean] = db.run {
    downloads.byId(Some(downloadId)).delete map { numDeleted =>
      if (numDeleted > 0) Logger.warn(s"${source.url}: Deleted unparseable download $downloadId")
      numDeleted > 0
    }
  }

  def loadSources: Future[Seq[FeedSource]] = db.run {
    sources.active.result
  }
}
