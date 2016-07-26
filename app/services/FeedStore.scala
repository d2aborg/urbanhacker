package services

import java.time.{Duration, ZonedDateTime}

import com.google.inject.{Inject, Singleton}
import model.Utils._
import model._
import play.api.db.slick.DatabaseConfigProvider
import play.api.{Environment, Logger}
import services.SlickPgPostgresDriver.api._
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
          pow(ageSeconds, 4.0.bind) * f.groupFrequency
        } drop offset take limit

        articlePage.result.tap { r =>
          Logger.info("Read articles query:\n" + r.statements.mkString("\n"))
        }.map {
          _.map(CachedArticle.tupled)
        }.map { as =>
          val articlePage = as.take(searchPermalink.pageSize)
          val resolvedPermalink = Permalink(resolvedTimestamp, searchPermalink.pageNum)
          val nextPage = Some(searchPermalink.pageNum + 1).filter(_ => as.size > searchPermalink.pageSize)

          ArticlesResult(articlePage, Some(resolvedPermalink), permalink, nextPage)
        }
      }
    }
  }

  def loadUnparsedDownload(downloadId: Long): Future[Option[Download]] = db.run {
    downloads
      .filter(_.id === downloadId)
      .filterNot(feeds.byDownload(_).exists)
      .result.headOption
  }

  def deleteOutOfVersionFeeds(source: FeedSource, parseVersion: Int): Future[Int] = db.run {
    feeds.filter(_.sourceId === source.id).filter(_.parseVersion < parseVersion).delete
  } tap { eventualNum =>
    for (num <- eventualNum if num > 0)
      Logger.info(s"xxx> Deleted $num out of version feeds: ${source.url}")
  }

  def loadUnparsedDownloadIds(source: FeedSource): Future[Seq[Long]] = db.run {
    downloads
      .filter(_.sourceId === source.id)
      .filterNot(d => feeds.filter(f => f.sourceId === d.sourceId && f.timestamp === d.timestamp).exists)
      .sortBy(_.timestamp) // ascending for reparsing in old->new order
      .map(_.id.get).result
  } tap { eventualIds =>
    for (ids <- eventualIds if ids.nonEmpty)
      Logger.info(s"...> Loaded Ids of ${ids.size} Unparsed Downloads: ${source.url}")
  }

  def saveDownload(source: FeedSource)(download: Download): Future[Long] = db.run {
    downloads.returningId += download
  } tap { eventualId =>
    for (id <- eventualId)
      Logger.info(s"...> Saved Download $id: ${source.url}")
  }

  def loadLatestMetaData(source: FeedSource): Future[Option[MetaData]] = db.run {
    downloads
      .filter(_.sourceId === source.id)
      .sortBy(_.timestamp.desc)
      .result
      .headOption
      .map(_.map(_.metaData))
  }

  def saveCachedFeed(downloadId: Long)(cachedFeed: CachedFeed): Future[Option[Long]] = db.run {
    val proposedArticles = cachedFeed.articles.map(_.record)
    articles.filter(_.link inSet proposedArticles.map(_.link.toString)).result
      .map(_.map(_.link))
      .flatMap { existingArticlesByLink =>
        val linkPrunedArticles = proposedArticles.filterNot(a => existingArticlesByLink.contains(a.link))

        articles
          .filter(a => linkPrunedArticles
            .map(b => a.title === b.title && a.text === b.text)
            .foldLeft(false.bind)(_ || _))
          .result
          .map(_.map(a => (a.title, a.text)))
          .flatMap { existingArticlesByText =>
            val textPrunedArticles = linkPrunedArticles.filterNot(a => existingArticlesByText.contains((a.title, a.text)))

            if (textPrunedArticles isEmpty) {
              downloads.byId(Some(downloadId)).delete map { numDeleted =>
                if (numDeleted > 0)
                  Logger.info(s"xxx> Deleted download $downloadId with no new articles: ${cachedFeed.source.url}")
                None
              }
            } else {
              val groupFrequency = cachedFeed.source.group.fold(0.0.bind) { group =>
                (for {
                  source <- sources if source.id =!= cachedFeed.record.sourceId && source.group === group
                  feed <- feeds if feed.sourceId === source.id && feed.timestamp === (
                  for (fb <- feeds if fb.sourceId === feed.sourceId && fb.timestamp <= cachedFeed.record.metaData.timestamp)
                    yield fb.timestamp).max
                } yield feed.frequency).sum.getOrElse(0.0)
              }
              groupFrequency.result.flatMap { groupFrequency =>
                cachedFeed.record.groupFrequency = cachedFeed.record.frequency + groupFrequency

                val savedIds = for {
                  feedId <- feeds.returningId += cachedFeed.record
                  articleIds <- articles.returningId ++= textPrunedArticles.map(_.copy(feedId = Some(feedId)))
                } yield {
                  (feedId, articleIds)
                }
                savedIds map { case (feedId, articleIds) =>
                  Logger.info(s"fff> Saved Feed $feedId and ${articleIds.size} Articles for Download $downloadId: ${cachedFeed.source.url}")
                  Some(feedId)
                }
              }
            }
          }
      } transactionally
  }

  def deleteUnparsedDownload(source: FeedSource, downloadId: Long): Future[Boolean] = db.run {
    downloads.byId(Some(downloadId)).delete map { numDeleted =>
      if (numDeleted > 0) Logger.info(s"xxx> Deleted unparseable download $downloadId: ${source.url}")
      numDeleted > 0
    }
  }

  def loadSources: Future[Seq[FeedSource]] = db.run {
    sources.result
  }
}
