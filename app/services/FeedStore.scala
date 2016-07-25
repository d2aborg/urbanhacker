package services

import java.time.{Duration, ZoneOffset, ZonedDateTime}

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
  val dbConfig = dbConfigProvider.get[JdbcProfile]
  val db = dbConfig.db

  Logger.info("Schemas:\n" +
    (downloads.schema ++ sources.schema ++ feeds.schema ++ articles.schema).create.statements.mkString("\n"))

  val extractEpoch = SimpleExpression.unary[Duration, Long] { (zdt, qb) =>
    qb.sqlBuilder += "EXTRACT(EPOCH FROM "
    qb.expr(zdt)
    qb.sqlBuilder += ")"
  }

  val div = SimpleExpression.binary[Long, Long, Long] { (num, den, qb) =>
    qb.sqlBuilder += "("
    qb.expr(num)
    qb.sqlBuilder += "/"
    qb.expr(den)
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
                           (implicit now: ZonedDateTime): Future[ArticlesResult] =
    db.run {
      val searchPermalink = permalink getOrElse Permalink(now, 1)
      feeds.historic(section, searchPermalink.timestamp).map(_.timestamp).max.result.flatMap {
        _.fold {
          DBIO.successful(ArticlesResult(Seq.empty, None, permalink, None)): DBIOAction[ArticlesResult, NoStream, Read]
        } { resolvedTimestamp =>
          val articlesWithSourceAndFeed = for {
            s <- sources.bySection(section)
            f <- feeds if f.sourceId === s.id && f.timestamp <= resolvedTimestamp
            a <- articles if a.feedId === f.id && !articles.newerThan(section, resolvedTimestamp, a.link, a.pubDate, f.timestamp).exists
          } yield (s, f, a)

          val offset = (searchPermalink.pageNum - 1) * searchPermalink.pageSize
          val limit = searchPermalink.pageSize + 1 // one extra for next page detection

          val articlePage = articlesWithSourceAndFeed sortBy { case (s, f, a) =>
            val ageMillis = extractEpoch(resolvedTimestamp.bind - a.pubDate)
            val ageSeconds = ageMillis.asColumnOf[Double] / 1000.0
            val frequency = f.frequency
            pow(ageSeconds, 4.0.bind) * frequency
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

  def loadUnparsedDownload(downloadId: Long): Future[Option[Download]] =
    db.run {
      downloads
        .filter(_.id === downloadId)
        .filterNot(feeds.byDownload(_).exists)
        .result.headOption
    }

  def loadUnparsedDownloadIds(source: FeedSource): Future[Seq[Long]] =
    db.run {
      downloads
        .filter(_.sourceId === source.id)
        .filterNot(d => feeds.filter(f => f.sourceId === d.sourceId && f.timestamp === d.timestamp).exists)
        .sortBy(_.timestamp)
        .map(_.id.get).result
    } tap { eventualIds =>
      for (ids <- eventualIds if ids.nonEmpty)
        Logger.info(s"'''> Loaded Ids of ${ids.size} Unparsed Downloads: ${source.url}")
    }

  def saveDownload(source: FeedSource)(download: Download): Future[Long] =
    db.run {
      downloads.returningId += download
    } tap { eventualId =>
      for (id <- eventualId)
        Logger.info("...> Saved Download " + id + ": " + source.url)
    }

  def loadLatestMetaData(source: FeedSource): Future[Option[MetaData]] =
    db.run {
      downloads
        .filter(_.sourceId === source.id)
        .sortBy(_.timestamp.desc)
        .result
        .headOption
        .map(_.map(_.metaData))
    }

  def saveCachedFeed(downloadId: Long)(cachedFeed: CachedFeed): Future[Option[Long]] =
    db.run {
      val proposedArticles = cachedFeed.articles.map(_.record)
      articles.filter(_.link inSet proposedArticles.map(_.link.toString)).result
        .map(_.map(a => (a.link.toString, a.pubDate.withZoneSameInstant(ZoneOffset.UTC))))
        .flatMap { existingArticlesByLink =>
          val linkPrunedArticles = proposedArticles
            .filterNot(a => existingArticlesByLink.contains((a.link.toString, a.pubDate)))

          articles.filter(a => linkPrunedArticles.map(b => a.title === b.title && a.text === b.text && a.pubDate === b.pubDate).foldLeft(false.bind)(_ || _)).result
            .map(_.map(a => (a.title, a.text, a.pubDate.withZoneSameInstant(ZoneOffset.UTC))))
            .flatMap { existingArticlesByText =>
              val textPrunedArticles = linkPrunedArticles
                .filterNot(a => existingArticlesByText.contains((a.title, a.text, a.pubDate)))

              if (textPrunedArticles isEmpty) {
                downloads.byId(downloadId).delete map { deletedDownloadId =>
                  Logger.info(s"...> Deleted download $deletedDownloadId with no new articles" + cachedFeed.source.url)
                  None
                }
              } else {
                val savedIds = for {
                  feedId <- feeds.returningId += cachedFeed.record
                  articleIds <- articles.returningId ++= textPrunedArticles.map(_.copy(feedId = Some(feedId)))
                } yield {
                  (feedId, articleIds)
                }
                savedIds map { case (feedId, articleIds) =>
                  Logger.info(s"...> Saved Feed $feedId and ${articleIds.size} Articles: " + cachedFeed.source.url)
                  Some(feedId)
                }
              }
            }
        } transactionally
    }

  def loadSources: Future[Seq[FeedSource]] =
    db.run {
      sources.result
    }
}
