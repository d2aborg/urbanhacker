package services

import java.time.{Duration, ZonedDateTime}

import com.google.inject.{Inject, Singleton}
import model.Utils._
import model._
import play.api.db.slick.DatabaseConfigProvider
import play.api.{Environment, Logger, Mode}
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

  def resolvePermalink(section: String, permalink: Option[Permalink])(implicit now: ZonedDateTime): Future[ResolvedPermalink] = {
    val pageNum = permalink.map(_.pageNum) getOrElse 1
    val timestamp = permalink.map(_.timestamp) getOrElse now

    db.run {
      feeds.historic(section, timestamp).map(_.timestamp).max.result map { (historicTimestamp: Option[ZonedDateTime]) =>
        ResolvedPermalink(historicTimestamp.map(Permalink(_, pageNum)), permalink)
      }
    }
  }

  def articlePageByFrecency(section: String, permalink: Option[Permalink])
                           (implicit now: ZonedDateTime): Future[ArticlesResult] =
    db.run {
      val pageNum = permalink.map(_.pageNum) getOrElse 1
      val timestamp = permalink.map(_.timestamp) getOrElse now
      val pageSize = Permalink.pageSize

      feeds.historic(section, timestamp).map(_.timestamp).max.result.flatMap {
        _.fold {
          DBIO.successful[ArticlesResult](ArticlesResult(Seq.empty, ResolvedPermalink(None, permalink), None)): DBIOAction[ArticlesResult, NoStream, Read]
        } { historicTimestamp =>
          val articlesWithSourceAndFeed = for {
            s <- sources.bySection(section)
            f <- feeds if f.sourceId === s.id && f.timestamp <= historicTimestamp
            a <- articles if a.feedId === f.id && !articles.newerThan(section, historicTimestamp, a.link, a.pubDate, f.timestamp).exists
          } yield (s, f, a)

          val offset = (pageNum - 1) * pageSize
          val limit = pageSize + 1 // one extra for next page detection

          val articlePage = articlesWithSourceAndFeed sortBy { case (s, f, a) =>
            val ageMillis = extractEpoch(historicTimestamp.bind - a.pubDate)
            val ageSeconds = ageMillis.asColumnOf[Double] / 1000.0
            val frequency = f.frequency
            pow(ageSeconds, 4.0.bind) * frequency
          } drop offset take limit

          articlePage.result.tap { r =>
            Logger.info("Read articles query:\n" + r.statements.mkString("\n"))
          }.map {
            _.map(CachedArticle.tupled)
          }.map { as =>
            val articlePage = as.take(pageSize)
            val hasMore = as.size > pageSize

            val nextPage = Some(pageNum + 1).filter(_ => hasMore)

            val resolvedPermalink = ResolvedPermalink(Some(Permalink(historicTimestamp, pageNum)), permalink)

            ArticlesResult(articlePage, resolvedPermalink, nextPage)
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

  def loadUnparsedDownloadIds(source: FeedSource): Future[Seq[Long]] = {
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
  }

  def saveDownload(source: FeedSource)(download: Download): Future[Long] =
    db.run {
      downloads.returningId += download
    } tap { eventualId =>
      for (id <- eventualId)
        Logger.info("...> Saved Download " + id + ": " + source.url)
    }

  def loadLatestMetaData(source: FeedSource): Future[Option[MetaData]] = {
    db.run {
      downloads
        .filter(_.sourceId === source.id)
        .sortBy(_.timestamp.desc)
        .result
        .headOption
        .map(_.map(_.metaData))
    }
  }

  def saveCachedFeed(cachedFeed: CachedFeed): Future[Long] = {
    db.run {
      (for {
        feedId <- feeds.returningId += cachedFeed.record
        articleIds <- articles.returningId ++= cachedFeed.articles.map(_.record.copy(feedId = Some(feedId)))
      } yield {
        (feedId, articleIds)
      }).transactionally
    } tap { eventuallyInserted =>
      eventuallyInserted onSuccess { case (feedId, articleIds) =>
        Logger.info(s"...> Saved Feed $feedId and ${articleIds.size} Articles: " + cachedFeed.source.url)
      }
      eventuallyInserted onFailure { case t =>
        Logger.warn("Failed to insert cached feed: " + cachedFeed.source.url +
          " at " + cachedFeed.record.metaData.timestamp, t)
      }
    } map (_._1)
  }

  def loadSources: Future[Seq[FeedSource]] = {
    val query = if (env.mode == Mode.Prod) sources else sources.take(1000)
    db.run {
      query.result
    }
  }
}
