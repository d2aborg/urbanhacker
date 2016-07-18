package services

import java.sql.Timestamp
import java.time.temporal.ChronoUnit
import java.time.{Duration, OffsetDateTime, ZonedDateTime}

import com.google.inject.Inject
import model.Utils._
import model._
import play.api.db.slick.DatabaseConfigProvider
import play.api.{Environment, Logger, Mode}
import play.libs.Scala
import services.MyPostgresDriver.api._
import slick.dbio.DBIOAction
import slick.dbio.Effect.Read
import slick.driver.JdbcProfile
import slick.lifted.AppliedCompiledFunction
import slick.profile.{FixedSqlAction, FixedSqlStreamingAction, SqlStreamingAction}

import scala.collection.SortedSet
import scala.concurrent.{ExecutionContext, Future}

class FeedStore @Inject()(dbConfigProvider: DatabaseConfigProvider, env: Environment)(implicit exec: ExecutionContext) {
  val dbConfig = dbConfigProvider.get[JdbcProfile]
  val db = dbConfig.db

  Logger.info("Schemas:\n" +
    (downloads.schema ++ sources.schema ++ feeds.schema ++ articles.schema).create.statements.mkString("\n"))

  def articlePageByFrecency(section: String, permalink: Option[Permalink])
                           (implicit now: ZonedDateTime): Future[Option[(Seq[CachedArticle], Permalink)]] = {
    val pageNum = permalink.map(_.page) getOrElse 1
    val timestamp = permalink.map(_.timestamp) getOrElse now

    articlesByFrecency(section, timestamp, pageNum, Permalink.pageSize).map {
      _.map {
        case (articlePage, latestTimestamp, hasMore) =>
          val nextPage = Some(pageNum + 1).filter(_ => hasMore)

          val resolvedPermalink = Permalink(latestTimestamp, pageNum, nextPage, permalink.map(_.timestamp))

          (articlePage, resolvedPermalink)
      }
    }
  }

  val extractEpoch = SimpleExpression.unary[ZonedDateTime, Long] { (zdt, qb) =>
    qb.sqlBuilder += "EXTRACT(EPOCH FROM "
    qb.expr(zdt)
    qb.sqlBuilder += ")"
  }

  def articlesByFrecency(section: String, timestamp: ZonedDateTime, pageNum: Int, pageSize: Int): Future[Option[(Seq[CachedArticle], ZonedDateTime, Boolean)]] = {
    db.run {
      feeds.historic(section, timestamp).map(_.timestamp).max.result.flatMap { historicTimestamp =>
        DBIOA.sequence {
          historicTimestamp.map { historicTimestamp =>
            val articlesWithSourceAndFeed = for {
              s <- sources.bySection(section)
              f <- feeds if f.sourceId === s.id && f.timestamp <= timestamp
              a <- articles.sortBy(_.pubDate.desc) if a.feedId === f.id && !articles.newerThan(section, historicTimestamp, a.link, a.pubDate, f.timestamp).exists
            } yield (s, f, a)

            val offset = (pageNum - 1) * pageSize
            val limit = pageSize + 1 // one extra for next page detection
            val articlePage = articlesWithSourceAndFeed.drop(offset).take(limit)

            articlePage.result.map {
              _.map(CachedArticle.tupled(_))
            }.map { as =>
              (as.take(pageSize), historicTimestamp, as.size > pageSize)
            }
          }
        }
      }
    }
  }

  def byFrecency(articles: SortedSet[CachedArticle], timestamp: OffsetDateTime): Seq[(Double, CachedArticle)] = {
    val groupFrequency = frequency(articles, timestamp)
    articles.toSeq.map(article => (frecency(article.record, groupFrequency, timestamp), article))
  }

  def frecency(article: Article, frequency: Double, timestamp: OffsetDateTime): Double =
    Math.pow(age(article, timestamp), 4) * frequency

  def age(article: Article, timestamp: OffsetDateTime): Long =
    ChronoUnit.SECONDS.between(article.pubDate, timestamp)

  def frequency(articles: SortedSet[CachedArticle], timestamp: OffsetDateTime): Double = {
    val mostRecent = articles take 10
    val ranges = (timestamp +: mostRecent.map(_.record.pubDate).toSeq).sliding(2)
    val periods = ranges.map(range => ChronoUnit.SECONDS.between(range.last, range.head))
    val weights = (1 to mostRecent.size).map(1.0 / _)
    val weightedPeriods = for ((period, weight) <- periods.zip(weights.iterator)) yield period * weight
    val weightedAveragePeriod = weightedPeriods.sum / weights.sum
    1.0 / weightedAveragePeriod
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
