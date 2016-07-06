package services

import java.io.StringReader
import java.net.HttpURLConnection
import java.security.cert.X509Certificate
import java.security.{MessageDigest, SecureRandom}
import java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME
import java.time.temporal.ChronoUnit
import java.time.{OffsetDateTime, ZoneOffset}
import javax.net.ssl._

import akka.actor.ActorSystem
import com.google.inject.{Inject, Singleton}
import model.Utils._
import model._
import play.api.inject.ApplicationLifecycle
import play.api.{Configuration, Logger}

import scala.collection.{SortedMap, SortedSet, mutable}
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source
import scala.xml.XML

@Singleton
class FeedCache @Inject()(feedStore: FeedStore, configuration: Configuration, actorSystem: ActorSystem, lifecycle: ApplicationLifecycle)(implicit exec: ExecutionContext) {
  val reloadTask = actorSystem.scheduler.schedule(5 minutes, 30 minutes) {
    reload()
  }
  lifecycle.addStopHook { () =>
    Logger.info("Cancelling reload task...")
    Future.successful(reloadTask.cancel)
  }

  // configure the SSLContext with a permissive TrustManager
  val ctx = SSLContext.getInstance("TLS")
  ctx.init(Array[KeyManager](), Array[TrustManager](new X509TrustManager {
    override def checkClientTrusted(arg0: Array[X509Certificate], arg1: String): Unit = ()

    override def checkServerTrusted(arg0: Array[X509Certificate], arg1: String): Unit = ()

    override def getAcceptedIssuers: Array[X509Certificate] = null
  }), new SecureRandom())
  SSLContext.setDefault(ctx)

  val sections: Set[String] = configuration.getConfig("sections").getOrElse(Configuration.empty).subKeys
  val cache: mutable.Map[FeedSource, CachedFeed] = mutable.Map.empty
  prime()

  case class CachedFeed(source: FeedSource, feed: Feed, previous: Option[CachedFeed]) {
    def articles: Seq[Article] =
      feed.articles ++ previous.map(_.articles).getOrElse(Nil)

    def historic(timestamp: OffsetDateTime): Option[CachedFeed] = feed.metaData.timestamp match {
      case myTimestamp if myTimestamp.isAfter(timestamp) => previous.flatMap(_.historic(timestamp))
      case _ => Some(this)
    }
  }

  def frequency(articles: SortedSet[Article], timestamp: OffsetDateTime): Double = {
    val mostRecent = articles take 10
    val ranges = (timestamp +: mostRecent.map(_.date).toSeq).sliding(2)
    val periods = ranges.map(range => ChronoUnit.SECONDS.between(range.last, range.head))
    val weights = (1 to mostRecent.size).map(1.0 / _)
    val weightedPeriods = for ((period, weight) <- periods.zip(weights.iterator)) yield period * weight
    val weightedAveragePeriod = weightedPeriods.sum / weights.sum
    1.0 / weightedAveragePeriod
  }

  def apply(section: String, permalink: Option[Permalink])(implicit now: OffsetDateTime): (Iterable[Article], Permalink) = {
    val (allArticles, latestTimestamp) = articles(section, permalink.map(_.timestamp) getOrElse now)
    val pageNum = permalink.map(_.page) getOrElse 1

    val (articlePage, hasMore) = paged(allArticles, pageNum, Permalink.pageSize)
    val nextPage = Some(pageNum + 1).filter(_ => hasMore)

    val resolvedPermalink = Permalink(latestTimestamp, pageNum, nextPage, permalink.map(_.timestamp))

    (articlePage, resolvedPermalink)
  }

  def paged(all: Iterable[Article], page: Int, pageSize: Int): (Iterable[Article], Boolean) = {
    val from = (page - 1) * pageSize
    val until = page * pageSize
    (all.slice(from, until), all.size > until)
  }

  def articles(section: String, timestamp: OffsetDateTime): (Iterable[Article], OffsetDateTime) = {
    val historicCachedFeeds = cache synchronized {
      cache.filterKeys(_.section == section).values
    }.flatMap {
      _.historic(timestamp)
    }

    val historicTimestamp = historicCachedFeeds.map(_.feed.metaData.timestamp)
      .reduceOption(implicitly[Ordering[OffsetDateTime]].max) getOrElse timestamp

    val articlesByFrecency = SortedMap[Double, Article]() ++ historicCachedFeeds.groupBy(_.source.group).mapValues {
      case cachedFeeds => cachedFeeds.flatMap(_.articles).groupBy(_.link).values.map(_.max).to[SortedSet]
    }.flatMap {
      case (_, articles) if articles nonEmpty => byFrecency(articles, historicTimestamp)
      case _ => Nil
    }

    (articlesByFrecency.values, historicTimestamp)
  }

  def byFrecency(articles: SortedSet[Article], timestamp: OffsetDateTime): Seq[(Double, Article)] = {
    val groupFrequency = frequency(articles, timestamp)
    articles.toSeq.map(article => (article.frecency(groupFrequency, timestamp), article))
  }

  def latest(source: FeedSource): Option[CachedFeed] = cache synchronized {
    cache get source
  }

  def prime(): Unit = {
    Logger.info("Initiating Priming...")

    for ((successful, total) <- update(loadOrDownload(_)))
      Logger.info("###> Primed " + successful + "/" + total)

    Logger.info("Priming initiated")
  }

  def reload(): Unit = {
    Logger.info("Initiating Reload...")

    for ((successful, total) <- update(download(_)))
      Logger.info("###> Reloaded " + successful + "/" + total)

    Logger.info("Reload initiated")
  }

  def update(loadEventualMaybeFeedsBySource: FeedSource => Future[Option[CachedFeed]]): Future[(Int, Int)] = {
    val eventualMaybeCachedFeeds = loadSources map loadEventualMaybeFeedsBySource

    for {
      eventualMaybeCachedFeed <- eventualMaybeCachedFeeds
      maybeCachedFeed <- eventualMaybeCachedFeed
      cachedFeed <- maybeCachedFeed
    } cache synchronized {
      cache(cachedFeed.source) = cachedFeed
      Logger.info("---> Cached: " + cachedFeed.source.url)
    }

    for (maybeCachedFeeds <- Future sequence eventualMaybeCachedFeeds)
      yield (maybeCachedFeeds.flatten.size, maybeCachedFeeds.size)
  }

  def loadOrDownload(source: FeedSource): Future[Option[CachedFeed]] =
    load(source) flatMap {
      case maybeCachedFeed: Some[CachedFeed] => Future.successful(maybeCachedFeed)
      case None => download(source)
    }

  def load(source: FeedSource): Future[Option[CachedFeed]] =
    feedStore.load(source.url).flatMap { downloads =>
      Future.sequence(downloads.map(parse(source, _))).map {
        Logger.info("Loaded: " + source.url)
        _.flatten.foldLeft(None: Option[CachedFeed]) { (previous, feed) =>
          Some(CachedFeed(source, feed, previous))
        }
      }
    }

  def download(source: FeedSource): Future[Option[CachedFeed]] =
    download(source, latest(source))

  def download(source: FeedSource, previous: Option[CachedFeed]): Future[Option[CachedFeed]] = {
    val timestamp = OffsetDateTime.now(ZoneOffset.UTC)

    try {
      val connection = source.url.toURL.openConnection.asInstanceOf[HttpURLConnection]
      connection match {
        case httpsConnection: HttpsURLConnection =>
          httpsConnection.setHostnameVerifier(new HostnameVerifier() {
            override def verify(arg0: String, arg1: SSLSession): Boolean = true
          })
        case _ =>
      }
      connection setRequestProperty("User-Agent", "UrbanHacker/0.1")

      previous map (_.feed.metaData) flatMap (_.eTag) foreach { eTag =>
        connection setRequestProperty("If-None-Match", eTag)
      }

      previous map (_.feed.metaData) flatMap (_.lastModified) orElse {
        previous map (_.feed.metaData.timestamp.format(RFC_1123_DATE_TIME))
      } foreach { lastModified =>
        connection setRequestProperty("If-Modified-Since", lastModified)
      }

      val eventualMaybeContent = Future {
        try {
          connection connect()
          if (connection.getResponseCode == 304)
            None
          else
            Some(Source.fromInputStream(connection.getInputStream, Option(connection.getContentEncoding).getOrElse("UTF-8")).mkString)
        } finally connection disconnect()
      }

      eventualMaybeContent.flatMap {
        case Some(content) =>
          Logger.info("Downloaded: " + source.url)

          val metaData = MetaData(source.url,
            Option(connection getHeaderField "Last-Modified"),
            Option(connection getHeaderField "ETag"),
            md5(content), timestamp)

          val download = TextDownload(0, metaData, content)

          parse(source, download).flatMap {
            case Some(feed) => save(source, CachedFeed(source, feed, previous), download)
            case _ => Future.successful(None)
          }
        case _ => Future.successful(None)
      }
    } catch {
      case e: Exception =>
        Logger.warn("Failed to download: " + source.url, e)
        Future.failed(e)
    }
  }

  def md5(data: String): String =
    MessageDigest.getInstance("MD5").digest(data.getBytes("UTF-8")).map("%02x".format(_)).mkString

  def parse(source: FeedSource, textDownload: TextDownload): Future[Option[Feed]] = Future {
    val xml = XML.load(new StringReader(textDownload.content))
    val xmlDownload = XmlDownload(textDownload.metaData, xml)
    Feed.parse(source, xmlDownload)
  }

  def save(source: FeedSource, cachedFeed: CachedFeed, download: TextDownload): Future[Option[CachedFeed]] = {
    if (cachedFeed.previous.map(_.feed.metaData.checksum).contains(cachedFeed.feed.metaData.checksum))
      return Future.successful(None)

    if (cachedFeed.previous.map(_.feed.articles).contains(cachedFeed.feed.articles))
      return Future.successful(None)

    feedStore.save(download).map(if (_) Some(cachedFeed) else None)
  }

  def loadSources: Set[FeedSource] =
    for {
      section <- sections
      line <- Source.fromFile(s"feeds.$section.txt").getLines.map(_.trim) if !line.startsWith("#") && !line.isEmpty
      source <- parseSource(section, line)
    } yield source


  def parseSource(section: String, line: String): Option[FeedSource] = line.split("\\|") match {
    case Array(feedUrl) => parseURI(feedUrl).map(FeedSource(section, _, feedUrl))
    case Array(feedUrl, group) => parseURI(feedUrl).map(FeedSource(section, _, nonEmpty(group, feedUrl)))
    case Array(feedUrl, group, siteUrl) => parseURI(feedUrl).map(FeedSource(section, _, nonEmpty(group, feedUrl), parseURI(siteUrl)))
    case Array(feedUrl, group, siteUrl, title) => parseURI(feedUrl).map(FeedSource(section, _, nonEmpty(group, feedUrl), parseURI(siteUrl), nonEmpty(title)))
  }
}
