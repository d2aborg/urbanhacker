package services

import java.io.StringReader
import java.net.{HttpURLConnection, URI}
import java.security.cert.X509Certificate
import java.security.{MessageDigest, SecureRandom}
import java.time.{OffsetDateTime, ZoneOffset}
import java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME
import java.time.temporal.ChronoUnit
import javax.net.ssl._

import akka.actor.ActorSystem
import com.google.inject.{Inject, Singleton}
import model.Utils._
import model._
import play.api.{Configuration, Logger}
import play.api.inject.ApplicationLifecycle

import scala.collection.{SortedSet, mutable}
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.io.Source
import scala.xml.XML

@Singleton
class FeedCache @Inject()(feedStore: FeedStore, configuration: Configuration, actorSystem: ActorSystem, lifecycle: ApplicationLifecycle)(implicit exec: ExecutionContext) {
  val reloadTask = actorSystem.scheduler.schedule(15 seconds, 30 minutes) {
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

  val sections: Seq[String] = configuration.getConfig("sections").getOrElse(Configuration.empty).subKeys.toSeq
  val cache: mutable.Map[String, mutable.Map[URI, Feed]] = mutable.Map(sections.map { section =>
    (section, mutable.Map(loadChains(section).map(_.byUrl): _*))
  }: _*)

  def frequency(articles: SortedSet[Article], timestamp: OffsetDateTime): Double = {
    val mostRecent = articles take 10
    val ranges = (timestamp +: mostRecent.map(_.date).toSeq).sliding(2)
    val periods = ranges.map(range => ChronoUnit.SECONDS.between(range.last, range.head))
    val weights = (1 to mostRecent.size).map(1.0 / _)
    val weightedPeriods = for ((period, weight) <- periods.zip(weights.iterator)) yield period * weight
    val weightedAveragePeriod = weightedPeriods.sum / weights.sum
    1.0 / weightedAveragePeriod
  }

  def apply(section: String, timestamp: OffsetDateTime, pageNum: Int, pageSize: Int): (Seq[Article], OffsetDateTime, Option[Int]) = {
    val (allArticles, latestTimestamp) = articles(section, timestamp)
    val (page, hasMore) = paged(allArticles, pageNum, pageSize)
    val nextPage = if (hasMore) Some(pageNum + 1) else None
    (page, latestTimestamp, nextPage)
  }

  def paged(all: Seq[Article], pageNum: Int, pageSize: Int): (Seq[Article], Boolean) = {
    val from = (pageNum - 1) * pageSize
    val until = pageNum * pageSize
    (all.slice(from, until), all.size > until)
  }

  def articles(section: String, timestamp: OffsetDateTime): (Seq[Article], OffsetDateTime) = {
    val feeds = cache synchronized {
      cache(section) values
    }.flatMap {
      _.latestAt(timestamp)
    }

    val articles = feeds.groupBy(_.source.group).values.map { feedGroup =>
      Article.uniqueSorted(feedGroup.flatMap(_.articles))
    }.filter {
      _.nonEmpty
    }.flatMap { articleGroup =>
      val groupFrequency = frequency(articleGroup, timestamp)
      articleGroup.toSeq.map(article => (article.frecency(groupFrequency, timestamp), article))
    }.toSeq.sorted(Ordering.by[(Double, Article), Double](_._1)).map {
      _._2
    }

    val latestTimestamp = feeds.map(_.metaData.timestamp).max

    (articles, latestTimestamp)
  }

  def latest(section: String, source: FeedSource): Option[Feed] = cache synchronized {
    cache(section) get source.url
  }

  def reload(): Unit = {
    Logger.info("Reloading...")

    val feedBatches = for (section <- sections) yield (section, downloadAll(section))

    for ((section, eventualAllParsed) <- feedBatches; eventualParsed <- eventualAllParsed) {
      eventualParsed.onSuccess {
        case Some(feed) =>
          cache synchronized {
            cache(section)(feed.url) = feed
            Logger.info("Updated: " + feed.source.url)
          }
      }
    }

    val eventuallyDownloaded = feedBatches flatMap (_._2)
    val downloaded = Await.result(Future sequence eventuallyDownloaded, 10 minutes).flatten
    Logger.info("Updated " + downloaded.size + "/" + eventuallyDownloaded.size)
  }

  def downloadAll(section: String): Seq[Future[Option[Feed]]] =
    for (source <- loadSources(section))
      yield download(section, source)

  def download(section: String, source: FeedSource): Future[Option[Feed]] =
    download(source, latest(section, source))

  def save(feed: Feed, download: TextDownload): Future[Option[Feed]] = {
    if (feed.previous.map(_.metaData).map(_.checksum).contains(feed.metaData.checksum))
      return Future(None)

    if (feed.previous.map(_.articles).contains(feed.articles))
      return Future(None)

    feedStore.save(download).map(if (_) Some(feed) else None)
  }

  def loadChains(section: String): Seq[Feed] = {
    Await.result(Future.traverse {
      loadSources(section)
    } { source =>
      loadChainOrDownload(source)
    }, 10 minutes) flatten
  }

  def loadChainOrDownload(source: FeedSource): Future[Option[Feed]] =
    loadChain(source) flatMap { f =>
      if (f isEmpty) download(source)
      else Future(f)
    }

  def loadChain(source: FeedSource): Future[Option[Feed]] = {
    feedStore.load(source.url).map { downloads =>
      val parsed = downloads.foldLeft(None: Option[Feed]) { (previous, download) =>
        parse(source, download, previous)
      }
      Logger.info("Parsed: " + parsed.map(_.copy(articles = SortedSet())))
      parsed
    }
  }

  def download(source: FeedSource, previous: Option[Feed] = None): Future[Option[Feed]] = {
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

      previous map (_.metaData) flatMap (_.eTag) foreach { eTag =>
        connection setRequestProperty("If-None-Match", eTag)
      }

      previous map (_.metaData) flatMap (_.lastModified) orElse {
        previous map (_.metaData.timestamp.format(RFC_1123_DATE_TIME))
      } foreach { lastModified =>
        connection setRequestProperty("If-Modified-Since", lastModified)
      }

      val content = try {
        connection connect()
        if (connection.getResponseCode == 304)
          return Future(None)

        Source.fromInputStream(connection.getInputStream, Option(connection.getContentEncoding).getOrElse("UTF-8")).mkString
      } finally {
        connection disconnect()
      }

      Logger.info("Downloaded: " + source.url)

      val metaData = MetaData(source.url,
        Option(connection getHeaderField "Last-Modified"),
        Option(connection getHeaderField "ETag"),
        md5(content), timestamp)

      val download = TextDownload(0, metaData, content)

      parse(source, download, previous).fold {
        Future(None): Future[Option[Feed]]
      } { save(_, download) }
    } catch {
      case e: Exception =>
        Logger.warn("Failed to download: " + source.url, e)
        Future(None)
    }
  }

  def md5(data: String): String =
    MessageDigest.getInstance("MD5").digest(data.getBytes("UTF-8")).map("%02x".format(_)).mkString

  def parse(source: FeedSource, textDownload: TextDownload, previous: Option[Feed]): Option[Feed] = {
    val xml = XML.load(new StringReader(textDownload.content))
    val xmlDownload = XmlDownload(textDownload.metaData, xml)
    Feed.parse(source, xmlDownload, previous)
  }

  def loadSources(section: String): Seq[FeedSource] =
    Source.fromFile(s"feeds.$section.txt").getLines
      .map(_.trim)
      .filterNot(line => line.startsWith("#") || line.isEmpty) map { line =>
      line.split("\\|") match {
        case Array(feedUrl) => FeedSource(new URI(feedUrl), feedUrl)
        case Array(feedUrl, group) => FeedSource(new URI(feedUrl), nonEmpty(group, feedUrl))
        case Array(feedUrl, group, siteUrl) => FeedSource(new URI(feedUrl), nonEmpty(group, feedUrl), nonEmpty(siteUrl).map(new URI(_)))
        case Array(feedUrl, group, siteUrl, title) => FeedSource(new URI(feedUrl), nonEmpty(group, feedUrl), nonEmpty(siteUrl).map(new URI(_)), nonEmpty(title))
      }
    } toSeq
}
