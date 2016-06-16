package services

import java.io._
import java.net.{HttpURLConnection, URL}
import java.security.cert.X509Certificate
import java.security.{MessageDigest, SecureRandom}
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME
import java.time.{LocalDateTime, ZoneOffset}
import java.util.regex.{Matcher, Pattern}
import javax.net.ssl._

import com.google.inject.{Inject, Singleton}
import model.{Article, Feed}
import play.api.Logger

import scala.collection.{SortedSet, mutable}
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.io.Source
import scala.pickling.Defaults._
import scala.pickling.json._
import scala.xml.{Elem, XML}

@Singleton
class FeedCache @Inject()(implicit exec: ExecutionContext) {

  case class Download(source: FeedSource,
                      xml: Elem,
                      feed: Feed,
                      metaData: MetaData,
                      timestamp: LocalDateTime,
                      previous: Option[Download]) {
    def byFeedUrl: (String, Download) = source.feedUrl -> this

    def articles(timestamp: LocalDateTime = LocalDateTime.now): SortedSet[Article] = {
      val myArticles = if (olderThan(timestamp)) feed.articles else SortedSet[Article]()
      val previousArticles = previous.map(_.articles(timestamp)).getOrElse(SortedSet[Article]())

      (myArticles ++ previousArticles).toSet[Article].to[SortedSet]
    }

    def olderThan(timestamp: LocalDateTime): Boolean = timestamp.compareTo(timestamp) <= 0
  }

  case class MetaData(lastModified: Option[String], eTag: Option[String], checksum: String)

  // configure the SSLContext with a permissive TrustManager
  val ctx = SSLContext.getInstance("TLS")
  ctx.init(Array[KeyManager](), Array[TrustManager](new X509TrustManager {
    override def checkClientTrusted(arg0: Array[X509Certificate], arg1: String): Unit = ()
    override def checkServerTrusted(arg0: Array[X509Certificate], arg1: String): Unit = ()
    override def getAcceptedIssuers: Array[X509Certificate] = null
  }), new SecureRandom())
  SSLContext.setDefault(ctx)

  val sections: List[String] = List("news", "blogs")
  val directory = new File("feeds")
  val filenameTimestampPattern = DateTimeFormatter.ofPattern("uuuu-MM-dd_HHmmss")
  val fileExtension: String = ".xml"
  val metaDataExtension: String = ".json"

  val cache: mutable.Map[String, mutable.Map[String, Download]] =
    mutable.Map((for (section <- sections)
      yield (section, mutable.Map(loadChains(section).map(_.byFeedUrl): _*))): _*)

  def apply(section: String, timestamp: LocalDateTime): Seq[Article] = cache synchronized {
    cache(section).values.flatMap(_.articles(timestamp)).toSeq
  }

  def latest(section: String, source: FeedSource): Option[Download] = cache synchronized {
    cache(section) get source.feedUrl
  }

  def reload(): Unit = {
    Logger.info("Reloading...")

    val feedBatches = for (section <- sections) yield (section, downloadAll(section))

    for ((section, eventualAllParsed) <- feedBatches; eventualParsed <- eventualAllParsed) {
      eventualParsed.onSuccess {
        case Some(download) =>
          cache synchronized {
            cache(section)(download.source.feedUrl) = download
            logInfo("Updated", download.source.feedUrl)
          }
      }
    }

    val eventuallyDownloaded = feedBatches flatMap (_._2)
    val downloaded = Await.result(Future sequence eventuallyDownloaded, 10 minutes).flatten
    Logger.info("Updated " + downloaded.size + "/" + eventuallyDownloaded.size)
  }

  def downloadAll(section: String): Seq[Future[Option[Download]]] =
    for (source <- loadSources(section))
      yield download(section, source)

  def download(section: String, source: FeedSource): Future[Option[Download]] =
    Future {
      download(source, latest(section, source))
    }

  def loadChains(section: String): Seq[Download] = {
    Await.result(Future.traverse {
      loadSources(section)
    } { source =>
      loadChainOrDownload(source)
    }, 10 minutes) flatten
  }

  def loadChainOrDownload(source: FeedSource): Future[Option[Download]] =
    Future {
      loadChain(source) orElse download(source)
    }

  def loadChain(source: FeedSource): Option[Download] = {
    files(source.feedUrl).foldLeft(None: Option[Download]) { (previous, file) =>
      load(source, file, previous)
    } map { download =>
      logInfo("Loaded " + download.articles().size + " from disk", source.feedUrl)
      download
    }
  }

  def load(source: FeedSource, file: File, previous: Option[Download]): Option[Download] = {
    val timestamp = fileTimestamp(source.feedUrl, file)
    try {
      val xml = XML.load(new InputStreamReader(new FileInputStream(file), "UTF-8"))
      val metaData = Source.fromFile(metaDataFile(source.feedUrl, timestamp)).mkString.unpickle[MetaData]

      parse(source, xml, metaData, timestamp, previous)
    } catch {
      case e: Exception =>
        logWarn("Failed to load from disk", file.getName, e)
        None
    }
  }

  def loadSources(section: String): Seq[FeedSource] =
    Source.fromFile(s"feeds.$section.txt").getLines
      .map(_.trim)
      .filterNot(line => line.startsWith("#") || line.isEmpty) map { line =>
      line.split("\\|") match {
        case Array(feedUrl) => FeedSource(feedUrl, None, None)
        case Array(feedUrl, siteUrl) => FeedSource(feedUrl, Some(siteUrl).filter(_.nonEmpty), None)
        case Array(feedUrl, siteUrl, title) => FeedSource(feedUrl, Some(siteUrl).filter(_.nonEmpty), Some(title).filter(_.nonEmpty))
      }
    } toSeq

  def fileTimestamp(url: String, file: File): LocalDateTime =
    LocalDateTime parse(filenameMatcher(url, file).get group 1, filenameTimestampPattern)

  def download(source: FeedSource, previous: Option[Download] = None): Option[Download] = {
    val timestamp = LocalDateTime.now

    try {
      val connection = new URL(source.feedUrl).openConnection.asInstanceOf[HttpURLConnection]
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
        previous map (_.timestamp.atOffset(ZoneOffset.UTC).format(RFC_1123_DATE_TIME))
      } foreach { lastModified =>
        connection setRequestProperty("If-Modified-Since", lastModified)
      }

      val xml = try {
        connection connect()
        if (connection.getResponseCode == 304)
          return None

        XML.load(connection.getInputStream)
      } finally {
        connection disconnect()
      }

      logInfo("Downloaded", source.feedUrl)

      val metaData = MetaData(
        Option(connection getHeaderField "Last-Modified"),
        Option(connection getHeaderField "ETag"),
        md5(xml.mkString))

      parse(source, xml, metaData, timestamp, previous).filter(save)
    } catch {
      case e: Exception =>
        logWarn("Failed to download", source.feedUrl, e)
        None
    }
  }

  def parse(source: FeedSource, xml: Elem, metaData: MetaData, timestamp: LocalDateTime, previous: Option[Download]): Option[Download] = {
    Feed.parse(source, xml).map(Download(source, xml, _, metaData, timestamp, previous))
  }

  def save(download: Download): Boolean = {
    if (download.previous.map(_.metaData.checksum).contains(download.metaData.checksum))
      return false

    if (download.previous.map(_.feed).contains(download.feed))
      return false

    if (!directory.isDirectory)
      return true

    try {
      val targetFile = file(download.source.feedUrl, download.timestamp)
      val targetMetaDataFile = metaDataFile(download.source.feedUrl, download.timestamp)

      if (!targetFile.getParentFile.isDirectory && !targetFile.getParentFile.mkdirs) {
        logInfo("Failed to create target directory " + targetFile.getParentFile + " for", download.source.feedUrl)
        return false
      }

      if (!targetMetaDataFile.getParentFile.isDirectory && !targetMetaDataFile.getParentFile.mkdirs) {
        logInfo("Failed to create metadata target directory " + targetMetaDataFile.getParentFile + " for", download.source.feedUrl)
        return false
      }

      try {
        XML.save(targetFile.getPath, download.xml, "UTF-8")
        writeMetaData(targetMetaDataFile, download.metaData)
        true
      } catch {
        case e: Exception =>
          targetFile.delete()
          targetMetaDataFile.delete()
          throw e
      }
    } catch {
      case e: Exception =>
        logWarn("Failed to save to disk", download.source.feedUrl, e)
        false
    }
  }

  def writeMetaData(file: File, metaData: MetaData): Unit = {
    val metaDataWriter = new BufferedWriter(new FileWriter(file))
    try {
      metaDataWriter.write(metaData.pickle.value)
      metaDataWriter.newLine()
    } finally {
      metaDataWriter.close()
    }
  }

  def md5(data: String): String =
    MessageDigest.getInstance("MD5").digest(data.getBytes("UTF-8")).map("%02x".format(_)).mkString

  def dir(url: String): File =
    new File(directory, dirname(url))

  def file(url: String, timestamp: LocalDateTime): File =
    new File(dir(url), filename(timestamp) + fileExtension)

  def metaDataFile(url: String, timestamp: LocalDateTime): File =
    new File(dir(url), filename(timestamp) + metaDataExtension)

  def dirname(url: String): String = shortUrl(url)

  def filename(timestamp: LocalDateTime): String = timestamp format filenameTimestampPattern

  def files(url: String): Array[File] = {
    Option(dir(url).listFiles(fileFilter(url))).map(_.sorted).getOrElse(Array[File]())
  }

  def fileFilter(url: String): FileFilter = new FileFilter {
    override def accept(file: File): Boolean = filenameMatcher(url, file) isDefined
  }

  def filenameMatcher(url: String, file: File): Option[Matcher] =
    filenamePattern(url) matcher file.getName match {
      case matcher if matcher.matches => Some(matcher)
      case _ => None
    }

  def filenamePattern(url: String): Pattern =
    Pattern compile "^(\\d{4}-\\d{2}-\\d{2}_\\d{6})" + Pattern.quote(fileExtension) + "$"

  def shortUrl(url: String): String = url
    .replaceFirst("^https?://(www\\.)?", "")
    .replaceFirst("^([^/]+)\\.com/", "$1/")
    .replaceAll("[^a-zA-Z0-9_\\-.]", "_")
    .replaceAll("_+", "_")
    .replaceFirst("^_", "")
    .replaceFirst("_$", "")

  def logInfo(msg: String, url: String): Unit = Logger.info(mkLogMsg(msg, url))

  def logWarn(msg: String, url: String, e: Exception): Unit = Logger.warn(mkLogMsg(msg, url), e)

  def mkLogMsg(msg: String, url: String): String = msg + ": " + shortUrl(url)
}

case class FeedSource(feedUrl: String, siteUrl: Option[String], title: Option[String])
