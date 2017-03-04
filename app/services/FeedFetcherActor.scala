package services

import java.net.HttpURLConnection
import java.security.cert.X509Certificate
import java.security.{MessageDigest, SecureRandom}
import java.time.format.DateTimeFormatter
import java.time.{ZoneOffset, ZonedDateTime}
import javax.net.ssl._

import akka.actor._
import com.markatta.timeforscala._
import model.Utils._
import model._
import play.api.Logger
import services.FeedProcessorActor.{FetchFeeds, FullReload, ParseFeed}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

object FeedFetcherActor {
  def props(feedStore: FeedStore)(implicit exec: ExecutionContext) = Props(new FeedFetcherActor(feedStore))

  // configure the SSLContext with a permissive TrustManager
  SSLContext.setDefault(SSLContext.getInstance("TLS").tap { ctx =>
    ctx.init(Array[KeyManager](), Array[TrustManager](new X509TrustManager {
      override def checkClientTrusted(arg0: Array[X509Certificate], arg1: String): Unit = ()

      override def checkServerTrusted(arg0: Array[X509Certificate], arg1: String): Unit = ()

      override def getAcceptedIssuers: Array[X509Certificate] = null
    }), new SecureRandom())
  })

}

class FeedFetcherActor(val feedStore: FeedStore)(implicit val exec: ExecutionContext) extends Actor {
  def receive = {
    case FetchFeeds(sources: Seq[FeedSource]) =>
      Await.result(update(sources), 10.minutes)

    case FullReload(sources: Seq[FeedSource]) =>
      Await.result(fullReload(sources), 10.minutes)
  }

  def update(sources: Seq[FeedSource]): Future[Seq[Long]] =
    for {
      maybeDownloaded <- downloadSave(sources)
      downloadedByTimestamp = for {
        (source, downloadId, metaData) <- maybeDownloaded.flatten.sortBy(_._3.timestamp)
      } yield (source, downloadId)
    } yield scheduleParse(downloadedByTimestamp)

  def fullReload(sources: Seq[FeedSource]): Future[Seq[Long]] =
    for {
      unparsed <- feedStore.loadIncompleteDownloadIds(sources, Feed.parseVersion)
    } yield scheduleParse(unparsed)

  def scheduleParse(downloads: Seq[(FeedSource, Long)]): Seq[Long] = {
    for ((source, downloadId) <- downloads) {
      sender ! ParseFeed(source, downloadId)
    }
    Logger.info(s"Scheduled parsing of ${downloads.size} downloads...")
    downloads.map(_._2)
  }

  def downloadSave(sources: Seq[FeedSource]): Future[Seq[Option[(FeedSource, Long, MetaData)]]] =
    Future.traverse(sources) { source =>
      (for {
        maybeLatest <- feedStore.loadLatestMetaData(source)
        maybeDownloaded <- download(source, maybeLatest)
        maybeSaved <- Futures.traverse(maybeDownloaded)(feedStore.saveDownload(source))
      } yield maybeSaved) recover {
        case t =>
          Logger.warn(s"${source.url}: Failed to download feed", t)
          None
      }
    }

  def download(source: FeedSource, previous: Option[MetaData]): Future[Option[Download]] = {
    val timestamp = ZonedDateTime.now(ZoneOffset.UTC)
    feedStore.updateSourceTimestamp(source.copy(timestamp = Some(timestamp)))

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

      previous flatMap (_.eTag) foreach {
        eTag => connection setRequestProperty("If-None-Match", eTag)
      }

      previous flatMap (_.lastModified) orElse {
        previous map (_.timestamp.format(DateTimeFormatter.RFC_1123_DATE_TIME))
      } foreach {
        lastModified =>
          connection setRequestProperty("If-Modified-Since", lastModified)
      }

      val eventualMaybeContentAndEncoding = Future {
        try connection.tap(_.connect()) match {
          case c if c.getResponseCode == 304 => None
          case c =>
            val encoding = Option(c.getContentEncoding)
            val is = c.getInputStream
            val bytes = Stream.continually(is.read).takeWhile(_ != -1).map(_.toByte).toArray
            Some(bytes, encoding)
        } finally connection disconnect()
      }

      for (maybeContentAndEncoding <- eventualMaybeContentAndEncoding)
        yield maybeContentAndEncoding.flatMap {
          case (content, encoding) =>
            val checksum = md5(content)
            if (previous.map(_.checksum).contains(checksum))
              None
            else
              Some(Download(None, source.id, MetaData(
                Option(connection getHeaderField "Last-Modified"),
                Option(connection getHeaderField "ETag"),
                checksum, timestamp), content, encoding))
        }
    } catch {
      case e: Exception =>
        Future.failed(e)
    }
  }

  def md5(data: String): String = md5(data.getBytes("UTF-8"))

  def md5(data: Array[Byte]): String =
    MessageDigest.getInstance("MD5").digest(data).map("%02x".format(_)).mkString
}