package services

import java.net.HttpURLConnection
import java.security.cert.X509Certificate
import java.security.{MessageDigest, SecureRandom}
import java.time.format.DateTimeFormatter
import java.time.{ZoneOffset, ZonedDateTime}
import javax.net.ssl._

import akka.actor._
import com.google.inject.Inject
import com.google.inject.name.Named
import com.markatta.timeforscala._
import model.Utils._
import model._
import play.api.Logger
import services.FeedFetcherActor.{FetchFeeds, FullReload}
import services.FeedParserActor.ParseFeed

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.io.Source

object FeedFetcherActor {
  def props = Props[FeedFetcherActor]

  case class FetchFeeds(sources: Seq[FeedSource])

  case class FullReload(sources: Seq[FeedSource])

  // configure the SSLContext with a permissive TrustManager
  SSLContext.setDefault(SSLContext.getInstance("TLS").tap { ctx =>
    ctx.init(Array[KeyManager](), Array[TrustManager](new X509TrustManager {
      override def checkClientTrusted(arg0: Array[X509Certificate], arg1: String): Unit = ()

      override def checkServerTrusted(arg0: Array[X509Certificate], arg1: String): Unit = ()

      override def getAcceptedIssuers: Array[X509Certificate] = null
    }), new SecureRandom())
  })

}

class FeedFetcherActor @Inject()(feedStore: FeedStore,
                                 @Named("feed-parser-actor") feedParser: ActorRef)
                                (implicit exec: ExecutionContext) extends Actor {
  def receive = {
    case FetchFeeds(sources: Seq[FeedSource]) =>
      Await.result(update(sources), 10.minutes)

    case FullReload(sources: Seq[FeedSource]) =>
      Await.result(fullReload(sources), 10.minutes)
  }

  def update(sources: Seq[FeedSource]): Future[Seq[Long]] =
    for {
      downloaded <- downloadSave(sources).map {
        _.flatten.sortBy(_._3.timestamp).map {
          case (source, downloadId, metaData) => (source, downloadId)
        }
      }
    } yield scheduleParse(downloaded)

  def fullReload(sources: Seq[FeedSource]): Future[Seq[Long]] =
    for {
      unparsed <- feedStore.loadIncompleteDownloadIds(sources, Feed.parseVersion)
    } yield scheduleParse(unparsed)

  def scheduleParse(downloads: Seq[(FeedSource, Long)]): Seq[Long] = {
    (for ((source, downloadId) <- downloads) yield {
      feedParser ! ParseFeed(source, downloadId); downloadId
    }) tap { scheduledDownloadIds =>
      Logger.info(s"Scheduled parsing of ${scheduledDownloadIds.size} downloads...")
    }
  }

  def downloadSave(sources: Seq[FeedSource]): Future[Seq[Option[(FeedSource, Long, MetaData)]]] =
    Future.traverse(sources) { source =>
      for {
        maybeLatest <- feedStore.loadLatestMetaData(source)
        maybeDownloaded <- download(source, maybeLatest)
        maybeSaved <- Futures.traverse(maybeDownloaded)(feedStore.saveDownload(source))
      } yield maybeSaved
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

      val eventualMaybeContent = Future {
        try connection.tap(_.connect()) match {
          case c if c.getResponseCode == 304 => None
          case c =>
            val encoding = Option(c.getContentEncoding).getOrElse("UTF-8")
            val source = Source.fromInputStream(c.getInputStream, encoding)
            Some(source.mkString)
        } finally connection disconnect()
      }

      for (maybeContent <- eventualMaybeContent)
        yield maybeContent.flatMap(content => {
          val checksum = md5(content)
          if (previous.map(_.checksum).contains(checksum))
            None
          else {
            Some(Download(None, source.id, MetaData(
              Option(connection getHeaderField "Last-Modified"),
              Option(connection getHeaderField "ETag"),
              checksum, timestamp), content))
          }
        })
    } catch {
      case e: Exception =>
        Future.failed(e)
    }
  }

  def md5(data: String): String =
    MessageDigest.getInstance("MD5").digest(data.getBytes("UTF-8")).map("%02x".format(_)).mkString
}