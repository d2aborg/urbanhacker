package services

import java.net.HttpURLConnection
import java.security.{MessageDigest, SecureRandom}
import java.security.cert.X509Certificate
import java.time.format.DateTimeFormatter
import java.time.{ZoneOffset, ZonedDateTime}
import javax.net.ssl._

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import com.google.inject.Inject
import com.google.inject.name.Named
import model.Utils._
import model._
import play.api.Logger
import services.FeedFetcherActor.FetchFeed
import services.FeedParserActor.ParseFeed

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.io.Source

object FeedFetcherActor {
  def props = Props[FeedFetcherActor]

  case class FetchFeed(sources: Seq[FeedSource])

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
    case FetchFeed(sourceGroup: Seq[FeedSource]) =>
      sender() ! Await.result(update(sourceGroup).recover {
        case t =>
          Logger.warn("Failed to update: " + sourceGroup.map(_.url), t)
          Seq.empty
      }, 60.minutes)
  }

  def update(sourceGroup: Seq[FeedSource]): Future[Seq[(FeedSource, Option[Long])]] = {
    for {
      deleted <- feedStore.deleteOutOfVersionFeeds(sourceGroup, Feed.parseVersion)
      someUnparsed <- feedStore.loadUnparsedDownloadIds(sourceGroup).map(_.map { case (s, dId) => (s, Some(dId)) })
      maybeDownloaded <- downloadSave(sourceGroup)
      allParsed <- Future {
        (someUnparsed ++ maybeDownloaded).map { case (source, maybeDownloadId) =>
          (source, maybeDownloadId.flatMap(parse(source)))
        }
      }
    } yield allParsed
  }

  def parse(source: FeedSource)(downloadId: Long): Option[Long] = {
    implicit val timeout: Timeout = 20.seconds

    Await.result((feedParser ? ParseFeed(source, downloadId)).mapTo[Option[Long]], 20.seconds)
  }

  def downloadSave(sourceGroup: Seq[FeedSource]): Future[Seq[(FeedSource, Option[Long])]] =
    Future.traverse(sourceGroup) { source =>
      for {
        maybeLatest <- feedStore.loadLatestMetaData(source)
        maybeDownloaded <- download(source, maybeLatest)
        maybeSaved <- Futures.traverse(maybeDownloaded)(feedStore.saveDownload(source))
      } yield (source, maybeSaved)
    }

  def download(source: FeedSource, previous: Option[MetaData]): Future[Option[Download]] = {
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
          case c => Some(Source.fromInputStream(c.getInputStream, Option(c.getContentEncoding).getOrElse("UTF-8")).mkString)
        } finally connection disconnect()
      }

      val timestamp = ZonedDateTime.now(ZoneOffset.UTC)

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