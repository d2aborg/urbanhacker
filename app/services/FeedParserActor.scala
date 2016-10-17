package services

import java.io.StringReader

import akka.actor._
import model.{Feed, _}
import play.api.Logger
import services.FeedProcessorActor.ParseFeed

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.xml.XML

object FeedParserActor {
  def props(feedStore: FeedStore)(implicit exec: ExecutionContext) = Props(new FeedParserActor(feedStore))
}

class FeedParserActor(val feedStore: FeedStore)(implicit val exec: ExecutionContext) extends Actor {
  def receive = {
    case ParseFeed(source: FeedSource, downloadId: Long) =>
      sender ! Await.result(parseSave(source, downloadId).recover {
        case t =>
          Logger.warn("Failed to parse: " + source.url, t)
          feedStore.deleteUnparsedDownload(source, downloadId)
          None
      }, 20.seconds)
  }

  def parseSave(source: FeedSource, downloadId: Long): Future[Option[Long]] = {
    for {
      maybeLoaded <- feedStore.loadDownload(downloadId)
      maybeParsed <- Futures.traverse(maybeLoaded)(parse(source)).map(_.flatten)
      maybeSaved <- Futures.traverse(maybeParsed)(feedStore.saveCachedFeed).map(_.flatten)
    } yield maybeSaved
  }

  def parse(source: FeedSource)(download: Download): Future[Option[CachedFeed]] = Future {
    Logger.info("---> Parsing feed: " + source.url)
    val xml = XML.load(new StringReader(download.content))
    val parsedDownload = ParsedDownload(download, xml)
    Feed.parse(source, parsedDownload)
  }
}
