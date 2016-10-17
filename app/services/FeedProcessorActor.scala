package services

import akka.actor.SupervisorStrategy.Restart
import akka.actor.{Actor, OneForOneStrategy}
import com.google.inject.Inject
import model.FeedSource
import services.FeedProcessorActor.{FetchFeeds, FullReload, ParseFeed}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object FeedProcessorActor {

  case class ParseFeed(source: FeedSource, downloadId: Long)

  case class FetchFeeds(sources: Seq[FeedSource])

  case class FullReload(sources: Seq[FeedSource])

}

class FeedProcessorActor @Inject()(feedStore: FeedStore)(implicit exec: ExecutionContext) extends Actor {

  override val supervisorStrategy =
    OneForOneStrategy(maxNrOfRetries = 100, withinTimeRange = 1 minute) {
      case _ => Restart
    }

  val feedFetcher = context.actorOf(FeedFetcherActor.props(feedStore))
  val feedParser = context.actorOf(FeedParserActor.props(feedStore))

  def receive = {
    case ff: FetchFeeds => feedFetcher ! ff
    case fr: FullReload => feedFetcher ! fr
    case pf: ParseFeed => feedParser ! pf
  }
}
