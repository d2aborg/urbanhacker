package services

import java.time.ZonedDateTime

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.ask
import akka.util.Timeout
import com.google.inject.name.Named
import com.google.inject.{Inject, Singleton}
import model._
import play.api.Logger
import play.api.inject.ApplicationLifecycle
import services.FeedFetcherActor.FetchFeed

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class FeedCache @Inject()(feedStore: FeedStore,
                          actorSystem: ActorSystem,
                          @Named("feed-fetcher-actor") feedFetcher: ActorRef,
                          lifecycle: ApplicationLifecycle)
                         (implicit exec: ExecutionContext) {
  val refreshCycle = 30.minutes
  val refreshInterval = 1.minute
  var feedSources: Future[Seq[FeedSource]] = _

  for (x <- 1 to refreshCycle.toMinutes.toInt) yield {
    val reloadTask = actorSystem.scheduler.schedule(x * refreshInterval, refreshCycle) {
      if (x == 1)
        feedSources = feedStore.loadSources

      feedSources.foreach { feedSources =>
        val groupSize = ((feedSources.size + refreshCycle.toMinutes - 1) /
          refreshCycle.toMinutes).toInt
        update(feedSources.slice((x - 1) * groupSize, x * groupSize))
      }
    }

    lifecycle.addStopHook { () =>
      Future(reloadTask.cancel)
    }
  }

  def apply(section: String, permalink: Option[Permalink])
           (implicit now: ZonedDateTime): Future[Option[(Seq[CachedArticle], Permalink)]] =
    feedStore.articlePageByFrecency(section, permalink)

  def update(sources: Seq[FeedSource]): Unit = {
    Logger.info(s"###> Updating ${sources.size} sources...")

    for (updated <- Future.traverse(sources)(update).map(_.flatten))
      Logger.info(s"###> Updated ${updated.flatten.size}/${updated.size}")
  }

  def update(source: FeedSource): Future[Seq[Option[Long]]] = {
    implicit val timeout: Timeout = 60.seconds

    (feedFetcher ? FetchFeed(source)).mapTo[Seq[Option[Long]]]
  }
}
