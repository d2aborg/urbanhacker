package services

import javax.inject.{Inject, Singleton}

import akka.actor.{ActorRef, ActorSystem}
import com.google.inject.name.Named
import com.markatta.timeforscala._
import model.FeedSource
import play.api.Logger
import play.api.inject.ApplicationLifecycle
import services.FeedFetcherActor.{FetchFeeds, FullReload}

import model.Utils._
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class FeedUpdater @Inject()(feedStore: FeedStore,
                            actorSystem: ActorSystem,
                            @Named("feed-fetcher-actor") feedFetcher: ActorRef,
                            lifecycle: ApplicationLifecycle)
                           (implicit exec: ExecutionContext) {
  val refreshCycle = 30.minutes
  val refreshInterval = 1.minute
  var eventualFeedSourceBatches: Future[Seq[Seq[FeedSource]]] = _

  Logger.info("Scheduling updates...")

  for (x <- 1 to refreshCycle.toMinutes.toInt) {
    actorSystem.scheduler.schedule(refreshInterval * x, refreshCycle) {
      Logger.info("Running refresh cycle #" + x + "/" + refreshCycle.toMinutes)

      if (x == 1) {
        Logger.info("Performing full refresh at cycle 1...")

        val eventualSources = feedStore.loadSources recover { case t =>
          Logger.warn("Failed to load sources", t)
          Seq.empty
        }

        eventualFeedSourceBatches = eventualSources map batch(refreshCycle.toMinutes.toInt)

        eventualSources foreach fullRefresh
      }

      eventualFeedSourceBatches foreach { feedSourceBatch =>
        if (feedSourceBatch.size >= x)
          update(feedSourceBatch(x - 1))
      }
    } tap { reloadTask =>
      lifecycle.addStopHook { () =>
        Future(reloadTask.cancel)
      }
    }
  }

  def batch(cycleLength: Int)(sources: Seq[FeedSource]): Seq[Seq[FeedSource]] = {
    sources.grouped((sources.size + cycleLength - 1) / cycleLength).toSeq
  }

  def fullRefresh(sources: Seq[FeedSource]): Unit = {
    Logger.info(s"###> Full refresh of ${sources.size} sources...")

    feedFetcher ! FullReload(sources)
  }

  def update(sources: Seq[FeedSource]): Unit = {
    Logger.info(s"###> Updating ${sources.size} sources...")

    feedFetcher ! FetchFeeds(sources)
  }
}
