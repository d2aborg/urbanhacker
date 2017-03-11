package services

import java.time.ZonedDateTime
import javax.inject.{Inject, Singleton}

import akka.actor.{ActorRef, ActorSystem}
import com.google.inject.name.Named
import com.markatta.timeforscala._
import model.FeedSource
import model.Utils._
import play.api.Logger
import play.api.inject.ApplicationLifecycle
import services.FeedProcessorActor.{FetchFeeds, FullReload}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class FeedUpdater @Inject()(feedStore: FeedStore,
                            actorSystem: ActorSystem,
                            @Named("feed-processor") feedProcessor: ActorRef,
                            lifecycle: ApplicationLifecycle)
                           (implicit exec: ExecutionContext) {
  val refreshCycle = 30.minutes
  val refreshCycleMinutes = refreshCycle.toMinutes.toInt
  val refreshInterval = 1.minute

  Logger.info("Scheduling updates...")

  for (x <- 1 to refreshCycleMinutes) {
    actorSystem.scheduler.schedule(refreshInterval * x, refreshCycle) {
      Logger.info("Running refresh cycle #" + x + "/" + refreshCycleMinutes)

      feedStore.loadSources recover { case t =>
        Logger.warn("Failed to load sources", t)
        Seq.empty
      } foreach { sources =>
        if (x == 1)
          refreshBacklog(sources)

        val now = ZonedDateTime.now

        val outdatedSources = sources.filter(_.timestamp.forall(now > _.plusMinutes(refreshCycleMinutes * 2)))

        val intervalSources = sources
          .filter(_.timestamp.forall(now > _.plusMinutes(refreshCycleMinutes)))
          .sortBy(_.timestamp)
          .take((sources.size + refreshCycleMinutes - 1) / refreshCycleMinutes)

        update(outdatedSources ++ intervalSources)
      }
    } tap { reloadTask =>
      lifecycle.addStopHook { () =>
        Future(reloadTask.cancel)
      }
    }
  }

  def refreshBacklog(sources: Seq[FeedSource]): Unit = {
    Logger.info(s"Refreshing backlog of ${sources.size} sources...")

    feedProcessor ! FullReload(sources)
  }

  def update(sources: Seq[FeedSource]): Unit = {
    Logger.info(s"Updating ${sources.size} sources...")

    feedProcessor ! FetchFeeds(sources)
  }
}
