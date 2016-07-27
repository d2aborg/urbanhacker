package services

import javax.inject.{Inject, Singleton}

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.ask
import akka.util.Timeout
import com.google.inject.name.Named
import model.FeedSource
import play.api.Logger
import play.api.inject.ApplicationLifecycle
import services.FeedFetcherActor.FetchFeed

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
  var feedSources: Future[Seq[Seq[Seq[FeedSource]]]] = _

  def toGroupsBySizeAndURL(cycleLength: Int)(sources: Seq[FeedSource]): Seq[Seq[Seq[FeedSource]]] = {
    val byGroup = sources.groupBy(_.group)
    val grouped = byGroup.filter(_._1.nonEmpty).values.toSeq
    val independent = byGroup(None).map(Seq(_))
    val all = grouped ++ independent
    val bySizeAndUrl = all.sortBy(group => (-group.size, group.head.group.getOrElse(group.head.url.toString)))

    val intervalSize = (bySizeAndUrl.flatten.size + cycleLength - 1) / cycleLength

    var byCycle = Seq.empty[Seq[Seq[FeedSource]]]
    var currentGroups = Seq.empty[Seq[FeedSource]]
    for (sourceGroup <- bySizeAndUrl) {
      currentGroups :+= sourceGroup
      if (currentGroups.flatten.size >= intervalSize) {
        byCycle :+= currentGroups
        currentGroups = Seq.empty[Seq[FeedSource]]
      }
    }

    while (byCycle.size < cycleLength)
      byCycle :+= Seq.empty

    Logger.info("Sources by cycle and group:\n" + byCycle.map(_.map(_.map(_.url))).mkString("\n"))

    byCycle
  }

  for (x <- 1 to refreshCycle.toMinutes.toInt) {
    val reloadTask = actorSystem.scheduler.schedule(x * refreshInterval, refreshCycle) {
      if (x == 1)
        feedSources = feedStore.loadSources map toGroupsBySizeAndURL(refreshCycle.toMinutes.toInt)

      feedSources.foreach { feedSources =>
        updateGroups(feedSources(x-1))
      }
    }

    lifecycle.addStopHook { () =>
      Future(reloadTask.cancel)
    }
  }

  def updateGroups(sourceGroups: Seq[Seq[FeedSource]]): Unit = {
    Logger.info(s"###> Updating ${sourceGroups.flatten.size} sources...")

    for (updated <- Future.traverse(sourceGroups)(updateGroup).map(_.flatten))
      Logger.info(s"###> Updated ${updated.flatMap(_._2).size}/${updated.size}")
  }

  def updateGroup(sourceGroup: Seq[FeedSource]): Future[Seq[(FeedSource, Option[Long])]] = {
    implicit val timeout: Timeout = 60.seconds

    (feedFetcher ? FetchFeed(sourceGroup)).mapTo[Seq[(FeedSource, Option[Long])]]
  }

}
