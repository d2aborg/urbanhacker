package controllers

import java.time.{ZoneOffset, ZonedDateTime}
import javax.inject._

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.ask
import akka.util.Timeout
import com.google.inject.name.Named
import model.Permalink.parseUrlTimestamp
import model.{CachedArticle, FeedSource, Permalink}
import play.api.{Configuration, Logger}
import play.api.inject.ApplicationLifecycle
import play.api.mvc._
import services.FeedFetcherActor.FetchFeed
import services.FeedStore

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

/**
  * This controller creates an `Action` to handle HTTP requests to the
  * application's home page.
  */
@Singleton
class ArticlesController @Inject()(feedStore: FeedStore,
                                   configuration: Configuration,
                                   actorSystem: ActorSystem,
                                   @Named("feed-fetcher-actor") feedFetcher: ActorRef,
                                   lifecycle: ApplicationLifecycle)
                                  (implicit exec: ExecutionContext) extends Controller {
  implicit val sections = configuration.getConfig("sections").getOrElse(Configuration.empty)

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

  def update(sources: Seq[FeedSource]): Unit = {
    Logger.info(s"###> Updating ${sources.size} sources...")

    for (updated <- Future.traverse(sources)(update).map(_.flatten))
      Logger.info(s"###> Updated ${updated.flatten.size}/${updated.size}")
  }

  def update(source: FeedSource): Future[Seq[Option[Long]]] = {
    implicit val timeout: Timeout = 60.seconds

    (feedFetcher ? FetchFeed(source)).mapTo[Seq[Option[Long]]]
  }

  def index(section: String, timestamp: String = "", page: Int = 1, ajax: Option[String]) = Action.async { implicit request =>
    implicit val now = ZonedDateTime.now(ZoneOffset.UTC)

    val requestedPermalink = parseUrlTimestamp(timestamp).map(t => Permalink(t, page))

    feedStore.articlePageByFrecency(section, requestedPermalink).map { (result: Option[(Seq[CachedArticle], Permalink)]) =>
      val (articles, resolvedPermalink) = result.fold((Seq.empty[CachedArticle], requestedPermalink.getOrElse(Permalink(parseUrlTimestamp(timestamp).getOrElse(now), 1))))(r => r)

      ajax.filter(_ == "true").fold {
        resolvedPermalink.requested.filterNot(_ == resolvedPermalink.timestamp.withZoneSameInstant(ZoneOffset.UTC)).fold {
          Ok(views.html.articleMain(section, articles, resolvedPermalink))
        } { _ =>
          Redirect(routes.ArticlesController.index(section, resolvedPermalink.urlTimestamp, resolvedPermalink.page, ajax))
        }
      } { _ =>
        Ok(views.html.articlePage(section, articles, resolvedPermalink))
      }
    }
  }

}
