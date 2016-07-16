package controllers

import java.time.{ZoneOffset, ZonedDateTime}
import javax.inject._

import model.Permalink.parseUrlTimestamp
import model.{CachedArticle, Permalink}
import play.api.Configuration
import play.api.mvc._
import services.FeedCache

import scala.concurrent.ExecutionContext

/**
  * This controller creates an `Action` to handle HTTP requests to the
  * application's home page.
  */
@Singleton
class HomeController @Inject()(feedCache: FeedCache, configuration: Configuration)
                              (implicit exec: ExecutionContext) extends Controller {
  implicit val sections = configuration.getConfig("sections").getOrElse(Configuration.empty)

  def about = Action { implicit request =>
    Ok(views.html.about())
  }

  def articles(section: String, timestamp: String = "", page: Int = 1, ajax: Option[String]) = Action.async { implicit request =>
    implicit val now = ZonedDateTime.now(ZoneOffset.UTC)

    val requestedPermalink = parseUrlTimestamp(timestamp).map(t => Permalink(t, page))

    feedCache(section, requestedPermalink).map { (result: Option[(Seq[CachedArticle], Permalink)]) =>
      val (articles, resolvedPermalink) = result.fold((Seq.empty[CachedArticle], requestedPermalink.getOrElse(Permalink(parseUrlTimestamp(timestamp).getOrElse(now), 1))))(r => r)

      ajax.filter(_ == "true").fold {
        resolvedPermalink.requested.filterNot(_ == resolvedPermalink.timestamp.withZoneSameInstant(ZoneOffset.UTC)).fold {
          Ok(views.html.articleMain(section, articles, resolvedPermalink))
        } { _ =>
          Redirect(routes.HomeController.articles(section, resolvedPermalink.urlTimestamp, resolvedPermalink.page, ajax))
        }
      } { _ =>
        Ok(views.html.articlePage(section, articles, resolvedPermalink))
      }
    }
  }

}
