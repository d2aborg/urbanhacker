package controllers

import java.time.{OffsetDateTime, ZoneOffset}
import javax.inject._

import model.Permalink
import model.Permalink.parseUrlTimestamp
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

  def articles(section: String, timestamp: String = "", page: Int = 1, ajax: Option[String]) = Action { implicit request =>
    implicit val now = OffsetDateTime.now(ZoneOffset.UTC)

    val requestedPermalink = parseUrlTimestamp(timestamp).map(t => Permalink(t, page))
    val (articles, resolvedPermalink) = feedCache(section, requestedPermalink)

    ajax.filter(_ == "true").fold {
      resolvedPermalink.requested.filterNot(_ == resolvedPermalink.timestamp).fold {
        Ok(views.html.articleMain(section, articles, resolvedPermalink))
      } { _ =>
        Redirect(routes.HomeController.articles(section, resolvedPermalink.urlTimestamp, resolvedPermalink.page, ajax))
      }
    } { _ =>
      Ok(views.html.articlePage(section, articles, resolvedPermalink))
    }
  }
}
