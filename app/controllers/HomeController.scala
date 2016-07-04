package controllers

import java.time.{OffsetDateTime, ZoneOffset}
import javax.inject._

import model.Utils
import model.Utils.nonEmpty
import play.api.Configuration
import play.api.mvc._
import services.FeedCache

import scala.concurrent.ExecutionContext
import scala.util.Try

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

  def articles(section: String, timestamp: String = "", page: Int = 1) = Action { implicit request =>
    implicit val now = OffsetDateTime.now(ZoneOffset.UTC)

    val timestampOrNow = nonEmpty(timestamp).flatMap(t => Try(OffsetDateTime.parse(t)).toOption) getOrElse now
    val (articles, latestTimestamp, nextPage) = feedCache(section, timestampOrNow, page, 15)

    if (request.getQueryString("ajax") isEmpty)
      Ok(views.html.articleMain(section, articles, latestTimestamp, page, nextPage))
    else
      Ok(views.html.articlePage(section, articles, latestTimestamp, page, nextPage))
  }
}
