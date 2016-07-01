package controllers

import java.time.{OffsetDateTime, ZoneOffset}
import javax.inject._

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

  def articles(section: String, timestamp: Option[String], page: Option[Int]) = Action { implicit request =>
    implicit val now = OffsetDateTime.now(ZoneOffset.UTC)

    val timestampOrNow = timestamp.flatMap(t => Try(OffsetDateTime.parse(t)).toOption) getOrElse now
    val (articles, latestTimestamp, nextPage) = feedCache(section, timestampOrNow, page getOrElse 1, 15)

    if (request.getQueryString("ajax") isEmpty)
      Ok(views.html.articles(section, articles, latestTimestamp, nextPage))
    else
      Ok(views.html.page(articles, latestTimestamp, nextPage))
  }
}
