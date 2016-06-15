package controllers

import java.time.LocalDateTime
import javax.inject._

import akka.actor.ActorSystem
import model.Article
import play.api.Logger
import play.api.inject.ApplicationLifecycle
import play.api.mvc._
import services.FeedCache

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._

/**
  * This controller creates an `Action` to handle HTTP requests to the
  * application's home page.
  */
@Singleton
class HomeController @Inject()(actorSystem: ActorSystem, feedCache: FeedCache, lifecycle: ApplicationLifecycle)
                              (implicit exec: ExecutionContext) extends Controller {
  val feedCacheReloadTask = actorSystem.scheduler.schedule(15 seconds, 30 minutes) {
    feedCache.reload()
  }
  lifecycle.addStopHook { () =>
    Logger.info("Cancelling reload task...")
    Future.successful(feedCacheReloadTask.cancel)
  }

  def index = Action { implicit request =>
    load("news", "News")
  }

  def blogs = Action { implicit request =>
    load("blogs", "Blogs")
  }

  def about = Action { implicit request =>
    Ok(views.html.about(request))
  }

  def reload = Action {
    feedCache.reload()

    Ok("Reloaded")
  }

  def load(section: String, title: String)(implicit request: Request[AnyContent]): Result = {
    implicit val now = LocalDateTime now

    val pageNum = request.getQueryString("page").map(_.toInt).getOrElse(1)
    val timestamp = request.getQueryString("timestamp").map(LocalDateTime parse).getOrElse(now)

    val (articlesByFrecency, nextPage) = page(section, pageNum, timestamp)

    if (request.getQueryString("ajax") isEmpty)
      Ok(views.html.articles(title, articlesByFrecency, nextPage, timestamp))
    else
      Ok(views.html.page(articlesByFrecency, nextPage, timestamp))
  }

  def page(section: String, pageNum: Int, timestamp: LocalDateTime): (Seq[Article], Option[Int]) = {
    val articles = feedCache(section, timestamp)
    val (byFrecency, hasMore) = pageByFrecency(articles, pageNum, timestamp)
    val nextPage = if (hasMore) Some(pageNum + 1) else None
    (byFrecency, nextPage)
  }

  def pageByFrecency(articles: Seq[Article], pageNum: Int, timestamp: LocalDateTime): (Seq[Article], Boolean) = {
    val all = byFrecency(articles, timestamp)
    page(all, pageNum)
  }

  private val pageSize: Int = 30

  def page(all: Seq[Article], pageNum: Int): (Seq[Article], Boolean) = {
    val from = (pageNum - 1) * pageSize
    val until = pageNum * pageSize
    (all.slice(from, until), all.size > until)
  }

  def byFrecency(articles: Seq[Article], timestamp: LocalDateTime): Seq[Article] = {
    val ordering = Ordering.by[Article, Double](_.frecency(timestamp))
    val sorted = articles.sorted(ordering)
    sorted
  }
}
