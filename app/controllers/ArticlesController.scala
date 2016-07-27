package controllers

import java.time.{ZoneOffset, ZonedDateTime}
import javax.inject._

import model.Permalink
import model.Permalink.parseUrlTimestamp
import play.api.Configuration
import play.api.mvc._
import services.FeedStore

import scala.concurrent.{ExecutionContext, Future}

class ArticlesController @Inject()(feedStore: FeedStore, configuration: Configuration)
                                  (implicit exec: ExecutionContext) extends Controller {
  implicit val sections = configuration.getConfig("sections").getOrElse(Configuration.empty)

  def index(section: String, timestamp: String = "", pageNum: Int = 1, ajax: Option[String] = None) = Action.async { implicit request =>
    implicit val now = ZonedDateTime.now(ZoneOffset.UTC)

    val requestedPermalink = parseUrlTimestamp(timestamp).map(ts => Permalink(ts, pageNum))

    val eventualMaybeResolvedPermalink = ajax.filter(_ == "true").fold(feedStore.resolvePermalink(section, requestedPermalink))(_ => Future.successful(None))
    eventualMaybeResolvedPermalink.flatMap { maybeResolvedPermalink =>
      maybeResolvedPermalink.fold {
        feedStore.articlePageByFrecency(section, requestedPermalink).map { result =>
          ajax.filter(_ == "true").fold {
            Ok(views.html.articleMain(section, result))
          } { _ =>
            Ok(views.html.articlePage(section, result))
          }
        }
      } { resolvedPermalink =>
        Future.successful(Redirect(routes.ArticlesController.index(section, resolvedPermalink.urlTimestamp, resolvedPermalink.pageNum, ajax)))
      }
    }
  }

}
