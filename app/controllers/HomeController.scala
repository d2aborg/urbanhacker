package controllers

import javax.inject._

import play.api.Configuration
import play.api.mvc._

class HomeController @Inject()(configuration: Configuration) extends Controller {
  implicit val sections = configuration.getConfig("sections").getOrElse(Configuration.empty)

  def about = Action { implicit request =>
    Ok(views.html.about())
  }
}
