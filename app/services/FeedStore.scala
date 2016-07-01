package services

import java.net.URI

import com.google.inject.Inject
import model._
import play.api.Logger
import play.api.db.slick.DatabaseConfigProvider
import slick.driver.JdbcProfile
import slick.driver.PostgresDriver.api._

import scala.concurrent.{ExecutionContext, Future}

class FeedStore @Inject()(dbConfigProvider: DatabaseConfigProvider)(implicit exec: ExecutionContext) {
  val dbConfig = dbConfigProvider.get[JdbcProfile]
  val db = dbConfig.db
  Logger.info(downloads.schema.create.statements.mkString("\n"))

  def load(url: URI): Future[Seq[TextDownload]] = {
    val loaded = db.run(downloads.filter(_.url === url.toString).sortBy(_.timestamp).result)
    Logger.info("Loaded: " + url)
    loaded
  }

  def save(download: TextDownload): Future[Boolean] = {
    val saved = db.run(downloads += download).map(_ == 1)
    Logger.info("Saved: " + download.copy(content = download.content.substring(0, 100)))
    saved
  }
}
