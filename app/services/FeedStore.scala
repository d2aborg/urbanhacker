package services

import java.net.URI

import com.google.inject.Inject
import model.Utils._
import model._
import play.api.{Configuration, Logger}
import play.api.db.slick.DatabaseConfigProvider
import slick.driver.JdbcProfile
import slick.driver.PostgresDriver.api._

import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source

class FeedStore @Inject()(dbConfigProvider: DatabaseConfigProvider)(implicit exec: ExecutionContext) {
  val dbConfig = dbConfigProvider.get[JdbcProfile]
  val db = dbConfig.db

  Logger.info("Schemas:\n" + (downloads.schema ++ sources.schema).create.statements.mkString("\n"))

  def loadDownloads(url: URI): Future[Seq[Download]] = {
    val loaded = db.run(downloads.filter(_.url === url.toString).sortBy(_.timestamp).result)
    Logger.info("Loaded: " + url)
    loaded
  }

  def saveDownloads(download: Download): Future[Download] = {
    val saved = db.run((downloads returning downloads.map(_.id) into ((download, id) => download.copy(id = id))) += download)
    Logger.info("Saved: " + download.copy(content = download.content.substring(0, 100)))
    saved
  }

  def loadSources: Future[Seq[FeedSource]] =
    db.run(sources.result)
}
