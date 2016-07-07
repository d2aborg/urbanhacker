package model

import java.net.URI

import slick.driver.PostgresDriver.api._
import slick.lifted.Tag

case class FeedSource(id: Long, section: String, group: Option[String], url: URI, siteUrl: Option[URI] = None, title: Option[String] = None) {
  def favicon: Option[URI] =
    siteUrl.map(_.resolve("/favicon.ico"))
}

class SourcesTable(tag: Tag) extends Table[FeedSource](tag, "sources") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

  def section = column[String]("section")

  def group = column[Option[String]]("group")

  def url = column[String]("url")

  def siteUrl = column[Option[String]]("site_url")

  def title = column[Option[String]]("title")

  override def * =
    (id, section, group, url, siteUrl, title).shaped <>( {
      case (id, section, group, url, siteUrl, title) => FeedSource(id, section, group, new URI(url), siteUrl.map(new URI(_)), title)
    }, { source: FeedSource =>
      Some((source.id, source.section, source.group, source.url.toString, source.siteUrl.map(_.toString), source.title))
    })
}

object sources extends TableQuery(new SourcesTable(_)) {
  val byUrl = this.findBy(_.url)
}
