package model

import java.net.URI
import java.time.ZonedDateTime

import services.SlickPgPostgresDriver.api._
import slick.lifted.Tag

case class FeedSource(id: Long, section: String, group: Option[String], url: URI, siteUrl: Option[URI] = None, title: Option[String] = None, timestamp: Option[ZonedDateTime]) {
}

class SourcesTable(tag: Tag) extends Table[FeedSource](tag, "sources") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

  def section = column[String]("section")

  def sectionIndex = index("sources_section_idx", section)

  def group = column[Option[String]]("group")

  def groupIndex = index("sources_group_idx", group)

  def url = column[String]("url")

  def urlIndex = index("sources_url_idx", url)

  def siteUrl = column[Option[String]]("site_url")

  def title = column[Option[String]]("title")

  def timestamp = column[Option[ZonedDateTime]]("timestamp")

  override def * =
    (id, section, group, url, siteUrl, title, timestamp).shaped <> ( {
      case (id, section, group, url, siteUrl, title, timestamp) => FeedSource(id, section, group, new URI(url), siteUrl.map(new URI(_)), title, timestamp)
    }, { source: FeedSource =>
      Some((source.id, source.section, source.group, source.url.toString, source.siteUrl.map(_.toString), source.title, source.timestamp))
    })

}

object sources extends TableQuery(new SourcesTable(_)) {
  val byId = this.findBy(_.id)

  def bySection(section: String): Query[SourcesTable, FeedSource, Seq] = filter {
    _.section startsWith section
  }
}
