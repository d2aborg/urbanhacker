package model

import java.net.URI
import java.time.ZonedDateTime

import services.SlickUtil._
import slick.driver.MySQLDriver.api._
import slick.lifted.Tag

case class FeedSource(id: Long, section: String, group: Option[String], url: URI, siteUrl: Option[URI] = None, title: Option[String] = None, timestamp: Option[ZonedDateTime], active: Boolean = true)

class SourcesTable(tag: Tag) extends Table[FeedSource](tag, "sources") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

  def section = column[String]("section", O.SqlType("varchar(50)"))

  def sectionIndex = index("sources_section_idx", section)

  def group = column[Option[String]]("group", O.SqlType("varchar(50)"))

  def groupIndex = index("sources_group_idx", group)

  def url = column[URI]("url", O.SqlType("varchar(1000)"))

  def urlIndex = index("sources_url_idx", url)

  def siteUrl = column[Option[URI]]("site_url")

  def title = column[Option[String]]("title")

  def timestamp = column[Option[ZonedDateTime]]("timestamp")

  def active = column[Boolean]("active", O.Default(true))

  override def * =
    (id, section, group, url, siteUrl, title, timestamp, active).shaped <> ( {
      case (id, section, group, url, siteUrl, title, timestamp, active) =>
        FeedSource(id, section, group, url, siteUrl, title, timestamp, active)
    }, { source: FeedSource =>
      Some((source.id, source.section, source.group, source.url, source.siteUrl, source.title, source.timestamp, source.active))
    })

}

object sources extends TableQuery(new SourcesTable(_)) {
  val byId = this.findBy(_.id)

  def active = filter {
    _.active === true
  }

  def bySection(section: String): Query[SourcesTable, FeedSource, Seq] = filter { source =>
    source.active === true && (source.section startsWith section)
  }
}
