package model

import java.net.URI
import java.sql.Timestamp
import java.time.{OffsetDateTime, ZoneOffset}

import slick.driver.PostgresDriver.api._
import slick.lifted.Tag

import scala.xml.Elem

case class MetaData(url: URI, lastModified: Option[String], eTag: Option[String], checksum: String, timestamp: OffsetDateTime)
case class Download(id: Long, metaData: MetaData, content: String)
case class XmlDownload(metaData: MetaData, xml: Elem)

class DownloadsTable(tag: Tag) extends Table[Download](tag, "downloads") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

  def url = column[String]("url")

  def lastModified = column[Option[String]]("last_modified")

  def eTag = column[Option[String]]("etag")

  def checksum = column[String]("checksum")

  def timestamp = column[Timestamp]("timestamp")

  def content = column[String]("content")

  override def * =
    (id, (url, lastModified, eTag, checksum, timestamp), content).shaped <>( {
      case (id, (url, lastModified, eTag, checksum, timestamp), content) => Download(id, MetaData(new URI(url), lastModified, eTag, checksum, timestamp.toInstant.atOffset(ZoneOffset.UTC)), content)
    }, { download: Download =>
      Some((download.id, (
        download.metaData.url.toString, download.metaData.lastModified, download.metaData.eTag, download.metaData.checksum,
        new Timestamp(download.metaData.timestamp.toInstant.toEpochMilli)),
        download.content))
    })
}

object downloads extends TableQuery(new DownloadsTable(_)) {
  val byUrl = this.findBy(_.url)
}
