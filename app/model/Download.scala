package model

import java.time.ZonedDateTime

import services.SlickPgPostgresDriver.api._
import slick.lifted.Tag
import slick.model.ForeignKeyAction.{Cascade, Restrict}

import scala.xml.Elem

case class MetaData(lastModified: Option[String], eTag: Option[String], checksum: String, timestamp: ZonedDateTime)

case class Download(id: Option[Long], sourceId: Long, metaData: MetaData, content: String) {
  def mkString: String = copy(content = Utils.crop(content, 100).replaceAll("\\s+", " ")).toString
}

case class ParsedDownload(record: Download, xml: Elem)

class DownloadsTable(tag: Tag) extends Table[Download](tag, "downloads") {
  def id = column[Option[Long]]("id", O.PrimaryKey, O.AutoInc)

  def sourceId = column[Long]("source_id")

  def source = foreignKey("source_fk", sourceId, sources)(_.id, onUpdate = Restrict, onDelete = Cascade)

  def lastModified = column[Option[String]]("last_modified")

  def eTag = column[Option[String]]("etag")

  def checksum = column[String]("checksum")

  def timestamp = column[ZonedDateTime]("timestamp")

  def content = column[String]("content")

  override def * =
    (id, sourceId, (lastModified, eTag, checksum, timestamp), content).shaped <>( {
      case (id, sourceId, (lastModified, eTag, checksum, timestamp), content) =>
        Download(id, sourceId, MetaData(lastModified, eTag, checksum, timestamp), content)
    }, { d: Download =>
      Some((d.id, d.sourceId, (d.metaData.lastModified, d.metaData.eTag, d.metaData.checksum, d.metaData.timestamp), d.content))
    })

  def timestampIndex = index("downloads_timestamp_idx", timestamp)
}

object downloads extends TableQuery(new DownloadsTable(_)) {
  val returningId = this returning this.map(_.id.get)

  val byId = this.findBy(_.id)
}
