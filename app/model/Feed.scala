package model

import java.net.URI
import java.time.ZonedDateTime

import model.Utils._
import play.api.Logger
import services.MyPostgresDriver.api._
import slick.lifted.Tag
import slick.model.ForeignKeyAction.{Cascade, Restrict}

import scala.xml.{Node, NodeSeq}

case class Feed(id: Option[Long], sourceId: Long, siteUrl: Option[URI], title: Option[String], metaData: MetaData) {
  def favicon: Option[URI] = siteUrl.map(_.resolve("/favicon.ico"))
}

object Feed {
  def parse(source: FeedSource, download: XmlDownload): Option[CachedFeed] = {
    if ((download.xml \\ "channel").nonEmpty) {
      val feed = Feed.rss(source, download, (download.xml \\ "channel") (0))
      val articles = for (item <- download.xml \\ "item"; article <- Article.rss(source, feed, item(0)))
        yield CachedArticle(source, feed, article)
      Some(CachedFeed(source, feed, articles))
    } else if (download.xml.label == "feed") {
      val feed = Feed.atom(source, download, download.xml(0))
      val articles = for (entry <- download.xml \\ "entry"; article <- Article.atom(source, feed, entry(0)))
        yield CachedArticle(source, feed, article)
      Some(CachedFeed(source, feed, articles))
    } else {
      Logger.warn("Couldn't find RSS or ATOM feed in XML: " + download.xml)
      None
    }
  }

  def rss(source: FeedSource, download: XmlDownload, channel: Node): Feed = {
    val title = nonEmpty(cleanTitle(channel \ "title"))

    val siteUrl = nonEmpty(unescape(channel \ "link")).flatMap(parseURI)

    Feed(None, source.id, source.siteUrl.orElse(siteUrl), source.title.orElse(title), download.metaData)
  }

  def atom(source: FeedSource, download: XmlDownload, feedRoot: Node): Feed = {
    val title = nonEmpty(cleanTitle(if ((feedRoot \ "title" text) nonEmpty) feedRoot \ "title" else feedRoot \ "id"))

    val siteUrl = nonEmpty((feedRoot \ "link").filterNot { l =>
      l \@ "rel" == "self" ||
        l \@ "rel" == "hub" ||
        l \@ "type" == "application/atom+xml"
    } \@ "href").map(new URI(_))

    if (siteUrl isEmpty)
      Logger.info("Found no viable link in feed: " + title + ", among: " + (feedRoot \ "link"))

    Feed(None, source.id, source.siteUrl.orElse(siteUrl), source.title.orElse(title), download.metaData)
  }

  def cleanTitle(title: NodeSeq): String = unescape(title)
    .replaceAll(" RSS Feed$", "")
    .replaceAll("^Latest blogs for ", "")
    .replaceAll(": the front page of the internet$", "")
    .replaceAll(" – Latest Articles$", "")
    .replaceAll(" — Medium$", "")
    .replaceAll(": The Full Feed$", "")
}

class FeedsTable(tag: Tag) extends Table[Feed](tag, "feeds") {
  def id = column[Option[Long]]("id", O.PrimaryKey, O.AutoInc)

  def sourceId = column[Long]("source_id")

  def source = foreignKey("source_fk", sourceId, sources)(_.id, onUpdate = Restrict, onDelete = Cascade)

  def siteUrl = column[Option[String]]("site_url")

  def title = column[Option[String]]("title")

  def lastModified = column[Option[String]]("last_modified")

  def eTag = column[Option[String]]("etag")

  def checksum = column[String]("checksum")

  def timestamp = column[ZonedDateTime]("timestamp")

  override def * =
    (id, sourceId, siteUrl, title, (lastModified, eTag, checksum, timestamp)).shaped <> ( {
      case (id, sourceId, siteUrl, title, (lastModified, eTag, checksum, timestamp)) =>
        Feed(id, sourceId, siteUrl.map(new URI(_)), title,
          MetaData(lastModified, eTag, checksum, timestamp))
    }, { f: Feed =>
      Some((f.id, f.sourceId, f.siteUrl.map(_.toString), f.title,
        (f.metaData.lastModified, f.metaData.eTag, f.metaData.checksum, f.metaData.timestamp)))
    })
}

object feeds extends TableQuery(new FeedsTable(_)) {
  val returningId = this returning this.map(_.id.get)

  val byId = this.findBy(_.id.get)

  def historic(section: String, timestamp: ZonedDateTime): Query[FeedsTable, Feed, Seq] =
    for {
      s <- sources.bySection(section)
      f <- feeds if f.sourceId === s.id && f.timestamp <= timestamp
    } yield f

  def byDownload(download: DownloadsTable) =
    filter(_.sourceId === download.sourceId).filter(_.timestamp === download.timestamp)
}

case class CachedFeed(source: FeedSource, record: Feed, articles: Seq[CachedArticle]) {
  def mkString: String = {
    copy(articles = articles.map(a => a.copy(record = a.record.copy(text = Utils.crop(a.record.text, 20))))).toString
  }
}

