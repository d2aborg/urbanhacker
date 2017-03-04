package model

import com.markatta.timeforscala._

import java.net.URI
import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit

import model.Utils._
import play.api.Logger
import services.SlickPgPostgresDriver.api._
import slick.lifted.Tag
import slick.model.ForeignKeyAction.{Cascade, Restrict}

import scala.xml.{Node, NodeSeq}

case class Feed(id: Option[Long],
                sourceId: Long,
                downloadId: Long,
                siteUrl: Option[URI],
                title: Option[String],
                metaData: MetaData,
                var frequency: Double = Double.PositiveInfinity,
                var groupFrequency: Double = Double.PositiveInfinity,
                parseVersion: Int = Feed.parseVersion) {
  def favicon: Option[URI] = siteUrl.map(_.resolve("/favicon.ico"))
}

object Feed {
  val parseVersion = 6

  def parse(source: FeedSource, download: ParsedDownload): Option[CachedFeed] = {
    val maybeParsed: Option[(Feed, Seq[Option[Article]])] = if ((download.xml \\ "channel").nonEmpty) {
      val feed = Feed.rss(source, download, (download.xml \\ "channel").head)
      val articles = for (item <- download.xml \\ "item") yield Article.rss(source, feed, item.head)
      Some((feed, articles))
    } else if (download.xml.label == "feed") {
      val feed = Feed.atom(source, download, download.xml.head)
      val articles = for (entry <- download.xml \\ "entry") yield Article.atom(source, feed, entry.head)
      Some((feed, articles))
    } else {
      Logger.warn("Couldn't find RSS or ATOM feed in XML: " + download.xml)
      None
    }

    maybeParsed.map { case (feed, maybeArticles) =>
      val articles = pruneDuplicateArticles(maybeArticles.flatten).map(CachedArticle(source, feed, _))
      feed.frequency = frequency(articles.map(_.record.pubDate))
      CachedFeed(source, feed, articles)
    }
  }

  def pruneDuplicateArticles(articles: Seq[Article]): Seq[Article] = {
    articles
      .groupBy(_.link).values.map(_.minBy(_.pubDate)).toSeq
      .groupBy(a => (a.title, a.imageSource, a.text)).values.map(_.minBy(_.pubDate)).toSeq
  }

  def rss(source: FeedSource, download: ParsedDownload, channel: Node): Feed = {
    val title = nonEmpty(cleanTitle(channel \ "title"))

    val siteUrl = nonEmpty(unescape(channel \ "link")).flatMap(parseURI)

    Feed(None, source.id, download.record.id.get, source.siteUrl.orElse(siteUrl), source.title.orElse(title), download.record.metaData)
  }

  def atom(source: FeedSource, download: ParsedDownload, feedRoot: Node): Feed = {
    val title = nonEmpty(cleanTitle(if ((feedRoot \ "title" text) nonEmpty) feedRoot \ "title" else feedRoot \ "id"))

    val siteUrl = nonEmpty((feedRoot \ "link").filterNot { l =>
      l \@ "rel" == "self" ||
        l \@ "rel" == "hub" ||
        l \@ "type" == "application/atom+xml"
    } \@ "href").map(new URI(_))

    if (siteUrl isEmpty)
      Logger.warn("Found no viable link in feed: " + title + ", among: " + (feedRoot \ "link"))

    Feed(None, source.id, download.record.id.get, source.siteUrl.orElse(siteUrl), source.title.orElse(title), download.record.metaData)
  }

  def cleanTitle(title: NodeSeq): String = unescape(title)
    .replaceAll(" RSS Feed$", "")
    .replaceAll("^Latest blogs for ", "")
    .replaceAll(": the front page of the internet$", "")
    .replaceAll(" – Latest Articles$", "")
    .replaceAll(" — Medium$", "")
    .replaceAll(": The Full Feed$", "")

  def frequency(dateTimes: Seq[ZonedDateTime]): Double = {
    val mostRecent = dateTimes.sorted.reverse.take(10)
    val periods = mostRecent.sliding(2).toSeq
    val periodSeconds = periods.map(range => ChronoUnit.SECONDS.between(range.last, range.head))
    val weights = (1 to periodSeconds.size).map(1.0 / _)
    val weightedPeriods = for ((period, weight) <- periodSeconds.zip(weights)) yield period * weight
    val weightedAveragePeriod = weightedPeriods.sum / weights.sum
    1.0 / weightedAveragePeriod
  }
}

class FeedsTable(tag: Tag) extends Table[Feed](tag, "feeds") {
  def id = column[Option[Long]]("id", O.PrimaryKey, O.AutoInc)

  def sourceId = column[Long]("source_id")

  def source = foreignKey("source_fk", sourceId, sources)(_.id, onUpdate = Restrict, onDelete = Cascade)

  def downloadId = column[Long]("download_id")

  def download = foreignKey("download_fk", downloadId, downloads)(_.id.get, onUpdate = Restrict, onDelete = Cascade)

  def downloadIdIndex = index("feeds_download_id_idx", downloadId, unique = true)

  def siteUrl = column[Option[String]]("site_url")

  def title = column[Option[String]]("title")

  def lastModified = column[Option[String]]("last_modified")

  def eTag = column[Option[String]]("etag")

  def checksum = column[String]("checksum")

  def timestamp = column[ZonedDateTime]("timestamp")

  def timestampIndex = index("feeds_timestamp_idx", timestamp)

  def frequency = column[Double]("frequency")

  def groupFrequency = column[Double]("group_frequency")

  def parseVersion = column[Int]("parse_version")

  def parseVersionIndex = index("feeds_parse_version_idx", parseVersion)

  override def * =
    (id, sourceId, downloadId, siteUrl, title, (lastModified, eTag, checksum, timestamp), frequency, groupFrequency, parseVersion).shaped <> ( {
      case (id, sourceId, downloadId, siteUrl, title, (lastModified, eTag, checksum, timestamp), frequency, groupFrequency, parseVersion) =>
        Feed(id, sourceId, downloadId, siteUrl.map(new URI(_)), title, MetaData(lastModified, eTag, checksum, timestamp), frequency, groupFrequency, parseVersion)
    }, { f: Feed =>
      Some((f.id, f.sourceId, f.downloadId, f.siteUrl.map(_.toString), f.title, (f.metaData.lastModified, f.metaData.eTag,
        f.metaData.checksum, f.metaData.timestamp), f.frequency, f.groupFrequency, f.parseVersion))
    })
}

object feeds extends TableQuery(new FeedsTable(_)) {
  val returningId = this returning this.map(_.id.get)

  val byId = this.findBy(_.id)
  val byDownloadId = this.findBy(_.downloadId)

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

