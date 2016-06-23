package model

import java.net.URI
import java.time.LocalDateTime
import java.time.temporal.ChronoUnit

import model.Utils.{nonEmpty, unescape}
import play.api.Logger

import scala.collection.SortedSet
import scala.xml.{Elem, Node, NodeSeq}

case class Feed(source: FeedSource, metaData: MetaData, previous: Option[Feed], articles: SortedSet[Article]) {
  def articlesPerSecond: Double = {
    val mostRecent = articles take 10
    val secondsBetween = ChronoUnit.SECONDS.between(mostRecent.last.date, mostRecent.head.date)
    mostRecent.size.toDouble / secondsBetween
  }

  def url: URI = source.url

  def byUrl: (URI, Feed) = source.url -> this

  def allArticles: SortedSet[Article] = Article.uniqueSorted(allArticles2)

  def allArticles2: SortedSet[Article] =
    articles ++ previous.map(_.allArticles2).getOrElse(Nil)

  def latestAt(timestamp: LocalDateTime): Option[Feed] =
    if (newerThan(timestamp))
      previous.flatMap(_.latestAt(timestamp))
    else
      Some(this)

  def newerThan(timestamp: LocalDateTime): Boolean = metaData.timestamp.isAfter(timestamp)
}

object Feed {
  def parse(source: FeedSource, download: XmlDownload, previous: Option[Feed]): Option[Feed] = {
    val rssChannel = download.xml \\ "channel"
    if (rssChannel.nonEmpty) {
      return Some(Feed.rss(source, download, rssChannel(0), previous) { source =>
        for (item <- download.xml \\ "item")
          yield Article.rss(source, item(0))
      })
    }

    if (download.xml.label == "feed") {
      return Some(Feed.atom(source, download, download.xml(0), previous) { source =>
        for (entry <- download.xml \\ "entry")
          yield Article.atom(source, entry(0))
      })
    }

    Logger.warn("Couldn't find RSS or ATOM feed in XML: " + download.xml)
    None
  }

  def rss(source: FeedSource, download: XmlDownload, channel: Node, previous: Option[Feed])
         (articles: (FeedSource) => Iterable[Option[Article]]): Feed = {
    val title = nonEmpty(cleanTitle(channel \ "title"))

    val siteUrl = nonEmpty(unescape(channel \ "link")).map(new URI(_))

    val appliedSource: FeedSource = source(siteUrl, title)
    new Feed(appliedSource, download.metaData, previous, Article.uniqueSorted(articles(appliedSource).flatten))
  }

  def atom(source: FeedSource, download: XmlDownload, feedRoot: Node, previous: Option[Feed])
          (articles: (FeedSource) => Iterable[Option[Article]]): Feed = {
    val title = nonEmpty(cleanTitle(if ((feedRoot \ "title" text) nonEmpty) feedRoot \ "title" else feedRoot \ "id"))

    val siteUrl = nonEmpty((feedRoot \ "link").filterNot { l =>
      l \@ "rel" == "self" ||
        l \@ "rel" == "hub" ||
        l \@ "type" == "application/atom+xml"
    } \@ "href").map(new URI(_))

    if (siteUrl isEmpty)
      Logger.info("Found no viable link in feed: " + title + ", among: " + (feedRoot \ "link"))

    val appliedSource: FeedSource = source(siteUrl, title)
    new Feed(appliedSource, download.metaData, previous, Article.uniqueSorted(articles(appliedSource).flatten))
  }

  def cleanTitle(title: NodeSeq): String = unescape(title)
    .replaceAll(" RSS Feed$", "")
    .replaceAll("^Latest blogs for ", "")
    .replaceAll(": the front page of the internet$", "")
    .replaceAll(" – Latest Articles$", "")
    .replaceAll(" — Medium$", "")
    .replaceAll(": The Full Feed$", "")
}

case class MetaData(url: String, lastModified: Option[String], eTag: Option[String], checksum: String,
                    timestamp: LocalDateTime) {
  def uri: URI = new URI(url)
}

case class TextDownload(metaData: MetaData, content: String)

case class XmlDownload(metaData: MetaData, xml: Elem)

case class FeedSource(url: URI, group: String, siteUrl: Option[URI] = None, title: Option[String] = None) {
  def apply(siteUrl: Option[URI], title: Option[String]): FeedSource =
    FeedSource(url, group, this.siteUrl.orElse(siteUrl), this.title.orElse(title))

  def favicon: String =
    siteUrl.map(_.resolve("/favicon.ico").toString).getOrElse("")
}
