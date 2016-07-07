package model

import java.net.URI
import java.time.temporal.ChronoUnit

import model.Utils.{nonEmpty, parseURI, unescape}
import play.api.Logger

import scala.xml.{Node, NodeSeq}

case class Feed(source: FeedSource, metaData: MetaData, articles: Seq[Article]) {
  def articlesPerSecond: Double = {
    val mostRecent = articles take 10
    val secondsBetween = ChronoUnit.SECONDS.between(mostRecent.last.date, mostRecent.head.date)
    mostRecent.size.toDouble / secondsBetween
  }
}

object Feed {
  def parse(source: FeedSource, download: XmlDownload): Option[Feed] = {
    val rssChannel = download.xml \\ "channel"
    if (rssChannel.nonEmpty) {
      return Some(Feed.rss(source, download, rssChannel(0)) { source =>
        for (item <- download.xml \\ "item")
          yield Article.rss(source, item(0))
      })
    }

    if (download.xml.label == "feed") {
      return Some(Feed.atom(source, download, download.xml(0)) { source =>
        for (entry <- download.xml \\ "entry")
          yield Article.atom(source, entry(0))
      })
    }

    Logger.warn("Couldn't find RSS or ATOM feed in XML: " + download.xml)
    None
  }

  def rss(source: FeedSource, download: XmlDownload, channel: Node)
         (articles: (FeedSource) => Seq[Option[Article]]): Feed = {
    val title = nonEmpty(cleanTitle(channel \ "title"))

    val siteUrl = nonEmpty(unescape(channel \ "link")).flatMap(parseURI)

    val appliedSource = source.copy(siteUrl = source.siteUrl.orElse(siteUrl), title = source.title.orElse(title))

    new Feed(appliedSource, download.metaData, articles(appliedSource).flatten)
  }

  def atom(source: FeedSource, download: XmlDownload, feedRoot: Node)
          (articles: (FeedSource) => Seq[Option[Article]]): Feed = {
    val title = nonEmpty(cleanTitle(if ((feedRoot \ "title" text) nonEmpty) feedRoot \ "title" else feedRoot \ "id"))

    val siteUrl = nonEmpty((feedRoot \ "link").filterNot { l =>
      l \@ "rel" == "self" ||
        l \@ "rel" == "hub" ||
        l \@ "type" == "application/atom+xml"
    } \@ "href").map(new URI(_))

    if (siteUrl isEmpty)
      Logger.info("Found no viable link in feed: " + title + ", among: " + (feedRoot \ "link"))

    val appliedSource = source.copy(siteUrl = source.siteUrl.orElse(siteUrl), title = source.title.orElse(title))

    new Feed(appliedSource, download.metaData, articles(appliedSource).flatten)
  }

  def cleanTitle(title: NodeSeq): String = unescape(title)
    .replaceAll(" RSS Feed$", "")
    .replaceAll("^Latest blogs for ", "")
    .replaceAll(": the front page of the internet$", "")
    .replaceAll(" – Latest Articles$", "")
    .replaceAll(" — Medium$", "")
    .replaceAll(": The Full Feed$", "")
}
