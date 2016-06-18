package model

import java.net.URI
import java.time.temporal.ChronoUnit

import model.Utils.unescape
import play.api.Logger
import services.FeedSource

import scala.collection.SortedSet
import scala.xml.{Elem, Node, NodeSeq}

/**
  *
  */
case class Feed(feedUrl: String, siteUrl: String, title: String, var articles: SortedSet[Article] = SortedSet()) {
  def articlesPerSecond: Double = {
    val mostRecent = articles take 10
    val secondsBetween = ChronoUnit.SECONDS.between(mostRecent.last.date, mostRecent.head.date)
    mostRecent.size.toDouble / secondsBetween
  }

  def favicon: String =
    if (siteUrl.length > 0)
      new URI(siteUrl).resolve("/favicon.ico").toString
    else
      ""
}

object Feed {
  def parse(source: FeedSource, root: Elem): Option[Feed] = {
    val rssChannel = root \\ "channel"
    if (rssChannel.nonEmpty) {
      val feed = Feed.rss(source, rssChannel(0))
      feed.articles = {
        for (item <- root \\ "item")
          yield Article.rss(feed, item(0))
      }.flatten.to[Set].to[SortedSet]
      return Some(feed)
    }

    if (root.label == "feed") {
      val atomFeedElem = root
      val feed = Feed.atom(source, atomFeedElem(0))
      feed.articles = {
        for (entry <- root \\ "entry")
          yield Article.atom(feed, entry(0))
      }.flatten.to[Set].to[SortedSet]
      return Some(feed)
    }

    Logger.warn("Couldn't find RSS or ATOM feed in XML: " + root)
    None
  }

  def rss(source: FeedSource, channel: Node): Feed = {
    val name = cleanTitle(channel \ "title")

    val link = unescape(channel \ "link")

    new Feed(source.feedUrl, source.siteUrl getOrElse link, source.title getOrElse name)
  }

  def atom(source: FeedSource, feedRoot: Node): Feed = {
    val name = cleanTitle(if ((feedRoot \ "title" text) nonEmpty) feedRoot \ "title" else feedRoot \ "id")

    val link = (feedRoot \ "link").filterNot { l =>
      l \@ "rel" == "self" ||
        l \@ "rel" == "hub" ||
        l \@ "type" == "application/atom+xml"
    } \@ "href"

    if (link isEmpty)
      Logger.info("Found no viable link in feed: " + name + ", among: " + (feedRoot \ "link"))

    new Feed(source.feedUrl, source.siteUrl getOrElse link, source.title getOrElse name)
  }

  def cleanTitle(title: NodeSeq): String = unescape(title)
    .replaceAll(" RSS Feed$", "")
    .replaceAll("^Latest blogs for ", "")
    .replaceAll(": the front page of the internet$", "")
    .replaceAll(" – Latest Articles$", "")
    .replaceAll(" — Medium$", "")
    .replaceAll(": The Full Feed$", "")
}