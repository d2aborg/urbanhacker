package model

import java.io.StringReader
import java.net.URI
import java.time._
import java.time.temporal.ChronoUnit
import java.util.regex.Pattern.quote

import com.markatta.timeforscala._
import com.optimaize.langdetect.i18n.LdLocale
import com.optimaize.langdetect.ngram.NgramExtractors
import com.optimaize.langdetect.profiles.LanguageProfileReader
import com.optimaize.langdetect.text.{CommonTextObjectFactories, TextObjectFactory}
import com.optimaize.langdetect.{LanguageDetector, LanguageDetectorBuilder}
import model.Article.{LANGUAGE_DETECTOR, TEXT_OBJECT_FACTORY}
import model.Utils._
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import play.api.Logger
import services.SlickPgPostgresDriver.api._
import slick.lifted.{QueryBase, TableQuery, Tag}
import slick.model.ForeignKeyAction.{Cascade, Restrict}

import scala.math.max
import scala.util.Try
import scala.xml._
import scala.xml.parsing.NoBindingFactoryAdapter

case class Article(id: Option[Long], sourceId: Long, feedId: Option[Long], title: String, link: URI, commentsLink: Option[URI],
                   pubDate: ZonedDateTime, imageSource: Option[URI], text: String) extends Ordered[Article] {
  override def compare(that: Article): Int = -pubDate.compareTo(that.pubDate)

  def since(implicit now: ZonedDateTime): String = {
    val years = ChronoUnit.YEARS.between(pubDate, now)
    val months = ChronoUnit.MONTHS.between(pubDate, now)
    val days = ChronoUnit.DAYS.between(pubDate, now)
    val hours = ChronoUnit.HOURS.between(pubDate, now)
    val minutes = ChronoUnit.MINUTES.between(pubDate, now)
    val seconds = ChronoUnit.SECONDS.between(pubDate, now)

    // compatibility with moment.js in JS layer: http://momentjs.com/docs/#/displaying/fromnow/
    if (seconds >= 0 && seconds < 45) "a few seconds ago"
    else if (seconds >= 45 && seconds < 90) "a minute ago"
    else if (seconds >= 90 && minutes < 45) max(minutes, 2) + " minutes ago"
    else if (minutes >= 45 && minutes < 90) "an hour ago"
    else if (minutes >= 90 && hours < 22) max(hours, 2) + " hours ago"
    else if (hours >= 22 && hours < 36) "a day ago"
    else if (hours >= 36 && days < 25) max(days, 2) + " days ago"
    else if (days >= 25 && days < 45) "a month ago"
    else if (days >= 45 && days < 345) max(months, 2) + " months ago"
    else if (days >= 345 && days < 545) "a year ago"
    else if (days >= 545) max(years, 2) + " years ago"
    else "not yet"
  }

  val maxSummaryLength = 250

  def croppedText: String = crop(text, maxSummaryLength)

  def same(a: Article): Boolean = link == a.link || (title == a.title && imageSource == a.imageSource && text == a.text)

  def similar(a: Article): Boolean = link == a.link || title == a.title

  def isEnglish: Boolean = {
    text.isEmpty || {
      val titleAndText = title + ": " + text
      val lang = LANGUAGE_DETECTOR.detect(TEXT_OBJECT_FACTORY.forText(titleAndText))
      !lang.isPresent || lang.asSet.contains(LdLocale.fromString("en"))
    }
  }
}

object Article {
  val LANGUAGE_DETECTOR: LanguageDetector = LanguageDetectorBuilder.create(NgramExtractors.standard)
    .withProfiles(new LanguageProfileReader().readAllBuiltIn)
    .build

  val TEXT_OBJECT_FACTORY: TextObjectFactory = CommonTextObjectFactories.forDetectingShortCleanText()

  def rss(source: FeedSource, feed: Feed, item: Node): Option[Article] = {
    val title = stripTitle(unescape(item \ "title"), feed)
    val link = unescapeOption(item \ "link")
    if (link isEmpty) {
      Logger.warn("Couldn't find a valid link among: " + (item \ "link"))
      return None
    }

    val commentsLink = unescapeOption(item \ "comments")

    def dateOption: Option[ZonedDateTime] = {
      val dateNodes = item \ "date" ++ item \ "pubDate"

      for {
        dateNode <- dateNodes
        nodeText <- unescapeOption(dateNode)
      } {
        val parsed = parseInternetDateTime(nodeText)
        if (parsed.isRight)
          return Some(parsed.right.get)

        Logger.warn("Failed to parse date of '" + title + "' in feed '" + feed.title.getOrElse(source.url) + "': " + parsed.left.get)
      }

      Logger.warn("No parseable date in '" + title + "' in feed '" + feed.title.getOrElse(source.url) + "' among: " + dateNodes)
      None
    }

    val parsedDescription = parseHtmlContent(unescape(item \ "description"))
    val selfLinks = Seq(link, feed.siteUrl.map(_.toString)).flatten
    val strippedDescription = stripDescription(parsedDescription, selfLinks: _*)
    val maybeImgSrc = imageSource(strippedDescription, link get)
    val strippedText = stripText(strippedDescription, title, feed)

    dateOption.map(date =>
      Article(None, source.id, feed.id, title, new URI(link get), commentsLink.map(new URI(_)), date, maybeImgSrc, strippedText)) filter { _.isEnglish }
  }

  def stripText(content: NodeSeq, title: String, feed: Feed): String =
    stripText(content, title, feed.title.getOrElse(""))

  def stripText(content: NodeSeq, title: String, feedTitle: String): String =
    content.text.trim.replaceAll("\\p{Cntrl}", " ").replaceAll("\\p{So}", " ").replaceAll("\\s+", " ")
      .replaceAll(quote(s" The post $title appeared first on $feedTitle.") + "$", "") // WIRED etc
      .replaceAll(quote(s"$title is a post from $feedTitle") + "$", "") // CSS-Tricks

  def stripTitle(title: String, feed: Feed): String = {
    title.replaceFirst(" - " + quote(feed.title.getOrElse("")) + "$", "")
  }

  def atom(source: FeedSource, feed: Feed, entry: Node): Option[Article] = {
    val title = stripTitle(unescape(entry \ "title"), feed)
    val link = (entry \ "link").filter(n => n \@ "rel" == "alternate" || (n \@ "rel" isEmpty)) \@ "href"
    if (link isEmpty) {
      Logger.warn("Failed to parse feed entry link from: " + (entry \ "link"))
      return None
    }
    val commentsLink = nonEmpty((entry \ "link").filter(n => n \@ "rel" == "replies" && n \@ "type" == "text/html") \@ "href")

    val date = parseInternetDateTime(unescape(entry \ "updated"))
    if (date.isLeft) {
      Logger.warn("Failed to parse date of '" + title + "' in feed '" + feed.title.getOrElse(source.url) + "': " + date.right.get)
      return None
    }

    val parsedDescription = parseHtmlContent(unescapeOption(entry \ "content").orElse(unescapeOption(entry \ "summary")).getOrElse(""))

    val selfLinks = Seq(Some(link), feed.siteUrl.map(_.toString)).flatten
    val strippedDescription = stripDescription(parsedDescription, selfLinks: _*)
    val maybeImgSrc = imageSource(strippedDescription, link)
    val strippedText = stripText(strippedDescription, title, feed)

    Some(Article(None, source.id, feed.id, title, new URI(link), commentsLink.map(new URI(_)), date.right get, maybeImgSrc, strippedText)) filter { _.isEnglish }
  }

  def tooSmall(img: Node): Boolean =
    Try((img \@ "width").toInt).toOption.exists(_ <= 20) || Try((img \@ "height").toInt).toOption.exists(_ <= 20)

  def imageSource(content: NodeSeq, link: String): Option[URI] = {
    val bannedImgSrcs = List(
      "http://www.infoq.com/styles/i/logo_bigger.jpg",
      "http://www.techmeme.com/img/pml.png",
      "http://assets.feedblitz.com/i/fblike20.png",
      "http://assets.feedblitz.com/i/googleplus20.png",
      "http://assets.feedblitz.com/i/linkedin20.png",
      "http://assets.feedblitz.com/i/twitter20.png",
      "http://assets.feedblitz.com/i/rss20.png")
    val imgs = content \\ "img" filterNot (img => bannedImgSrcs contains (img \@ "src")) filterNot tooSmall
    imgs.headOption map (_ \@ "src") flatMap { src =>
      try {
        Some(new URI(link).resolve(src))
      } catch {
        case t: Throwable =>
          Logger.warn("Failed to resolve image '" + src + "' against URL: '" + link + "'", t)
          None
      }
    }
  }

  def parseHtmlContent(htmlText: String): NodeSeq = {
    val inputSource = new InputSource(new StringReader(htmlText))

    val parser = new SAXFactoryImpl().newSAXParser
    val adapter = new NoBindingFactoryAdapter
    val content = adapter.loadXML(inputSource, parser)
    enterSingleNode(content)
  }

  def enterSingleNode(nodes: NodeSeq): NodeSeq = {
    if (nodes.size == 1 && nodes.head.isInstanceOf[Elem] && nodes.head.label != "a")
      enterSingleNode(nodes.head.asInstanceOf[Elem].child)
    else nodes
  }

  def stripDescription(nodes: NodeSeq, selfLinks: String*): NodeSeq = {
    nodes filterNot { n =>
      n.label == "iframe" ||
        n.label == "div" && n \@ "class" == "share_submission" ||
        n.label == "div" && n \@ "class" == "feedflare" ||
        n.label == "div" && n \@ "class" == "shares" ||
        n.label == "div" && n \@ "class" == "blogger-post-footer" ||
        n.label == "a" && (n text) == "Read More" ||
        n.label == "a" && (n text) == "Comments" ||
        n.label == "a" && (n text) == "Read more..." ||
        n.label == "p" && (n \ "a" text) == "Read more..." ||
        n.label == "p" && (n \ "a" text) == "Read more of this story" ||
        n.label == "p" && (n \@ "class" == "medium-feed-link") ||
        n.label == "span" && List("[link]", "[comments]").contains(n \ "a" text) ||
        n.label == "table" && (n \ "tr" \ "td" \ "div" \ "img").exists(_ \@ "src" == "http://statisches.auslieferung.commindo-media-ressourcen.de/advertisement.gif")
    } map {
      case <br/> => Text(" ")
      case n => n
    }
  }
}

class ArticlesTable(tag: Tag) extends Table[Article](tag, "articles") {
  def id = column[Option[Long]]("id", O.PrimaryKey, O.AutoInc)

  def sourceId = column[Long]("source_id")

  def source = foreignKey("source_fk", sourceId, sources)(_.id, onUpdate = Restrict, onDelete = Cascade)

  def feedId = column[Option[Long]]("feed_id")

  def feed = foreignKey("feed_fk", feedId, feeds)(_.id, onUpdate = Restrict, onDelete = Cascade)

  def title = column[String]("title")

  def link = column[String]("link")

  def commentsLink = column[Option[String]]("comments_link")

  def pubDate = column[ZonedDateTime]("pub_date")

  def imageSource = column[Option[String]]("image_source")

  def text = column[String]("text")

  override def * =
    (id, sourceId, feedId, title, link, commentsLink, pubDate, imageSource, text).shaped <> ( {
      case (id, sourceId, feedId, title, link, commentsLink, pubDate, imageSource, text) =>
        Article(id, sourceId, feedId, title, new URI(link), commentsLink.map(new URI(_)), pubDate, imageSource.map(new URI(_)), text)
    }, { a: Article =>
      Some((a.id, a.sourceId, a.feedId, a.title, a.link.toString, a.commentsLink.map(_.toString), a.pubDate,
        a.imageSource.map(_.toString), a.text))
    })

  def linkIndex = index("articles_link_idx", link)

  def titleIndex = index("articles_title_idx", title)

  def similar(articles: Seq[Article]) = {
    articles.map(a => link === a.link.toString || title === a.title).reduce(_ || _)
  }
}

object articles extends TableQuery(new ArticlesTable(_)) {
  val returningId = this returning this.map(_.id.get)

  val byId = this.findBy(_.id)

  def newerThan(section: String,
                historicTimestamp: ZonedDateTime,
                link: Rep[String],
                pubDate: Rep[ZonedDateTime],
                feedTimestamp: Rep[ZonedDateTime]): Query[ArticlesTable, Article, Seq] =
    for {
      s <- sources.bySection(section)
      f <- feeds if f.sourceId === s.id && f.timestamp <= historicTimestamp
      a <- this if a.feedId === f.id && a.link === link && (a.pubDate > pubDate || (a.pubDate === pubDate && f.timestamp > feedTimestamp))
    } yield a

  def historicalInGroup(cachedFeed: CachedFeed) = {
    for {
      s <- sources.inSameGroup(cachedFeed.source)
      f <- feeds if f.sourceId === s.id && f.timestamp <= cachedFeed.record.metaData.timestamp
      a <- this if a.feedId === f.id
    } yield a
  }

  def lastTenInGroup(cachedFeed: CachedFeed) = {
    historicalInGroup(cachedFeed).sortBy(_.pubDate.desc).take(10)
  }

  def similar(cachedFeed: CachedFeed, articlez: Seq[Article]): Seq[QueryBase[Seq[Article]]] = {
    for (articleGroup <- articlez.grouped(50).toSeq) yield {
      for (a <- articles.historicalInGroup(cachedFeed) if a.similar(articleGroup)) yield a
    }
  }

  def byIds(ids: Traversable[Long]) = {
    for (a <- this if a.id inSet ids) yield a
  }
}

case class CachedArticle(source: FeedSource, feed: Feed, record: Article) extends Ordered[CachedArticle] {
  override def compare(that: CachedArticle): Int = record.compare(that.record)
}
