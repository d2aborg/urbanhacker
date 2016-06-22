package model

import java.io.StringReader
import java.net.URI
import java.time.format.DateTimeFormatter.{ISO_OFFSET_DATE_TIME, RFC_1123_DATE_TIME}
import java.time.format._
import java.time.temporal.ChronoField._
import java.time.temporal.ChronoUnit
import java.time.{LocalDateTime, _}
import java.util.Locale
import java.util.regex.Pattern.quote

import com.optimaize.langdetect.i18n.LdLocale
import com.optimaize.langdetect.ngram.NgramExtractors
import com.optimaize.langdetect.profiles.LanguageProfileReader
import com.optimaize.langdetect.text.{CommonTextObjectFactories, TextObjectFactory}
import com.optimaize.langdetect.{LanguageDetector, LanguageDetectorBuilder}
import model.Utils.{nonEmpty, unescape, unescapeOption}
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import play.api.Logger

import scala.collection.SortedSet
import scala.math.max
import scala.xml._
import scala.xml.parsing.NoBindingFactoryAdapter

/**
  *
  */
class Article(val feed: Feed, val title: String, val link: String, val commentsLink: Option[String],
              val date: LocalDateTime, val image: Option[URI], val text: String) extends Ordered[Article] {
  val maxSummaryLength = 250

  override def compare(that: Article): Int = -date.compareTo(that.date)

  def frecency(frequency: Double, timestamp: LocalDateTime): Double =
    Math.pow(age(timestamp), 4) * frequency

  def age(timestamp: LocalDateTime): Long =
    ChronoUnit.SECONDS.between(date, timestamp)

  def dateUTC = date.atZone(ZoneId.systemDefault).toOffsetDateTime

  def since(implicit now: LocalDateTime): String = {
    val years = ChronoUnit.YEARS.between(date, now)
    val months = ChronoUnit.MONTHS.between(date, now)
    val days = ChronoUnit.DAYS.between(date, now)
    val hours = ChronoUnit.HOURS.between(date, now)
    val minutes = ChronoUnit.MINUTES.between(date, now)
    val seconds = ChronoUnit.SECONDS.between(date, now)

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

  def croppedText: String = {
    def crop(text: String, maxLength: Int): String = {
      if (text.length > maxLength)
        text.substring(0, maxLength) + "..."
      else
        text
    }

    crop(text, maxSummaryLength)
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Article]

  override def equals(other: Any): Boolean = other match {
    case that: Article =>
      (that canEqual this) &&
        link == that.link
    case _ => false
  }

  override def hashCode(): Int = Seq(link).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
}

object Article {
  //build language detector:
  val languageDetector: LanguageDetector = LanguageDetectorBuilder.create(NgramExtractors.standard)
    .withProfiles(new LanguageProfileReader().readAllBuiltIn)
    .build

  //create a text object factory
  val textObjectFactory: TextObjectFactory = CommonTextObjectFactories.forDetectingShortCleanText()

  def isEnglish(article: Article): Boolean = {
    article.text.isEmpty || {
      val text = article.title + ": " + article.text
      val lang = languageDetector.detect(textObjectFactory.forText(text))
      !lang.isPresent || lang.asSet.contains(LdLocale.fromString("en"))
    }
  }

  def uniqueSorted(articles: Iterable[Article]): SortedSet[Article] = articles.toSet[Article].to[SortedSet]

  def rss(feed: Feed, item: Node): Option[Article] = {
    val title = stripTitle(unescape(item \ "title"), feed)
    val link = unescapeOption(item \ "link")
    if (link isEmpty) {
      Logger.warn("Couldn't find a valid link among: " + (item \ "link"))
      return None
    }

    val commentsLink = unescapeOption(item \ "comments")

    def dateOption: Option[LocalDateTime] = {
      val dateNodes = item \ "date" ++ item \ "pubDate"

      for (dateNode <- dateNodes; nodeText <- unescapeOption(dateNode)) {
        val parsed = parseInternetDate(nodeText)
        if (parsed.isLeft)
          return Some(parsed.left.get)

        Logger.warn("Failed to parse date of '" + title + "' in feed '" + feed.title + "': " + parsed.right.get)
      }

      Logger.warn("No parseable date in '" + title + "' in feed '" + feed.title + "' among: " + dateNodes)
      None
    }

    val parsedDescription = parseHtmlContent(unescape(item \ "description"))
    val strippedDescription = stripDescription(parsedDescription, link get, feed.siteUrl)
    val maybeImgSrc = imageSource(strippedDescription, link get)
    val strippedText = stripText(strippedDescription, title, feed)

    dateOption.map(new Article(feed, title, link get, commentsLink, _, maybeImgSrc, strippedText)) filter isEnglish
  }

  def stripText(content: NodeSeq, title: String, feed: Feed): String = {
    content.text.trim.replaceAll("\\s+", " ")
      .replaceAll(quote(s" The post $title appeared first on ${feed.title}.") + "$", "") // WIRED
      .replaceAll(quote(s"$title is a post from ${feed.title}") + "$", "") // CSS-Tricks
  }

  def stripTitle(title: String, feed: Feed): String = {
    title.replaceFirst(" - " + quote(feed.title) + "$", "")
  }

  /*
  <link rel="replies" type="application/atom+xml" href="http://keaplogik.blogspot.com/feeds/7850892430692034846/comments/default" title="Post Comments" xmlns="http://www.w3.org/2005/Atom" xmlns:openSearch="http://a9.com/-/spec/opensearchrss/1.0/" xmlns:blogger="http://schemas.google.com/blogger/2008" xmlns:georss="http://www.georss.org/georss" xmlns:gd="http://schemas.google.com/g/2005" xmlns:thr="http://purl.org/syndication/thread/1.0"/>
  <link rel="replies" type="text/html" href="http://keaplogik.blogspot.com/2016/01/java-base64-url-safe-encoding.html#comment-form" title="0 Comments" xmlns="http://www.w3.org/2005/Atom" xmlns:openSearch="http://a9.com/-/spec/opensearchrss/1.0/" xmlns:blogger="http://schemas.google.com/blogger/2008" xmlns:georss="http://www.georss.org/georss" xmlns:gd="http://schemas.google.com/g/2005" xmlns:thr="http://purl.org/syndication/thread/1.0"/>
  <link rel="edit" type="application/atom+xml" href="http://www.blogger.com/feeds/3047562558564532519/posts/default/7850892430692034846" xmlns="http://www.w3.org/2005/Atom" xmlns:openSearch="http://a9.com/-/spec/opensearchrss/1.0/" xmlns:blogger="http://schemas.google.com/blogger/2008" xmlns:georss="http://www.georss.org/georss" xmlns:gd="http://schemas.google.com/g/2005" xmlns:thr="http://purl.org/syndication/thread/1.0"/>
  <link rel="self" type="application/atom+xml" href="http://www.blogger.com/feeds/3047562558564532519/posts/default/7850892430692034846" xmlns="http://www.w3.org/2005/Atom" xmlns:openSearch="http://a9.com/-/spec/opensearchrss/1.0/" xmlns:blogger="http://schemas.google.com/blogger/2008" xmlns:georss="http://www.georss.org/georss" xmlns:gd="http://schemas.google.com/g/2005" xmlns:thr="http://purl.org/syndication/thread/1.0"/>
  <link rel="alternate" type="text/html" href="http://keaplogik.blogspot.com/2016/01/java-base64-url-safe-encoding.html" title="Java Base64 URL Safe Encoding" xmlns="http://www.w3.org/2005/Atom" xmlns:openSearch="http://a9.com/-/spec/opensearchrss/1.0/" xmlns:blogger="http://schemas.google.com/blogger/2008" xmlns:georss="http://www.georss.org/georss" xmlns:gd="http://schemas.google.com/g/2005" xmlns:thr="http://purl.org/syndication/thread/1.0"/>
     */
  def atom(feed: Feed, entry: Node): Option[Article] = {
    val title = stripTitle(unescape(entry \ "title"), feed)
    val link = (entry \ "link").filter(n => n \@ "rel" == "alternate" || (n \@ "rel" isEmpty)) \@ "href"
    if (link isEmpty) {
      Logger.warn("Failed to parse feed entry link from: " + (entry \ "link"))
      return None
    }
    val commentsLink = nonEmpty((entry \ "link").filter(n => n \@ "rel" == "replies" && n \@ "type" == "text/html") \@ "href")

    val date = parseInternetDate(unescape(entry \ "updated"))
    if (date.isRight) {
      Logger.warn("Failed to parse date of '" + title + "' in feed '" + feed.title + "': " + date.right.get)
      return None
    }

    val parsedDescription = parseHtmlContent(unescapeOption(entry \ "content").orElse(unescapeOption(entry \ "summary")).getOrElse(""))

    val strippedDescription = stripDescription(parsedDescription, link, feed.siteUrl)
    val maybeImgSrc = imageSource(strippedDescription, link)
    val strippedText = stripText(strippedDescription, title, feed)

    Some(new Article(feed, title, link, commentsLink, date.left get, maybeImgSrc, strippedText)) filter isEnglish
  }

  def tooSmall(img: Node): Boolean = {
    try {
      if ((img \@ "width").toInt <= 20)
        return true
    } catch {
      case e: NumberFormatException =>
    }

    try {
      if ((img \@ "height").toInt <= 20)
        return true
    } catch {
      case e: NumberFormatException =>
    }

    false
  }

  def imageSource(content: NodeSeq, link: String): Option[URI] = {
    val bannedImgSrcs = List(
      "http://www.infoq.com/styles/i/logo_bigger.jpg",
      "http://www.techmeme.com/img/pml.png",
      "http://assets.feedblitz.com/i/fblike20.png",
      "http://assets.feedblitz.com/i/googleplus20.png",
      "http://assets.feedblitz.com/i/linkedin20.png",
      "http://assets.feedblitz.com/i/twitter20.png",
      "http://assets.feedblitz.com/i/rss20.png")
    val imgs = content \\ "img" filterNot (img => bannedImgSrcs contains (img \@ "src")) filterNot (tooSmall(_))
    imgs.headOption map (_ \@ "src") flatMap { src =>
      try {
        Some(new URI(link).resolve(src))
      } catch {
        case t: Throwable =>
          Logger.warn("Failed to resolve image '" + src + "' against URL: '" + link + "'")
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
    if (nodes.size == 1 && nodes(0).isInstanceOf[Elem] && nodes(0).label != "a")
      enterSingleNode(nodes(0).asInstanceOf[Elem].child)
    else nodes
  }

  def stripDescription(nodes: NodeSeq, selfLinks: String*): NodeSeq = {
    nodes filterNot { n =>
      n.label == "iframe" ||
        n.label == "div" && n \@ "class" == "share_submission" ||
        n.label == "div" && n \@ "class" == "feedflare" ||
        n.label == "div" && n \@ "class" == "shares" ||
        n.label == "div" && n \@ "class" == "blogger-post-footer" ||
//        n.label == "figure" ||
        n.label == "a" && (n text) == "Read More" ||
        n.label == "a" && (n text) == "Comments" ||
        n.label == "a" && (n text) == "Read more..." ||
        n.label == "p" && (n \ "a" text) == "Read more..." ||
        n.label == "p" && (n \ "a" text) == "Read more of this story" ||
        n.label == "p" && (n \@ "class" == "medium-feed-link") ||
        n.label == "span" && List("[link]", "[comments]").contains(n \ "a" text) ||
        n.label == "table" && (n \ "tr" \ "td" \ "div" \ "img").exists(_ \@ "src" == "http://statisches.auslieferung.commindo-media-ressourcen.de/advertisement.gif")
    } map {
      case <br /> => Text(" ")
      case n => n
    }
  }

      /*
<table width="650">
<tr>
<td width="650">
<div style="width:650px;">
<img src="http://statisches.auslieferung.commindo-media-ressourcen.de/advertisement.gif" alt="" border="0" /><br />
<a href="http://auslieferung.commindo-media-ressourcen.de/random.php?mode=target&collection=smashing-rss&position=1" target="_blank">
<img src="http://auslieferung.commindo-media-ressourcen.de/random.php?mode=image&collection=smashing-rss&position=1" border="0" alt="" />
</a>&nbsp;
<a href="http://auslieferung.commindo-media-ressourcen.de/random.php?mode=target&collection=smashing-rss&position=2" target="_blank">
<img src="http://auslieferung.commindo-media-ressourcen.de/random.php?mode=image&collection=smashing-rss&position=2" border="0" alt="" />
</a>&nbsp;
<a href="http://auslieferung.commindo-media-ressourcen.de/random.php?mode=target&collection=smashing-rss&position=3" target="_blank">
<img src="http://auslieferung.commindo-media-ressourcen.de/random.php?mode=image&collection=smashing-rss&position=3" border="0" alt="" />
</a>
</div>
</td>
</tr>
</table>
      */

      // <span><a href="https://vimeo.com/169289500">[link]</a></span> &#32; <span><a href="https://www.reddit.com/r/videos/comments/4meq4l/hoe_lee_shit/">[comments]</a></span> </td></tr></table>
      /*
      <div class="shares">
        <div class="icons">
          <span class="label">Share:</span>
          <a href="https://twitter.com/intent/tweet?url=" title="Share on Twitter">
            <img src="/t_mini-a.png"></a>
          <a href="https://facebook.com/sharer.php?u=" title="Share on Facebook">
            <img src="/fb-icon-20.png"></a>
          <a href="https://plus.google.com/share?url=" title="Share on Google Plus">
            <img src="/gplus-16.png"></a>
        </div>
        <div class="comment">if you found this article useful, please share it. I appreciate the feedback and encouragement</div>
      </div>
      <div class="clear"></div>
       */

  val dateFormats = Seq(
    // 2016-04-08T00:00:00-07:00
    ("ISO-8601", ISO_OFFSET_DATE_TIME),
    // Thu, 14 Apr 2015 18:00:00 +0300
    ("RFC 1123", RFC_1123_DATE_TIME),
    // 22 Jul 2013 14:55:00 EST
    ("Zoned RFC 1123", new DateTimeFormatterBuilder()
      .parseCaseInsensitive
      .parseLenient
      .appendValue(DAY_OF_MONTH, 1, 2, SignStyle.NOT_NEGATIVE)
      .appendLiteral(' ').appendText(MONTH_OF_YEAR, TextStyle.SHORT)
      .appendLiteral(' ').appendValue(YEAR, 4)
      .appendLiteral(' ').appendValue(HOUR_OF_DAY, 2)
      .appendLiteral(':').appendValue(MINUTE_OF_HOUR, 2)
      .optionalStart.appendLiteral(':').appendValue(SECOND_OF_MINUTE, 2).optionalEnd
      .optionalStart.appendLiteral(' ').optionalEnd.appendZoneText(TextStyle.SHORT)
      .toFormatter(Locale.US)),
    // Tue, 4 Feb 2009 15:50:00+0300
    ("Inset Offset RFC 1123", new DateTimeFormatterBuilder()
      .parseCaseInsensitive
      .parseLenient
      .appendValue(DAY_OF_MONTH, 1, 2, SignStyle.NOT_NEGATIVE)
      .appendLiteral(' ').appendText(MONTH_OF_YEAR, TextStyle.SHORT)
      .appendLiteral(' ').appendValue(YEAR, 4)
      .appendLiteral(' ').appendValue(HOUR_OF_DAY, 2)
      .appendLiteral(':').appendValue(MINUTE_OF_HOUR, 2)
      .optionalStart.appendLiteral(':').appendValue(SECOND_OF_MINUTE, 2).optionalEnd
      .optionalStart.appendLiteral(' ').optionalEnd.appendOffset("+HHMM", "GMT")
      .toFormatter(Locale.US)),
    // 2015-09-29 14:56:55 UTC
    ("ISO With Spaces", DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm:ss zzz", Locale.US)))

  def parseInternetDate(date: String): Either[LocalDateTime, Seq[DateTimeParseException]] = {
    val parsedOrFailures = for ((formatDesc, dateFormat) <- dateFormats) yield try {
      val parsed = ZonedDateTime parse(date replaceFirst("^(Mon|Tue|Wed|Thu|Fri|Sat|Sun), ", ""), dateFormat)
      Left(parsed withZoneSameInstant ZoneId.systemDefault() toLocalDateTime)
    } catch {
      case e: DateTimeParseException => Right(new DateTimeParseException(formatDesc + ": " + e.getMessage, e.getParsedString, e.getErrorIndex, e))
    }

    for (parsedOrFailure <- parsedOrFailures; parsed <- parsedOrFailure.left.toOption)
      return Left(parsed)

    Right(for (parsedOrFailure <- parsedOrFailures; failure <- parsedOrFailure.right.toOption) yield failure)
  }
}