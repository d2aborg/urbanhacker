package model

import java.io.StringReader
import java.net.URI
import java.time._
import java.time.temporal.ChronoUnit
import java.util.regex.Pattern.quote

import com.optimaize.langdetect.i18n.LdLocale
import com.optimaize.langdetect.ngram.NgramExtractors
import com.optimaize.langdetect.profiles.LanguageProfileReader
import com.optimaize.langdetect.text.{CommonTextObjectFactories, TextObjectFactory}
import com.optimaize.langdetect.{LanguageDetector, LanguageDetectorBuilder}
import model.Utils.{nonEmpty, parseInternetDateTime, unescape, unescapeOption}
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import play.api.Logger

import scala.math.max
import scala.xml._
import scala.xml.parsing.NoBindingFactoryAdapter

case class Article(source: FeedSource, title: String, link: String, commentsLink: Option[String],
                   date: OffsetDateTime, image: Option[URI], text: String) extends Ordered[Article] {
  val maxSummaryLength = 250

  override def compare(that: Article): Int = -date.compareTo(that.date)

  def frecency(frequency: Double, timestamp: OffsetDateTime): Double =
    Math.pow(age(timestamp), 4) * frequency

  def age(timestamp: OffsetDateTime): Long =
    ChronoUnit.SECONDS.between(date, timestamp)

  def since(implicit now: OffsetDateTime): String = {
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

  def rss(source: FeedSource, item: Node): Option[Article] = {
    val title = stripTitle(unescape(item \ "title"), source)
    val link = unescapeOption(item \ "link")
    if (link isEmpty) {
      Logger.warn("Couldn't find a valid link among: " + (item \ "link"))
      return None
    }

    val commentsLink = unescapeOption(item \ "comments")

    def dateOption: Option[OffsetDateTime] = {
      val dateNodes = item \ "date" ++ item \ "pubDate"

      for (dateNode <- dateNodes; nodeText <- unescapeOption(dateNode)) {
        val parsed = parseInternetDateTime(nodeText)
        if (parsed.isLeft)
          return parsed.left.toOption

        Logger.warn("Failed to parse date of '" + title + "' in feed '" + source.title.getOrElse(source.url) + "': " + parsed.right.get)
      }

      Logger.warn("No parseable date in '" + title + "' in feed '" + source.title.getOrElse(source.url) + "' among: " + dateNodes)
      None
    }

    val parsedDescription = parseHtmlContent(unescape(item \ "description"))
    val selfLinks = Seq(link, source.siteUrl.map(_.toString)).flatten
    val strippedDescription = stripDescription(parsedDescription, selfLinks: _*)
    val maybeImgSrc = imageSource(strippedDescription, link get)
    val strippedText = stripText(strippedDescription, title, source)

    dateOption.map(new Article(source, title, link get, commentsLink, _, maybeImgSrc, strippedText)) filter isEnglish
  }

  def stripText(content: NodeSeq, title: String, source: FeedSource): String = {
    val feedTitle = source.title.getOrElse("")
    content.text.trim.replaceAll("\\s+", " ")
      .replaceAll(quote(s" The post $title appeared first on $feedTitle.") + "$", "") // WIRED
      .replaceAll(quote(s"$title is a post from $feedTitle") + "$", "") // CSS-Tricks
  }

  def stripTitle(title: String, source: FeedSource): String = {
    title.replaceFirst(" - " + quote(source.title.getOrElse("")) + "$", "")
  }

  /*
  <link rel="replies" type="application/atom+xml" href="http://keaplogik.blogspot.com/feeds/7850892430692034846/comments/default" title="Post Comments" xmlns="http://www.w3.org/2005/Atom" xmlns:openSearch="http://a9.com/-/spec/opensearchrss/1.0/" xmlns:blogger="http://schemas.google.com/blogger/2008" xmlns:georss="http://www.georss.org/georss" xmlns:gd="http://schemas.google.com/g/2005" xmlns:thr="http://purl.org/syndication/thread/1.0"/>
  <link rel="replies" type="text/html" href="http://keaplogik.blogspot.com/2016/01/java-base64-url-safe-encoding.html#comment-form" title="0 Comments" xmlns="http://www.w3.org/2005/Atom" xmlns:openSearch="http://a9.com/-/spec/opensearchrss/1.0/" xmlns:blogger="http://schemas.google.com/blogger/2008" xmlns:georss="http://www.georss.org/georss" xmlns:gd="http://schemas.google.com/g/2005" xmlns:thr="http://purl.org/syndication/thread/1.0"/>
  <link rel="edit" type="application/atom+xml" href="http://www.blogger.com/feeds/3047562558564532519/posts/default/7850892430692034846" xmlns="http://www.w3.org/2005/Atom" xmlns:openSearch="http://a9.com/-/spec/opensearchrss/1.0/" xmlns:blogger="http://schemas.google.com/blogger/2008" xmlns:georss="http://www.georss.org/georss" xmlns:gd="http://schemas.google.com/g/2005" xmlns:thr="http://purl.org/syndication/thread/1.0"/>
  <link rel="self" type="application/atom+xml" href="http://www.blogger.com/feeds/3047562558564532519/posts/default/7850892430692034846" xmlns="http://www.w3.org/2005/Atom" xmlns:openSearch="http://a9.com/-/spec/opensearchrss/1.0/" xmlns:blogger="http://schemas.google.com/blogger/2008" xmlns:georss="http://www.georss.org/georss" xmlns:gd="http://schemas.google.com/g/2005" xmlns:thr="http://purl.org/syndication/thread/1.0"/>
  <link rel="alternate" type="text/html" href="http://keaplogik.blogspot.com/2016/01/java-base64-url-safe-encoding.html" title="Java Base64 URL Safe Encoding" xmlns="http://www.w3.org/2005/Atom" xmlns:openSearch="http://a9.com/-/spec/opensearchrss/1.0/" xmlns:blogger="http://schemas.google.com/blogger/2008" xmlns:georss="http://www.georss.org/georss" xmlns:gd="http://schemas.google.com/g/2005" xmlns:thr="http://purl.org/syndication/thread/1.0"/>
     */
  def atom(source: FeedSource, entry: Node): Option[Article] = {
    val title = stripTitle(unescape(entry \ "title"), source)
    val link = (entry \ "link").filter(n => n \@ "rel" == "alternate" || (n \@ "rel" isEmpty)) \@ "href"
    if (link isEmpty) {
      Logger.warn("Failed to parse feed entry link from: " + (entry \ "link"))
      return None
    }
    val commentsLink = nonEmpty((entry \ "link").filter(n => n \@ "rel" == "replies" && n \@ "type" == "text/html") \@ "href")

    val date = parseInternetDateTime(unescape(entry \ "updated"))
    if (date.isRight) {
      Logger.warn("Failed to parse date of '" + title + "' in feed '" + source.title.getOrElse(source.url) + "': " + date.right.get)
      return None
    }

    val parsedDescription = parseHtmlContent(unescapeOption(entry \ "content").orElse(unescapeOption(entry \ "summary")).getOrElse(""))

    val selfLinks = Seq(Some(link), source.siteUrl.map(_.toString)).flatten
    val strippedDescription = stripDescription(parsedDescription, selfLinks: _*)
    val maybeImgSrc = imageSource(strippedDescription, link)
    val strippedText = stripText(strippedDescription, title, source)

    Some(new Article(source, title, link, commentsLink, date.left get, maybeImgSrc, strippedText)) filter isEnglish
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
}