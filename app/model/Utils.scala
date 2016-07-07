package model

import java.net.URI
import java.time.{OffsetDateTime, ZoneOffset, ZonedDateTime}
import java.time.format._
import java.time.format.DateTimeFormatter._
import java.time.temporal.ChronoField._
import java.util.Locale

import org.apache.commons.lang3.StringEscapeUtils._
import play.api.Logger

import scala.util.{Failure, Success, Try}
import scala.xml.NodeSeq

/**
  * Helper utilities for feeds.
  */
object Utils {
  def crop(text: String, maxLength: Int): String = {
    if (text.length > maxLength)
      text.substring(0, maxLength) + "..."
    else
      text
  }

  def parseOffsetDateTime(dts: String, dtf: DateTimeFormatter): Option[OffsetDateTime] = {
    nonEmpty(dts).flatMap { dts =>
      Try(ZonedDateTime parse(dts, dtf)) match {
        case Success(ts) => Some(ts withZoneSameInstant ZoneOffset.UTC toOffsetDateTime)
        case Failure(e) =>
          Logger.warn("Failed to parse date: " + dts, e)
          None
      }
    }
  }

  val internetDateTimeFormats = Seq(
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

  def parseInternetDateTime(dts: String): Either[OffsetDateTime, Seq[DateTimeParseException]] = {
    def parseInternetDateTime(dtf: DateTimeFormatter, desc: String): Either[OffsetDateTime, DateTimeParseException] = {
      try {
        val parsed = ZonedDateTime parse(dts replaceFirst("^(Mon|Tue|Wed|Thu|Fri|Sat|Sun), ", ""), dtf)
        Left(parsed withZoneSameInstant ZoneOffset.UTC toOffsetDateTime)
      } catch {
        case e: DateTimeParseException => Right(new DateTimeParseException(desc + ": " + e.getMessage, e.getParsedString, e.getErrorIndex, e))
      }
    }

    val parsedOrFailures = for ((formatDesc, dateTimeFormat) <- internetDateTimeFormats) yield parseInternetDateTime(dateTimeFormat, formatDesc)

    for (parsedOrFailure <- parsedOrFailures; parsed <- parsedOrFailure.left.toOption)
      return Left(parsed)

    Right(for (parsedOrFailure <- parsedOrFailures; failure <- parsedOrFailure.right.toOption) yield failure)
  }

  def unescapeOption(ns: NodeSeq): Option[String] = Some(unescape(ns text)).filter(_.nonEmpty)

  def unescape(ns: NodeSeq): String = unescape(ns text)

  def unescape(content: String): String = unescapeHtml4(content replaceAll("&nbsp;", " ")) trim

  def nonEmpty(s: String): Option[String] = Some(s).filter(_.nonEmpty)

  def nonEmpty(s: String, alt: String): String = nonEmpty(s) getOrElse alt

  def parseURI(uri: String): Option[URI] =
    nonEmpty(uri) flatMap { uri =>
      val maybeURI = Try(new URI(uri))
      if (maybeURI isFailure)
        Logger.warn("Failed to parse URI: " + uri, maybeURI.failed.get)
      maybeURI.toOption
    }
}
