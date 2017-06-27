package model

import java.net.URI
import java.sql.Timestamp
import java.time.format.DateTimeFormatter._
import java.time.format._
import java.time.temporal.ChronoField._
import java.time.{OffsetDateTime, ZoneOffset, ZonedDateTime}
import java.util.Locale

import org.apache.commons.lang3.StringEscapeUtils._
import com.markatta.timeforscala._
import play.api.Logger
import slick.dbio.{DBIOAction, Effect, NoStream}

import scala.concurrent.{ExecutionContext, Future}
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

  def toOffsetDateTime(ts: Timestamp): OffsetDateTime = ts.toInstant.atOffset(ZoneOffset.UTC)

  def toSqlTimestamp(odt: OffsetDateTime): Timestamp = new Timestamp(odt.toInstant.toEpochMilli)

  def toSqlTimestamp(zdt: ZonedDateTime): Timestamp = new Timestamp(zdt.toInstant.toEpochMilli)

  def parseOffsetDateTime(dts: String, dtf: DateTimeFormatter): Option[OffsetDateTime] =
    parseZonedDateTime(dts, dtf).map(_ withZoneSameInstant ZoneOffset.UTC toOffsetDateTime)

  def parseZonedDateTime(dts: String, dtf: DateTimeFormatter): Option[ZonedDateTime] =
    nonEmpty(dts).flatMap { dts =>
      Try(ZonedDateTime parse(dts, dtf)) match {
        case Success(ts) => Some(ts)
        case Failure(e) =>
          Logger.warn("Failed to parse date: " + dts, e)
          None
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
    ("ISO With Spaces", java.time.format.DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm:ss zzz", Locale.US)))

  def parseInternetDateTime(dts: String): Either[Seq[DateTimeParseException], ZonedDateTime] = {
    def parseInternetDateTime(dtf: DateTimeFormatter, desc: String): Either[DateTimeParseException, ZonedDateTime] = {
      try {
        val parsed = ZonedDateTime parse(dts replaceFirst("^(Mon|Tue|Wed|Thu|Fri|Sat|Sun), ", ""), dtf) withZoneSameInstant ZoneOffset.UTC
        Right(parsed)
      } catch {
        case e: DateTimeParseException => Left(new DateTimeParseException(desc + ": " + e.getMessage, e.getParsedString, e.getErrorIndex, e))
      }
    }

    val parsedOrFailures = for ((formatDesc, dateTimeFormat) <- internetDateTimeFormats) yield parseInternetDateTime(dateTimeFormat, formatDesc)

    for (parsedOrFailure <- parsedOrFailures; parsed <- parsedOrFailure.right)
      return Right(parsed)

    Left(for (parsedOrFailure <- parsedOrFailures; failure <- parsedOrFailure.left.toOption) yield failure)
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

  def min(d1: ZonedDateTime, d2: ZonedDateTime): ZonedDateTime = if (d1 > d2) d1 else d2

  class Tappable[A](x: A) {
    def tap[U](action: (A) => U): A = {
      action(x)
      x
    }
  }

  implicit def any2Tappable[A](x: A): Tappable[A] = new Tappable[A](x)

  class TappableTraversable[A, M[B] <: Traversable[B]](x: M[A]) {
    def tapeach[U](action: (A) => U): M[A] = {
      x.foreach(action)
      x
    }
  }

  implicit def traversable2TappableTraversable[A, M[B] <: Traversable[B]](x: M[A]): TappableTraversable[A, M] =
    new TappableTraversable[A, M](x)

  class NonUTCZonedDateTime(dt: ZonedDateTime) {
    def UTC: ZonedDateTime = dt.withZoneSameInstant(ZoneOffset.UTC)
  }

  implicit def zdt2nonUTCZdt(dt: ZonedDateTime): NonUTCZonedDateTime = new NonUTCZonedDateTime(dt)
}

object Futures {
  def sequence[A](maybeFuture: Option[Future[A]])
                 (implicit ec: ExecutionContext): Future[Option[A]] = maybeFuture match {
    case Some(future) => future.map(Some(_))
    case None => Future.successful(None)
  }

  def traverse[A, B](option: Option[A])
                    (action: A => Future[B])
                    (implicit ec: ExecutionContext): Future[Option[B]] = {
    sequence(option.map(action))
  }
}

object DBIOA {
  def sequence[R](maybeAction: Option[DBIOAction[R, NoStream, Effect.Read]])(implicit executor: ExecutionContext): DBIOAction[Option[R], NoStream, Effect.Read] = {
    maybeAction.fold(DBIOAction.successful(Option.empty[R]): DBIOAction[Option[R], NoStream, Effect.Read]) {
      _.map(Some(_))
    }
  }

  def traverse[A, B](option: Option[A])
                    (action: A => DBIOAction[B, NoStream, Effect.Read])
                    (implicit ec: ExecutionContext): DBIOAction[Option[B], NoStream, Effect.Read] = {
    sequence(option.map(action))
  }
}