package model

import java.time.{ZoneOffset, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.util.Locale

import model.Permalink.{urlFormatter, viewFormatter}
import model.Utils.parseZonedDateTime

case class Permalink(timestamp: ZonedDateTime, pageNum: Int) {
  def urlTimestamp: String = timestampUTC.format(urlFormatter)
  def viewTimestamp: String = timestampUTC.format(viewFormatter)

  def prevPage: Option[Int] = Some(pageNum - 1).filter(_ >= 1)

  def timestampUTC: ZonedDateTime = timestamp.withZoneSameInstant(ZoneOffset.UTC)
}

object Permalink {
  val pageSize: Int = 15

  val urlFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss.SSSXX", Locale.US)
  val viewFormatter: DateTimeFormatter = DateTimeFormatter.RFC_1123_DATE_TIME

  def parseUrlTimestamp(dts: String): Option[ZonedDateTime] = parseZonedDateTime(dts, urlFormatter)
}

case class ResolvedPermalink(resolved: Option[Permalink], requested: Option[Permalink] = None) {
  def isConsistent: Boolean =
    requested.map(_.timestampUTC).forall {
      resolved.map(_.timestampUTC).contains(_)
    }
}
