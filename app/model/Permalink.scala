package model

import java.time.{ZoneOffset, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.util.Locale

import model.Utils.parseZonedDateTime

case class Permalink(timestamp: ZonedDateTime, page: Int, nextPage: Option[Int] = None, requested: Option[ZonedDateTime] = None) {
  def urlTimestamp: String = timestamp.withZoneSameInstant(ZoneOffset.UTC).format(Permalink.urlFormatter)
  def viewTimestamp: String = timestamp.withZoneSameInstant(ZoneOffset.UTC).format(Permalink.viewFormatter)

  def prevPage: Option[Int] = Some(page - 1).filter(_ >= 1)
}

object Permalink {
  val pageSize: Int = 15

  val urlFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss.SSSXX", Locale.US)
  val viewFormatter: DateTimeFormatter = DateTimeFormatter.RFC_1123_DATE_TIME

  def parseUrlTimestamp(dts: String): Option[ZonedDateTime] = parseZonedDateTime(dts, urlFormatter)
}