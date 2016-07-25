package model

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.util.Locale

import model.Permalink.{urlFormatter, viewFormatter}
import model.Utils._

case class Permalink(timestamp: ZonedDateTime, pageNum: Int) {
  val pageSize: Int = 15

  def urlTimestamp: String = timestamp.UTC.format(urlFormatter)
  def viewTimestamp: String = timestamp.UTC.format(viewFormatter)

  def prevPage: Option[Int] = Some(pageNum - 1).filter(_ >= 1)
}

object Permalink {
  val urlFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss.SSSXX", Locale.US)
  val viewFormatter: DateTimeFormatter = DateTimeFormatter.RFC_1123_DATE_TIME

  def parseUrlTimestamp(dts: String): Option[ZonedDateTime] = parseZonedDateTime(dts, urlFormatter)
}
