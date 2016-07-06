package model

import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter
import java.util.Locale

import model.Utils.parseOffsetDateTime

case class Permalink(timestamp: OffsetDateTime, page: Int, nextPage: Option[Int] = None, requested: Option[OffsetDateTime] = None) {
  def urlTimestamp: String = timestamp.format(Permalink.urlFormatter)
  def viewTimestamp: String = timestamp.format(Permalink.viewFormatter)

  def prevPage: Option[Int] = Some(page - 1).filter(_ >= 1)
}

object Permalink {
  val pageSize: Int = 15

  val urlFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss.SSSXX", Locale.US)
  val viewFormatter: DateTimeFormatter = DateTimeFormatter.RFC_1123_DATE_TIME

  def parseUrlTimestamp(dts: String): Option[OffsetDateTime] = parseOffsetDateTime(dts, urlFormatter)
}