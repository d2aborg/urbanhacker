package model

import org.apache.commons.lang3.StringEscapeUtils._

import scala.xml.NodeSeq

/**
  * Helper utilities for feeds.
  */
object Utils {
  def unescapeOption(ns: NodeSeq): Option[String] = Some(unescape(ns text)).filter(_.nonEmpty)

  def unescape(ns: NodeSeq): String = unescape(ns text)

  def unescape(content: String): String = unescapeHtml4(content replaceAll("&nbsp;", " ")) trim

  def nonEmpty(s: String): Option[String] = Some(s).filter(_.nonEmpty)
  def nonEmpty(s: String, alt: String): String = Some(s).filter(_.nonEmpty).getOrElse(alt)
}
