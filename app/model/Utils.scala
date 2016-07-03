package model

import java.net.URI

import org.apache.commons.lang3.StringEscapeUtils._
import play.api.Logger

import scala.util.Try
import scala.xml.NodeSeq

/**
  * Helper utilities for feeds.
  */
object Utils {
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
