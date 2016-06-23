package services

import java.io._
import java.net.URI
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.regex.{Matcher, Pattern}

import com.google.inject.Inject
import model._
import play.api.Logger
import play.api.db.slick.DatabaseConfigProvider
import slick.driver.JdbcProfile

import scala.io.Source
import scala.pickling.Defaults._
import scala.pickling.json._

trait FeedStore {
  def load(url: URI): Seq[TextDownload]

  def save(download: TextDownload): Boolean

  def shortUrl(url: URI): String = url.toString
    .replaceFirst("^https?://(www\\.)?", "")
    .replaceFirst("^([^/]+)\\.com/", "$1/")
    .replaceAll("[^a-zA-Z0-9_\\-.]", "_")
    .replaceAll("_+", "_")
    .replaceFirst("^_", "")
    .replaceFirst("_$", "")

  def logInfo(msg: String, url: URI): Unit = Logger.info(mkLogMsg(msg, shortUrl(url)))

  def logWarn(msg: String, url: URI, e: Exception): Unit = Logger.warn(mkLogMsg(msg, shortUrl(url)), e)

  def logWarn(msg: String, which: String, e: Exception): Unit = Logger.warn(mkLogMsg(msg, which), e)

  def mkLogMsg(msg: String, which: String): String = msg + ": " + which
}

class DBFeedStore @Inject()(dbConfigProvider: DatabaseConfigProvider) extends FeedStore {
  val dbConfig = dbConfigProvider.get[JdbcProfile]

  def load(url: URI): Seq[TextDownload] = {
    Seq.empty
  }

  def save(download: TextDownload): Boolean = {
    true
  }
}

class FileFeedStore extends FeedStore {
  val directory = new File("feeds")
  val filenameTimestampPattern = DateTimeFormatter.ofPattern("uuuu-MM-dd_HHmmss")
  val fileExtension: String = ".xml"
  val metaDataExtension: String = ".json"

  def load(url: URI): Seq[TextDownload] = {
    files(url).flatMap(load(url, _))
  }

  def load(url: URI, file: File): Option[TextDownload] = {
    val timestamp = fileTimestamp(url, file)
    try {
      val content = Source.fromFile(file).mkString
      val metaData = Source.fromFile(metaDataFile(url, timestamp)).mkString.unpickle[MetaData]

      Some(TextDownload(metaData, content))
    } catch {
      case e: Exception =>
        logWarn("Failed to load from disk", file.getName, e)
        None
    }
  }

  def save(download: TextDownload): Boolean = {
    if (!directory.isDirectory)
      return true

    try {
      val targetFile = file(download.metaData.uri, download.metaData.timestamp)
      val targetMetaDataFile = metaDataFile(download.metaData.uri, download.metaData.timestamp)

      if (!targetFile.getParentFile.isDirectory && !targetFile.getParentFile.mkdirs) {
        logInfo("Failed to create target directory " + targetFile.getParentFile + " for", download.metaData.uri)
        return false
      }

      if (!targetMetaDataFile.getParentFile.isDirectory && !targetMetaDataFile.getParentFile.mkdirs) {
        logInfo("Failed to create metadata target directory " + targetMetaDataFile.getParentFile + " for", download.metaData.uri)
        return false
      }

      val writer = new FileWriter(targetFile)
      try {
        writer.write(download.content)
        writeMetaData(targetMetaDataFile, download.metaData)
        true
      } catch {
        case e: Exception =>
          targetFile.delete
          targetMetaDataFile.delete
          throw e
      } finally {
        writer.close()
      }
    } catch {
      case e: Exception =>
        logWarn("Failed to save to disk", download.metaData.url, e)
        false
    }
  }

  def writeMetaData(file: File, metaData: MetaData): Unit = {
    val metaDataWriter = new BufferedWriter(new FileWriter(file))
    try {
      metaDataWriter.write(metaData.pickle.value)
      metaDataWriter.newLine()
    } finally {
      metaDataWriter.close()
    }
  }

  def fileTimestamp(url: URI, file: File): LocalDateTime =
    LocalDateTime parse(filenameMatcher(url, file).get group 1, filenameTimestampPattern)

  def dir(url: URI): File =
    new File(directory, dirname(url))

  def file(url: URI, timestamp: LocalDateTime): File =
    new File(dir(url), filename(timestamp) + fileExtension)

  def metaDataFile(url: URI, timestamp: LocalDateTime): File =
    new File(dir(url), filename(timestamp) + metaDataExtension)

  def dirname(url: URI): String = shortUrl(url)

  def filename(timestamp: LocalDateTime): String = timestamp format filenameTimestampPattern

  def files(url: URI): Array[File] = {
    Option(dir(url).listFiles(fileFilter(url))).map(_.sorted).getOrElse(Array[File]())
  }

  def fileFilter(url: URI): FileFilter = new FileFilter {
    override def accept(file: File): Boolean = filenameMatcher(url, file) isDefined
  }

  def filenameMatcher(url: URI, file: File): Option[Matcher] =
    filenamePattern(url) matcher file.getName match {
      case matcher if matcher.matches => Some(matcher)
      case _ => None
    }

  def filenamePattern(url: URI): Pattern =
    Pattern compile "^(\\d{4}-\\d{2}-\\d{2}_\\d{6})" + Pattern.quote(fileExtension) + "$"
}

