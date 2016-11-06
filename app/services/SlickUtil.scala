package services

import java.net.URI
import java.sql.Timestamp
import java.time.{ZoneOffset, ZonedDateTime}

import slick.driver.MySQLDriver.api._

object SlickUtil {
  implicit val zonedDateTimeMapper = MappedColumnType.base[ZonedDateTime, Timestamp](
    zdt => Timestamp.from(zdt.toInstant),
    ts => ZonedDateTime.ofInstant(ts.toInstant, ZoneOffset.UTC)
  )

  implicit val uriMapper = MappedColumnType.base[URI, String](
    uri => uri.toString,
    s => new URI(s)
  )
}
