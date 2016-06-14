package model

import java.time.LocalDateTime

import org.scalatest._

class ArticleTest extends FlatSpec with Matchers {
  "parseInternetDate" should "parse incorrect weekdays" in  {
    Article.parseInternetDate("Thu, 06 Apr 2016 15:00:00 +0300") shouldBe Left(LocalDateTime.parse("2016-04-06T14:00:00"))
  }
  it should "parse RFC 1123 with incorrect weekdays" in  {
    Article.parseInternetDate("Thu, 06 Apr 2016 15:00:00 +0300") shouldBe Left(LocalDateTime.parse("2016-04-06T14:00:00"))
  }
  it should "parse RFC 1123 with inset offset" in  {
    Article.parseInternetDate("Tue, 4 Feb 2009 15:50:00+0300") shouldBe Left(LocalDateTime.parse("2009-02-04T13:50:00"))
  }
  it should "parse ISO-8601" in  {
    Article.parseInternetDate("2016-04-08T00:00:00-07:00") shouldBe Left(LocalDateTime.parse("2016-04-08T09:00:00"))
  }
}
