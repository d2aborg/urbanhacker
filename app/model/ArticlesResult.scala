package model

case class ArticlesResult(articles: Seq[CachedArticle], permalink: Option[Permalink], requested: Option[Permalink], nextPage: Option[Int])
