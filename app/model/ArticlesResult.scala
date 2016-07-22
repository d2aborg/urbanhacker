package model

case class ArticlesResult(articles: Seq[CachedArticle], permalink: ResolvedPermalink, nextPage: Option[Int])
