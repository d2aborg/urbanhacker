name := """urbanhacker"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala, LauncherJarPlugin)

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  cache,
  ws,
  evolutions,
  "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.1" % Test,
  "org.scala-lang.modules" %% "scala-pickling" % "0.10.1",
  "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2.1",
  "com.optimaize.languagedetector" % "language-detector" % "0.5",
//  "org.postgresql" % "postgresql" % "9.4.1208", -- can't use due to bug in SQL driver
  "org.postgresql" % "postgresql" % "9.4-1201-jdbc41",
  "com.typesafe.play" %% "play-slick" % "2.0.0",
  "com.typesafe.play" %% "play-slick-evolutions" % "2.0.0",
  "com.zaxxer" % "HikariCP" % "2.4.5",
  "com.github.tminglei" %% "slick-pg" % "0.14.2",
  "com.github.tminglei" %% "slick-pg_date2" % "0.14.2",
  "com.markatta" %% "timeforscala" % "1.2"
)

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

fork in run := false

