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
  "org.postgresql" % "postgresql" % "9.4.1212.jre7",
  "com.typesafe.play" %% "play-slick" % "2.1.0-M1",
  "com.typesafe.play" %% "play-slick-evolutions" % "2.1.0-M1",
  "com.typesafe.slick" %% "slick" % "3.2.0-RC1",
  "com.typesafe.slick" %% "slick-hikaricp" % "3.2.0-RC1",
  "com.zaxxer" % "HikariCP" % "2.4.7",
  "com.github.tminglei" %% "slick-pg" % "0.15.0-M4",
  "com.github.tminglei" %% "slick-pg_date2" % "0.15.0-M2",
  "com.markatta" %% "timeforscala" % "1.2",
  "com.ibm.icu" % "icu4j" % "58.2"
)

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"
resolvers += Resolver.url("Typesafe Ivy releases", url("https://repo.typesafe.com/typesafe/ivy-releases"))(Resolver.ivyStylePatterns)

fork in run := false
