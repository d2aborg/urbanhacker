name := "urbanhacker"
organization := "org.d2ab"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala, LauncherJarPlugin)

scalaVersion := "2.12.2"

libraryDependencies ++= Seq(
  ws,
  evolutions,
  guice,
  "org.scalatestplus.play" %% "scalatestplus-play" % "3.0.0" % Test,
//  "org.scala-lang.modules" %% "scala-pickling" % "0.10.1",
  "org.postgresql" % "postgresql" % "42.1.1",
  "com.zaxxer" % "HikariCP" % "2.4.7",
  "com.typesafe.play" %% "play-slick" % "3.0.0",
  "com.typesafe.play" %% "play-slick-evolutions" % "3.0.0",
//  "com.typesafe.slick" %% "slick" % "3.2.0-RC1",
//  "com.typesafe.slick" %% "slick-hikaricp" % "3.2.0-RC1",
  "com.github.tminglei" %% "slick-pg" % "0.15.1",
//  "com.github.tminglei" %% "slick-pg_date2" % "0.15.1",
  "com.markatta" %% "timeforscala" % "1.4",
  "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2.1",
  "com.optimaize.languagedetector" % "language-detector" % "0.5",
  "com.ibm.icu" % "icu4j" % "58.2"
)

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"
resolvers += Resolver.url("Typesafe Ivy releases", url("https://repo.typesafe.com/typesafe/ivy-releases"))(Resolver.ivyStylePatterns)
