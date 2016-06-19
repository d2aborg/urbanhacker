name := """urbanhacker"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws,
  "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.1" % Test,
  "org.scala-lang.modules" %% "scala-pickling" % "0.10.1",
  "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2.1",
  "com.optimaize.languagedetector" % "language-detector" % "0.5"
)

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

fork in run := false