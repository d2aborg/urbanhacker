name := "urbanhacker"
organization := "org.d2ab"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala, LauncherJarPlugin)

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  ws,
  evolutions,
  guice,
  "org.scalatestplus.play" %% "scalatestplus-play" % "3.0.0" % Test,
  "org.postgresql" % "postgresql" % "42.1.4",
  "com.typesafe.play" %% "play-slick" % "3.0.0",
  "com.typesafe.play" %% "play-slick-evolutions" % "3.0.0",
  "com.github.tminglei" %% "slick-pg" % "0.15.1",
  "com.markatta" %% "timeforscala" % "1.4",
  "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2.1",
  "com.optimaize.languagedetector" % "language-detector" % "0.6",
  "com.ibm.icu" % "icu4j" % "60.1"
)

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"
resolvers += Resolver.url("Typesafe Ivy releases", url("https://repo.typesafe.com/typesafe/ivy-releases"))(Resolver.ivyStylePatterns)
