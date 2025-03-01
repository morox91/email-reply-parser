name := "email-reply-parser"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.1"

organization := "net.methvin"

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

libraryDependencies ++= Seq(
  "com.sun.mail" % "javax.mail" % "1.5.3",
  "org.specs2" %% "specs2" % "2.4.3" % "test"
)
