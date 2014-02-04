import play.Project._

name := "ssdemo-scala"

version := "1.0"

libraryDependencies ++= Seq(
    "ws.securesocial" %% "securesocial" % "master-SNAPSHOT"
)

playScalaSettings
