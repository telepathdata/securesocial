name := "SecureSocial"

version := "master-SNAPSHOT"

libraryDependencies ++= Seq(
  cache,
  "commons-codec" % "commons-codec" % "1.9",
  "com.typesafe" %% "scalalogging-slf4j" % "1.0.1",
  "com.typesafe" %% "play-plugins-util" % "2.2.0",
  "com.typesafe" %% "play-plugins-mailer" % "2.2.0",
  "org.mindrot" % "jbcrypt" % "0.3m"
)

resolvers ++= Seq(
  Resolver.typesafeRepo("releases")
)

organization := "ws.securesocial"

organizationName := "SecureSocial"

organizationHomepage := Some(new URL("http://www.securesocial.ws")) 

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (version.value.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

startYear := Some(2012)

description := "An authentication module for Play Framework applications supporting OAuth, OAuth2, OpenID, Username/Password and custom authentication schemes."

licenses := Seq("The Apache Software License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

homepage := Some(url("http://www.securesocial.ws"))

pomExtra := (
  <scm>
    <url>https://github.com/jaliss/securesocial</url>
    <connection>scm:git:git@github.com:jaliss/securesocial.git</connection>
    <developerConnection>scm:git:https://github.com/jaliss/securesocial.git</developerConnection>
  </scm>
  <developers>
    <developer>
      <id>jaliss</id>
      <name>Jorge Aliss</name>
      <email>jaliss [at] gmail.com</email>
      <url>https://twitter.com/jaliss</url>
    </developer>
  </developers>
)

scalacOptions := Seq("-feature", "-deprecation",  "-target:jvm-1.7")

javacOptions ++= Seq("-source", "1.7", "-target", "1.7")

