name := "PrimeGenTesting"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies += "com.jsuereth" %% "scala-arm" % "2.0"
libraryDependencies += "org.typelevel" %% "spire" % "0.14.1"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

lazy val root = (project in file(".")).settings(
  name := "thyme-test",
  version := "0.1.0",
  scalaVersion := "2.12.0",
  libraryDependencies += "com.github.ichoran" %% "thyme" % "0.1.2-SNAPSHOT"
)