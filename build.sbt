// https://typelevel.org/sbt-typelevel/faq.html#what-is-a-base-version-anyway
ThisBuild / tlBaseVersion := "0.0" // your current series x.y

ThisBuild / organization := "io.github.quafadas"
ThisBuild / organizationName := "quafadas"
ThisBuild / startYear := Some(2023)
ThisBuild / licenses := Seq(License.Apache2)
ThisBuild / developers := List(
  // your GitHub handle and name
  tlGitHubDev("quafadas", "Simon Parten")
)

// publish to s01.oss.sonatype.org (set to true to publish to oss.sonatype.org instead)
ThisBuild / tlSonatypeUseLegacyHost := false

// publish website from this branch
ThisBuild / tlSitePublishBranch := Some("main")
ThisBuild / scalaVersion := "3.3.1"

lazy val root = tlCrossRootProject.aggregate(core)

lazy val core = crossProject(
  JSPlatform,
  JVMPlatform,
  NativePlatform
)
  .crossType(CrossType.Full)
  .settings(
    description := "Dyanmic extensions for slash",
    libraryDependencies ++= Seq("ai.dragonfly" %% "slash" % "0.1")
  )
  .jvmSettings(
  )
  .jsSettings()
  .nativeSettings()

lazy val docs = project.in(file("site")).enablePlugins(TypelevelSitePlugin)

lazy val tests = crossProject(
    JVMPlatform,
    JSPlatform,
    NativePlatform
  )
  .in(file("tests"))
  .enablePlugins(NoPublishPlugin)
  .dependsOn(core)
  .settings(
    name := "tests",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.0.0-M8" % Test
  )
