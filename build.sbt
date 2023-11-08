import laika.helium.Helium
import laika.helium.config.*
import laika.markdown.github.GitHubFlavor
import laika.parse.code.SyntaxHighlighting
import laika.ast.Path.Root
import laika.theme.config.Color
import laika.ast.LengthUnit.*
import laika.ast.*

import java.time.OffsetDateTime

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

ThisBuild / tlCiDocCheck := false

// publish to s01.oss.sonatype.org (set to true to publish to oss.sonatype.org instead)
ThisBuild / tlSonatypeUseLegacyHost := false

// publish website from this branch
ThisBuild / tlSitePublishBranch := Some("main")
ThisBuild / scalaVersion := "3.4.0-RC1-bin-20231106-f61026d-NIGHTLY"

lazy val root = tlCrossRootProject.aggregate(core, tests)

lazy val core = crossProject(
  JSPlatform,
  JVMPlatform
  // NativePlatform
).crossType(CrossType.Full)
  .settings(
    name := "vecxt",
    description := """High performance extensions for numeric workloads for
      - Array[Double] on JVM
      - Array[Double] on native.
      - Float64Array on JS"""
  )
  .jvmSettings(
  )
  .jsSettings()
//.nativeSettings()

lazy val docs = project
  .in(file("site"))
  .enablePlugins(TypelevelSitePlugin)
  .dependsOn(core.jvm)
  .settings(
    laikaConfig ~= { _.withRawContent },
    laikaExtensions := Seq(
      GitHubFlavor,
      SyntaxHighlighting
    ),
    tlSiteHelium := {
      Helium.defaults.site
        .metadata(
          title = Some("vecxt"),
          language = Some("en"),
          description = Some("vecxt"),
          authors = Seq("Simon Parten"),
          datePublished = Some(OffsetDateTime.now)
        )
        .site
        .topNavigationBar(
          homeLink = IconLink.internal(laika.ast.Path(List("index.md")), HeliumIcon.home),
          navLinks = Seq(IconLink.external("https://github.com/Quafadas/vecxt", HeliumIcon.github))
        )
      Helium.defaults.site
        .externalJS(
          url = "https://cdn.jsdelivr.net/npm/vega@5"
        )
        .site
        .externalJS(
          url = "https://cdn.jsdelivr.net/npm/vega-lite@5"
        )
        .site
        .externalJS(
          url = "https://cdn.jsdelivr.net/npm/vega-embed@6"
        )
        .site
        .autoLinkJS()
    }
  )

lazy val tests = crossProject(
  JVMPlatform,
  JSPlatform
  // NativePlatform
)
  .in(file("tests"))
  .enablePlugins(NoPublishPlugin)
  .dependsOn(core)
  .settings(
    name := "tests",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.0.0-M10" % Test
  )
