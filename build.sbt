import laika.helium.Helium
import laika.helium.config.*
import laika.format.Markdown.GitHubFlavor
import laika.config.SyntaxHighlighting
import laika.parse.code.languages.ScalaSyntax
import laika.ast.Path.Root
import laika.theme.config.Color
import laika.ast.LengthUnit.*
import laika.format.Markdown.GitHubFlavor
import laika.config.SyntaxHighlighting
import laika.parse.code.languages.ScalaSyntax

import org.scalajs.linker.interface.OutputPatterns

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

ThisBuild / githubWorkflowBuildPreamble ++= Seq(
  WorkflowStep.Use(
    UseRef.Public("actions", "setup-node", "v3"),
    name = Some("Setup NodeJS v18 LTS"),
    params = Map("node-version" -> "18", "cache" -> "npm"),
    cond = Some("matrix.project == 'rootJS'")
  ),
  WorkflowStep.Run(
    List("npm install"),
    cond = Some("matrix.project == 'rootJS'")
  )
)

ThisBuild / tlCiDocCheck := false
// ThisBuild / githubWorkflowBuildPreamble ++= nativeBrewInstallWorkflowSteps.value
// ThisBuild / nativeBrewInstallCond := Some("matrix.project == 'rootNative'")

// publish to s01.oss.sonatype.org (set to true to publish to oss.sonatype.org instead)
ThisBuild / tlSonatypeUseLegacyHost := false

// publish website from this branch
ThisBuild / tlSitePublishBranch := Some("main")
ThisBuild / scalaVersion := "3.3.1"
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17"))
ThisBuild / tlJdkRelease := Some(17)

lazy val root = tlCrossRootProject.aggregate(core, tests)

lazy val core = crossProject(
  JSPlatform,
  JVMPlatform,
  NativePlatform
)
  .crossType(CrossType.Full)
  .settings(
    name := "vecxt",
    description := """High performance extensions for numeric workloads for
      - Array[Double] on JVM
      - Array[Double] on native.
      - Float64Array on JS""",
    libraryDependencies ++= Seq(
      // ai.dragonfly" %%% "narr" % "0.103", I do'nt think we need this as a dependance ... it can be added seperately in userland!
      "dev.ludovic.netlib" % "blas" % "3.0.3"
    )
  )
  .jvmSettings(
  )
  .jsSettings(
    scalaJSUseMainModuleInitializer := false,
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
    jsEnv := {
      import org.scalajs.jsenv.nodejs.NodeJSEnv
      new NodeJSEnv(NodeJSEnv.Config().withArgs(List("--enable-source-maps")))
    }
  )
  .nativeConfigure(_.enablePlugins(ScalaNativeBrewedConfigPlugin))
  .nativeSettings(
    nativeBrewFormulas += "libopenblas-dev", //??
    libraryDependencies += "org.ekrich" %%% "sblas" % "0.5.0"
    // nativeConfig ~= { c => c.withLinkingOptions(c.linkingOptions :+ "-latlas") }
  )

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
    libraryDependencies ++= Seq(
      "ai.dragonfly" %%% "narr" % "0.103"
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
    }
  )

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
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit" % "1.0.0-M10" % Test,
      "ai.dragonfly" %%% "narr" % "0.103" % Test
    )
  )
  .jsSettings(
    scalaJSUseMainModuleInitializer := true,
    scalaJSUseTestModuleInitializer := false,
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
    jsEnv := {
      import org.scalajs.jsenv.nodejs.NodeJSEnv
      new NodeJSEnv(NodeJSEnv.Config().withArgs(List("--enable-source-maps")))
    }
  )
