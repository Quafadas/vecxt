
import $ivy.`com.github.lolgab::mill-crossplatform::0.2.4`
import $ivy.`io.github.quafadas::millSite::0.0.15`
import $ivy.`de.tototec::de.tobiasroeser.mill.vcs.version::0.4.0`

import de.tobiasroeser.mill.vcs.version._
import com.github.lolgab.mill.crossplatform._
import mill._, mill.scalalib._, mill.scalajslib._, mill.scalanativelib._
import io.github.quafadas.millSite.SiteModule
import io.github.quafadas.millSite.QuickChange
import mill._, scalalib._, publish._
import mill.scalajslib.api._
import mill.scalanativelib._

import mill.api.Result

trait Common extends ScalaModule  with PublishModule {
  def scalaVersion = "3.3.1"

  def publishVersion = VcsVersion.vcsState().format()

  def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"ai.dragonfly::narr::0.105"
  )

  override def pomSettings = T {
    PomSettings(
      description = "Automatically generate html tables from scala case classes",
      organization = "io.github.quafadas",
      url = "https://github.com/Quafadas/vecxt",
      licenses = Seq(License.`Apache-2.0`),
      versionControl =
        VersionControl.github("quafadas", "vecxt"),
      developers = Seq(
        Developer("quafadas", "Simon Parten", "https://github.com/quafadas")
      )
    )
  }

}

trait CommonJS extends ScalaJSModule {
  def scalaJSVersion = "1.15.0"

  def moduleKind = ModuleKind.CommonJSModule
}


trait CommonNative extends ScalaNativeModule {
  def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"org.ekrich::sblas::0.5.0"
  )
  def scalaNativeVersion: mill.T[String] = "0.4.16"
}

trait CommonTests extends TestModule.Munit {
  def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"org.scalameta::munit::1.0.0-M10",
  )
}


object vecxt extends CrossPlatform {
  trait Shared extends CrossPlatformScalaModule with Common {
    // common `core` settings here
    trait SharedTests extends CommonTests {
      // common `core` test settings here
    }
  }
  object jvm extends Shared {
    // jvm specific settings here
    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"dev.ludovic.netlib:blas:3.0.3"
    )

    object test extends ScalaTests with SharedTests
  }
  object js extends Shared with CommonJS {
    // js specific settings here
    object test extends ScalaJSTests with SharedTests
  }

  object native extends Shared with CommonNative {
    // native specific settings here
    object test extends ScalaNativeTests with SharedTests
  }

}

object site extends SiteModule {

  def scalaVersion = vecxt.jvm.scalaVersion

  override def moduleDeps = Seq(vecxt.jvm)

}