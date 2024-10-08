
import $ivy.`com.github.lolgab::mill-crossplatform::0.2.4`
import $ivy.`io.github.quafadas::millSite::0.0.33`
import $ivy.`de.tototec::de.tobiasroeser.mill.vcs.version::0.4.0`
import $ivy.`com.lihaoyi::mill-contrib-jmh:`


import de.tobiasroeser.mill.vcs.version._
import com.github.lolgab.mill.crossplatform._
import mill._, mill.scalalib._, mill.scalajslib._, mill.scalanativelib._
import io.github.quafadas.millSite._
import mill._, scalalib._, publish._
import mill.scalajslib.api._
import mill.scalanativelib._
import contrib.jmh.JmhModule
import mill.util.Jvm
import mill.api.Result

trait Common extends ScalaModule  with PublishModule {
  def scalaVersion = "3.3.4"

  def publishVersion = VcsVersion.vcsState().format()

  def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"io.github.quafadas::narr::0.0.5"
  )

  override def scalacOptions: Target[Seq[String]] = super.scalacOptions() ++ Seq("-explain-cyclic")

  override def pomSettings = T {
    PomSettings(
      description = "Making cross platform compute intense problems less vexing",
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

val vecIncubatorFlag = Seq("""--add-modules=jdk.incubator.vector""")

trait CommonJS extends ScalaJSModule {
  def scalaJSVersion = "1.16.0"
  // def ivyDeps = super.ivyDeps() ++ Seq(ivy"com.raquo::ew::0.2.0")
  // def moduleKind = ModuleKind.
}


trait CommonNative extends ScalaNativeModule {
  def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"org.ekrich::sblas::0.7.0"
  )
  def scalaNativeVersion: mill.T[String] = "0.5.5"
}

trait CommonTests extends TestModule.Munit {
  def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"org.scalameta::munit::1.0.1",
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
    override def javacOptions: T[Seq[String]] = super.javacOptions() ++ vecIncubatorFlag
    def forkArgs = super.forkArgs() ++ vecIncubatorFlag
    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"dev.ludovic.netlib:blas:3.0.3"
    )

    object test extends ScalaTests with SharedTests {
      def forkArgs = super.forkArgs() ++ vecIncubatorFlag
    }
  }
  object js extends Shared with CommonJS {
    // js specific settings here
    object test extends ScalaJSTests with SharedTests {
        def moduleKind = ModuleKind.CommonJSModule
    }
  }

  object native extends Shared with CommonNative {
    // native specific settings here
    object test extends ScalaNativeTests with SharedTests
  }
}

object vecxt_extensions extends CrossPlatform {
  override def moduleDeps: Seq[CrossPlatform] = Seq(vecxt)
  trait Shared extends CrossPlatformScalaModule with Common {
    // common `core` settings here
    trait SharedTests extends CommonTests {
      // common `core` test settings here
    }
  }
  object jvm extends Shared {
    override def javacOptions: T[Seq[String]] = super.javacOptions() ++ vecIncubatorFlag
    def forkArgs = super.forkArgs() ++ vecIncubatorFlag
    def ivyDeps = super.ivyDeps() ++ Agg(

    )

    object test extends ScalaTests with SharedTests {
      def forkArgs = super.forkArgs() ++ vecIncubatorFlag
    }
  }
  object js extends Shared with CommonJS {
    override def ivyDeps: Target[Agg[Dep]] = super.ivyDeps() ++ Agg(
      ivy"com.lihaoyi::scalatags::0.13.1",
      ivy"com.raquo::laminar::17.1.0"
    )
    // js specific settings here
    object test extends ScalaJSTests with SharedTests {
      def moduleKind = ModuleKind.CommonJSModule
    }
  }

  object native extends Shared with CommonNative {
    // native specific settings here
    object test extends ScalaNativeTests with SharedTests
  }
}



object benchmark extends JmhModule with ScalaModule {
    def scalaVersion = vecxt.jvm.scalaVersion
    def jmhCoreVersion = "1.37"
    override def forkArgs: T[Seq[String]] = super.forkArgs() ++ vecIncubatorFlag
    override def moduleDeps: Seq[JavaModule] = Seq(vecxt.jvm)

    override def generateBenchmarkSources = T{
      val dest = T.ctx().dest

      val forkedArgs = forkArgs().toSeq

      val sourcesDir = dest / "jmh_sources"
      val resourcesDir = dest / "jmh_resources"

      os.remove.all(sourcesDir)
      os.makeDir.all(sourcesDir)
      os.remove.all(resourcesDir)
      os.makeDir.all(resourcesDir)

      Jvm.runSubprocess(
        "org.openjdk.jmh.generators.bytecode.JmhBytecodeGenerator",
        (runClasspath() ++ generatorDeps()).map(_.path),
        mainArgs = Seq(
          compile().classes.path.toString,
          sourcesDir.toString,
          resourcesDir.toString,
          "default"
        ),
        jvmArgs = forkedArgs
      )


      (sourcesDir, resourcesDir)
    }
}

object jsSite extends SiteJSModule {

  override def moduleDeps = Seq(vecxt.js, vecxt_extensions.js)
  override def scalaVersion = vecxt.js.scalaVersion
  override def scalaJSVersion = vecxt.js.scalaJSVersion
  override def moduleKind = ModuleKind.ESModule
  override def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"org.scala-js::scalajs-dom::2.8.0",
    ivy"com.lihaoyi::scalatags::0.13.1",
    ivy"com.raquo::laminar::17.0.0"
  )
}

// note that scastic won't work, as I don't think we can start a JVM with the incubator flag.
object site extends SiteModule {

  override val jsSiteModule = jsSite
  override def pathToImportMap = Some(PathRef(T.workspace / "importmap.json"))
  override def forkArgs: T[Seq[String]] = super.forkArgs() ++ vecIncubatorFlag
  def scalaVersion = vecxt.jvm.scalaVersion
  override def moduleDeps = Seq(vecxt.jvm)
  override def scalaDocOptions = super.scalaDocOptions

}