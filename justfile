set shell := ["pwsh", "-c"]

format:
  mill mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources

benchmark:
  mill benchmark.runJmh -jvmArgs --add-modules=jdk.incubator.vector -rf json