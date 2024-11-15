set shell := ["pwsh", "-c"]

format:
  mill mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources

benchmark:
  mill benchmark.runJmh -jvmArgs --add-modules=jdk.incubator.vector -rf json

benchmarkOnly:
  mill benchmark.runJmh -jvmArgs --add-modules=jdk.incubator.vector -rf json vecxt.benchmark.VarianceBenchmark


setJvm:
  eval "$(cs java --jvm 21 --env)"