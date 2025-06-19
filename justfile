
MILL := "./millw"

format:
  {{MILL}} mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources

benchmark:
  {{MILL}} benchmark.runJmh -jvmArgs --add-modules=jdk.incubator.vector -rf json

benchmarkOnly:
  {{MILL}} benchmark.runJmh -jvmArgs --add-modules=jdk.incubator.vector -rf json vecxt.benchmark.DgemmBenchmark

prepareForIde:
  {{MILL}} __.compiledClassesAndSemanticDbFiles


setJvm:
  eval "$(cs java --jvm 21 --env)"