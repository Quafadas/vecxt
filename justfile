
MILL := "./millw"

format:
  {{MILL}} mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources

benchmark:
  {{MILL}} benchmark.runJmh -jvmArgs --add-modules=jdk.incubator.vector -rf json

benchmarkOnly:
  {{MILL}} benchmark.runJmh -jvmArgs --add-modules=jdk.incubator.vector -rf json vecxt.benchmark.DgemmBenchmark

prepareForIde:
  {{MILL}} __.compiledClassesAndSemanticDbFiles

testJS:
  {{MILL}} clean vecxt.js.fastLinkJS
  {{MILL}} vecxt.js.test

testNative:
  {{MILL}} vecxt.native.test

testJvm:
  {{MILL}} vecxt.jvm.test

test:
  {{MILL}} vecxt.__.test

setJvm:
  eval "$(cs java --jvm 21 --env)"