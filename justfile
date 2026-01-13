
MILL := "./mill"

format:
  {{MILL}} mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources

benchmark:
  {{MILL}} benchmark.runJmh -jvmArgs --add-modules=jdk.incubator.vector -rf json

benchmarkOnly:
  {{MILL}} benchmark.runJmh -jvmArgs --add-modules=jdk.incubator.vector -rf json vecxt.benchmark.DgemmBenchmark

prepareBsp:
  {{MILL}} __.compiledClassesAndSemanticDbFiles
  {{MILL}} mill.bsp.BSP/install

cleanJS:
  {{MILL}} clean vecxt.js._

testJS:
  {{MILL}} clean vecxt.js.compile
  {{MILL}} vecxt.js.test

testNative:
  {{MILL}} vecxt.native.test

testJvm:
  {{MILL}} vecxt.jvm.test

test:
  {{MILL}} vecxt.__.test

testOnly target:
  {{MILL}} vecxt.jvm.test.testOnly vecxt.{{target}}

console:
  {{MILL}} -i vecxt.jvm.console

setJvm:
  eval "$(cs java --jvm 21 --env)"