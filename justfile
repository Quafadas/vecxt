
MILL := "JAVA_OPTS=\"-Ddev.ludovic.netlib.blas.nativeLibPath=/opt/homebrew/Cellar/openblas/0.3.30/lib/libopenblas.dylib\" ./millw"

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

jextractIncBlas:
  /Users/simon/Code/jextract-1/build/jextract/bin/jextract \
  --dump-includes includes_blas.txt /opt/homebrew/Cellar/openblas/0.3.30/include/cblas.h

jextractIncBlis:
  /Users/simon/Code/jextract-1/build/jextract/bin/jextract \
  --dump-includes includes_blis.txt /opt/homebrew/opt/blis/include/blis/cblas.h

jextract:
  /Users/simon/Code/jextract-1/build/jextract/bin/jextract \
    --output generated/src \\
    -D FORCE_OPENBLAS_COMPLEX_STRUCT \
    -l :/opt/homebrew/Cellar/openblas/0.3.30/lib/libopenblas.dylib \
    -t blas \
    @myInclude.txt \
    /opt/homebrew/Cellar/openblas/0.3.30/include/cblas.h

jextractBlis:
  /Users/simon/Code/jextract-1/build/jextract/bin/jextract \
    --output generated/src \
    --include-function dgemv \
    -D FORCE_OPENBLAS_COMPLEX_STRUCT \
    -l :/opt/homebrew/opt/blis/lib/libblis.dylib \
    -t blis \
    @myInclude.txt \
    /opt/homebrew/opt/blis/include/blis/cblas.h