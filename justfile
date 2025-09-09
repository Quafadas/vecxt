set shell := ["zsh", "-cu"]

MILL := "./millw"

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
  --dump-includes generated/includes_blas.txt /opt/homebrew/Cellar/openblas/0.3.30/include/cblas.h

jextractIncBlis:
  /Users/simon/Code/jextract-1/build/jextract/bin/jextract \
  --dump-includes generated/includes_blis_cblas.txt /opt/homebrew/opt/blis/include/blis/cblas.h

blisTyped:
  /Users/simon/Code/jextract-1/build/jextract/bin/jextract \
  --dump-includes generated/includes_blis_typed.txt /opt/homebrew/opt/blis/include/blis/blis.h

jextract:
  /Users/simon/Code/jextract-1/build/jextract/bin/jextract \
    --output generated/src \\
    -D FORCE_OPENBLAS_COMPLEX_STRUCT \
    -l :/opt/homebrew/Cellar/openblas/0.3.30/lib/libopenblas.dylib \
    -t blas \
    @generated/myInclude.txt \
    /opt/homebrew/Cellar/openblas/0.3.30/include/cblas.h

jextractBlis:
  /Users/simon/Code/jextract-1/build/jextract/bin/jextract \
    --output generated/src \
    --include-function dgemv \
    -D FORCE_OPENBLAS_COMPLEX_STRUCT \
    -l blis \
    -t blis \
    --use-system-load-library \
    @generated/myInclude.txt \
    /opt/homebrew/opt/blis/include/blis/cblas.h

jextractBlisTyped:
  /Users/simon/Code/jextract-1/build/jextract/bin/jextract \
    --output generated/src \
    -D FORCE_OPENBLAS_COMPLEX_STRUCT \
    -l blis \
    -t blis_typed \
    --use-system-load-library \
    @generated/my_includes_blis_typed.txt \
    /opt/homebrew/opt/blis/include/blis/blis.h

jextractMLX:
  /Users/simon/Code/jextract-1/build/jextract/bin/jextract \
    -t mlx \
    --output generated/src \
    --include-dir /Users/simon/Code/mlx-c/ \
    /Users/simon/Code/mlx-c/mlx/c/mlx.h

jextractMLX2:
  /Users/simon/Code/jextract-1/build/jextract/bin/jextract \
    -t mlx2 \
    --output generated/src/mlx2 \
    --include-dir /Users/simon/Code/mlx-c/ \
    /Users/simon/Code/mlx-c/mlx/c/ops.h
