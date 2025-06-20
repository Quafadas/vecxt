

import java.lang.foreign.{Arena, MemorySegment, ValueLayout}
import blis.*


@main def testBlis() =
  val Layout = blis.cblas_h.CblasColMajor
  val transa = blis.cblas_h.CblasNoTrans

  val m = 4 // Number of rows
  val n = 4 // Number of columns
  val lda = 4 // Leading dimension
  val incx = 1
  val incy = 1
  val alpha = 1.0
  val beta = 0.0

  val arena = Arena.ofConfined()
  try {
    val a = arena.allocateFrom(
      ValueLayout.JAVA_DOUBLE,
      1.0, 2.0, 3.0, 4.0,
      1.0, 1.0, 1.0, 1.0,
      3.0, 4.0, 5.0, 6.0,
      5.0, 6.0, 7.0, 8.0
    )
    val x = arena.allocateFrom(
      ValueLayout.JAVA_DOUBLE,
      1.0, 2.0, 1.0, 1.0
    )
    val y = arena.allocate(ValueLayout.JAVA_DOUBLE, n)

    cblas_h.cblas_dgemv(
      Layout, transa, m, n, alpha,
      a, lda, x, incx, beta, y, incy
    )

    for i <- 0 until n do
      println(s"y$i = ${y.getAtIndex(ValueLayout.JAVA_DOUBLE, i)}")
    ()
  }
  finally
    arena.close()
