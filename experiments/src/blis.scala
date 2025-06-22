import java.lang.foreign.{Arena, MemorySegment, ValueLayout}
import blis.*
import vecxt.all.*
import narr.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

@main def testBlis() =
  println("blis test" +
    "")
  val Layout = blis.cblas_h.CblasColMajor
  val transa = blis.cblas_h.CblasNoTrans

  val m = 4 // Number of rows
  val n = 4 // Number of columns
  val lda = 4 // Leading dimension
  val incx = 1
  val incy = 1
  val alpha = 1.0
  val beta = 0.0

  val mat1 = Matrix.fromRows[Double](
    NArray[Double](1.0, 2.0, 3.0, 4.0),
    NArray[Double](1.0, 1.0, 1.0, 1.0),
    NArray[Double](3.0, 4.0, 5.0, 6.0),
    NArray[Double](5.0, 6.0, 7.0, 8.0)
  )
  val vec1 = NArray[Double](1.0, 2.0, 1.0, 1.0)
  val vec2 = NArray[Double](0.0, 0.0, 0.0, 0.0)


  val arena = Arena.ofConfined()
  try {
    val a = arena.allocateFrom(
      ValueLayout.JAVA_DOUBLE, mat1.raw*
    )
    val x = arena.allocateFrom(
      ValueLayout.JAVA_DOUBLE, vec1*
    )
    val y = arena.allocateFrom(
      ValueLayout.JAVA_DOUBLE, vec2*
    )
    cblas_h.cblas_dgemv(
      Layout, transa, m, n, alpha,
      a, lda, x, incx, beta, y, incy
    )
    // Copy result from native y to vec2
    var i = 0
    while (i < vec2.length) {
      vec2(i) = y.getAtIndex(ValueLayout.JAVA_DOUBLE, i)
      i += 1
    }
  }
  finally
    arena.close()

  // y0 = 12.0
  // y1 = 5.0
  // y2 = 22.0
  // y3 = 32.0


  println(vec2.printArr)
