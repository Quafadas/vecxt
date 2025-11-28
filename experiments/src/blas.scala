import java.lang.foreign.{Arena, MemorySegment, ValueLayout}
import blas.*

@main def testBlas() =
  // Print library information
  println("=== Verifying Accelerate Framework Usage ===")
  println(s"Operating System: ${System.getProperty("os.name")}")
  println(s"Architecture: ${System.getProperty("os.arch")}")

  val Layout = cblas_h.CblasColMajor()
  val transa = cblas_h.CblasNoTrans()

  val m = 4 // Number of rows
  val n = 4 // Number of columns
  val lda = 4 // Leading dimension
  val incx = 1
  val incy = 1
  val alpha = 1.0
  val beta = 0.0

  println("\n=== Running CBLAS DGEMV (Matrix-Vector Multiplication) ===")
  val arena = Arena.ofConfined()
  try
    val a = arena.allocateFrom(
      ValueLayout.JAVA_DOUBLE,
      1.0,
      2.0,
      3.0,
      4.0,
      1.0,
      1.0,
      1.0,
      1.0,
      3.0,
      4.0,
      5.0,
      6.0,
      5.0,
      6.0,
      7.0,
      8.0
    )
    val x = arena.allocateFrom(
      ValueLayout.JAVA_DOUBLE,
      1.0,
      2.0,
      1.0,
      1.0
    )
    val y = arena.allocate(ValueLayout.JAVA_DOUBLE, n)

    println("Matrix A (4x4, column-major):")
    println("1 1 3 5")
    println("2 1 4 6")
    println("3 1 5 7")
    println("4 1 6 8")
    println("\nVector x: [1, 2, 1, 1]")
    println("\nCalling cblas_dgemv from Accelerate framework...")

    cblas_h.cblas_dgemv(
      Layout,
      transa,
      m,
      n,
      alpha,
      a,
      lda,
      x,
      incx,
      beta,
      y,
      incy
    )

    println("\n=== Results (y = A * x) ===")
    for i <- 0 until n do println(s"y[$i] = ${y.getAtIndex(ValueLayout.JAVA_DOUBLE, i)}")
    end for
    ()
  finally arena.close()
  end try
end testBlas
