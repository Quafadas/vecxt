package vecxt

import munit.FunSuite
import all.*
import narr.*

class NotImplTest extends FunSuite:
  import BoundsCheck.DoBoundsCheck.yes

  // Helper: create a non-simple memory layout matrix (e.g., a view/slice)
  private def m: Matrix[Double] =
    val base = Matrix[Double](NArray.tabulate[Double](9)(_.toDouble), 3, 3)
    base(::, NArray(1, 2)) // This should create a view, not a simple contiguous layout
  end m

  private def bmat: Matrix[Boolean] =
    val base = Matrix[Boolean](NArray.tabulate[Boolean](9)(i => i % 2 == 0), 3, 3)
    base(::, NArray(1, 2))
  end bmat

  test("DoubleMatrix./ with non-simple layout throws") {
    intercept[NotImplementedError](m / 2.0)
  }

  test("DoubleMatrix.- with non-simple layout throws") {
    intercept[NotImplementedError](m - 2.0)
  }

  test("DoubleMatrix.hadamard with non-simple layout throws") {
    intercept[NotImplementedError](m.hadamard(m))
  }
  test("DoubleMatrix./:/ with non-simple layout throws") {
    intercept[NotImplementedError](m./:/(m))
  }
  test("DoubleMatrix.-:- with non-simple layout throws") {
    intercept[NotImplementedError](m.-:-(m))
  }
  test("DoubleMatrix.unary_- with non-simple layout throws") {
    intercept[NotImplementedError](-m)
  }
  test("DoubleMatrix.exp! with non-simple layout throws") {
    intercept[NotImplementedError](m.`exp!`)
  }
  test("DoubleMatrix.log! with non-simple layout throws") {
    intercept[NotImplementedError](m.`log!`)
  }
  test("DoubleMatrix.exp with non-simple layout throws") {
    intercept[NotImplementedError](m.exp)
  }
  test("DoubleMatrix.log with non-simple layout throws") {
    intercept[NotImplementedError](m.log)
  }
  test("DoubleMatrix.sqrt! with non-simple layout throws") {
    intercept[NotImplementedError](m.`sqrt!`)
  }
  test("DoubleMatrix.sqrt with non-simple layout throws") {
    intercept[NotImplementedError](m.sqrt)
  }
  test("DoubleMatrix.sin with non-simple layout throws") {
    intercept[NotImplementedError](m.sin)
  }
  test("DoubleMatrix.sin! with non-simple layout throws") {
    intercept[NotImplementedError](m.`sin!`)
  }
  test("DoubleMatrix.cos with non-simple layout throws") {
    intercept[NotImplementedError](m.cos)
  }

  // JvmDoubleMatrix
  test("JvmDoubleMatrix.matmul with non-simple layout throws") {
    intercept[NotImplementedError](m.matmul(m.transpose))
  }

  test("JvmDoubleMatrix vector multiply") {
    intercept[NotImplementedError](m.matmul(m.transpose))
  }

  test("JvmDoubleMatrix.*:* with non-simple layout throws") {
    intercept[NotImplementedError](m.*:*(bmat))
  }

  test("JvmDoubleMatrix.*= with non-simple layout throws") {
    intercept[NotImplementedError](m *= 2.0)
  }

  test("JvmDoubleMatrix.>= with non-simple layout throws") {
    intercept[NotImplementedError](m >= 1.0)
  }

  test("JvmDoubleMatrix.> with non-simple layout throws") {
    intercept[NotImplementedError](m > 1.0)
  }

  test("JvmDoubleMatrix.<= with non-simple layout throws") {
    intercept[NotImplementedError](m <= 1.0)
  }

  test("JvmDoubleMatrix.< with non-simple layout throws") {
    intercept[NotImplementedError](m < 1.0)
  }

end NotImplTest
