package vecxt

import scala.scalajs.js.typedarray.Float64Array
import scala.util.chaining.*
import BoundsCheck.DoBoundsCheck
import arrays.*
import narr.*
import BoundsCheck.DoBoundsCheck.yes

class BoundsCheckSuite extends munit.FunSuite:

  lazy val v_fill: NArray[Double] = NArray
    .ofSize[Double](5)
    .tap(a =>
      a(0) = 1.0
      a(1) = 2.0
      a(2) = 3.0
      a(3) = 4.0
      a(4) = 5.0
    )

  test("Bounds check") {
    intercept[VectorDimensionMismatch] {
      v_fill - NArray.fill[Double](2)(2.0)
    }
  }

  // I don't know how to do this.
  // test("no bound check") {
  //   intercept[java.lang.ClassCastException](v_fill.-(Float64Array(2))(using vecxt.BoundsCheck.no))
  // }

end BoundsCheckSuite
