package vecxt_re

import munit.FunSuite
import vecxt.all.*
import vecxt_re.rpt.*
import vecxt_re.Limits.Limit
import vecxt_re.Retentions.Retention

class RptMatrixSuite extends FunSuite:

  private def mat = Matrix.fromRows[Double](
    Array[Double](1.0, 20.0, 30.0),
    Array[Double](40.0, 5.0, 60.0)
  )

  test("applyReinsurance works on a dense matrix") {
    val m = mat
    m.applyReinsurance(Some(Limit(10.0)), Some(Retention(10.0)))
    assertEquals(m.raw.toSeq.sorted, Seq(0.0, 0.0, 10.0, 10.0, 10.0, 10.0))
  }

  test("applyReinsurance works on a transposed (dense row-major) matrix") {
    val m = mat.transpose
    m.applyReinsurance(Some(Limit(10.0)), Some(Retention(10.0)), 0.5)
    assertEquals(m.raw.toSeq.sorted, Seq(0.0, 0.0, 5.0, 5.0, 5.0, 5.0))
  }

  test("applyReinsurance throws on a submatrix view and leaves the parent untouched") {
    val parent = mat
    val before = parent.raw.clone()
    val view = parent.submatrix(0 to 1, 1 to 2)
    assert(!view.hasSimpleContiguousMemoryLayout)

    intercept[IllegalArgumentException](view.applyReinsurance(Some(Limit(10.0)), Some(Retention(10.0))))
    intercept[IllegalArgumentException](view.applyReinsurance(Some(Limit(10.0)), Some(Retention(10.0)), 0.5))
    intercept[IllegalArgumentException](view.applyFranchise(Some(Limit(10.0)), Some(Retention(10.0))))

    assertEquals(parent.raw.toSeq, before.toSeq)
  }

end RptMatrixSuite
