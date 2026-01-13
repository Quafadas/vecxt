
package vecxt.reinsurance

import Limits.Limit
import Retentions.Retention
import rpt.*

import narr.*

import scala.util.chaining.*

class XSuite extends munit.FunSuite:

  // This test is a duplicate ... but if it works, it proves that the extension methods work on every platform with a common NArray supertype :-)...
  test("reinsurance function - ret and limit") {
    val v = NArray
      .ofSize[Double](3)
      .tap(n =>
        n(0) = 8
        n(1) = 11
        n(2) = 16
      )
    v.reinsuranceFunction(Some(Limit(5.0)), Some(Retention(10.0)))
    assert(v(0) == 0.0)
    assert(v(1) == 1.0)
    assert(v(2) == 5.0)
  }
end XSuite
