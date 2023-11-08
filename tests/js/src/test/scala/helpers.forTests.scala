package vecxt

import scala.scalajs.js.typedarray.Float64Array

inline def assertVecEquals(v1: Float64Array, v2: Float64Array)(implicit loc: munit.Location): Unit = {
  var i: Int = 0;
  while (i < v1.length) {
    munit.Assertions.assertEquals(v1(i), v2(i))
    i += 1
  }
}