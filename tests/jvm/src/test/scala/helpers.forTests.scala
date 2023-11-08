package vecxt

inline def assertVecEquals(v1: Array[Double], v2: Array[Double])(implicit loc: munit.Location): Unit = {
  var i: Int = 0;
  while (i < v1.length) {
    munit.Assertions.assertEquals(v1(i), v2(i))
    i += 1
  }
}