package vecxtensions

import munit.Assertions.assertEqualsDouble
 
def assertVecEquals(v1: Array[Double], v2: Array[Double])(implicit loc: munit.Location): Unit =
    assert(v1.length == v2.length)
    var i: Int = 0;
    while i < v1.length do
        assertEqualsDouble(v1(i), v2(i), 1 / 1e6, clue = s"at index $i")
        i += 1
    end while
end assertVecEquals
