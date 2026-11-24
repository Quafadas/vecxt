package vecxt_re

import munit.FunSuite

class ScenarrBuilderSuite extends FunSuite:

  test("events added in order are used as they are and flagged sorted") {
    val b = Scenarr.builder(3, name = "b", sizeHint = 1)
    b.add(1, 5, 10.0, 1L).add(1, 5, 11.0, 2L).add(2, 1, 20.0, 3L).add(3, 7, 30.0, 4L)
    assertEquals(b.length, 4)
    val s = b.result()
    assert(s.isSorted)
    assertEquals(s.iterations.toList, List(1, 1, 2, 3))
    assertEquals(s.days.toList, List(5, 5, 1, 7))
    assertEquals(s.amounts.toList, List(10.0, 11.0, 20.0, 30.0))
    assertEquals(s.ids.toList, List(1L, 2L, 3L, 4L))
    assertEquals(s.name, "b")
    assertEquals(s.numberIterations, 3)
  }

  test("events added out of order are sorted") {
    val b = Scenarr.builder(3)
    b.add(2, 1, 20.0, 1L).add(1, 9, 10.0, 2L).add(1, 3, 5.0, 3L)
    val s = b.result()
    assert(s.isSorted)
    assertEquals(s.iterations.toList, List(1, 1, 2))
    assertEquals(s.days.toList, List(3, 9, 1))
    assertEquals(s.ids.toList, List(3L, 2L, 1L))
    assertEquals(s.freq.toList, List(2, 1, 0))
  }

  test("an empty builder gives an empty sorted scenario") {
    val s = Scenarr.builder(5).result()
    assert(s.isSorted)
    assertEquals(s.amounts.length, 0)
    assertEquals(s.oep.amounts.toList, List.fill(5)(0.0))
  }

  test("grows past its size hint") {
    val b = Scenarr.builder(1000, sizeHint = 2)
    (1 to 1000).foreach(i => b.add(i, 1, i.toDouble, i.toLong))
    val s = b.result()
    assertEquals(s.amounts.length, 1000)
    assertEquals(s.agg.sum, (1 to 1000).sum.toDouble)
  }

  test("rejects bad events") {
    val b = Scenarr.builder(3)
    intercept[IllegalArgumentException](b.add(0, 1, 1.0, 1L))
    intercept[IllegalArgumentException](b.add(4, 1, 1.0, 1L))
    intercept[IllegalArgumentException](b.add(1, 0, 1.0, 1L))
    intercept[IllegalArgumentException](b.add(1, 1, Double.NaN, 1L))
    assertEquals(b.length, 0)
    intercept[IllegalArgumentException](Scenarr.builder(0))
  }

  test("can't be used after result") {
    val b = Scenarr.builder(3)
    b.result()
    intercept[IllegalStateException](b.add(1, 1, 1.0, 1L))
    intercept[IllegalStateException](b.result())
  }
end ScenarrBuilderSuite
