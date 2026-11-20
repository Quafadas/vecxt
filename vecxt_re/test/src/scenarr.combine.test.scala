package vecxt_re

import munit.FunSuite
import java.time.LocalDate

class ScenarrCombineSuite extends FunSuite:

  private val Day1 = LocalDate.of(2019, 1, 1)

  private def assertSameEvents(result: Scenarr, s: Scenarr)(using munit.Location): Unit =
    assertEquals(result.iterations.toSeq, s.iterations.toSeq)
    assertEquals(result.days.toSeq, s.days.toSeq)
    assertEquals(result.amounts.toSeq, s.amounts.toSeq)
    assertEquals(result.ids.toSeq, s.ids.toSeq)
    assertEquals(result.numberIterations, s.numberIterations)
    assertEquals(result.threshold, s.threshold)
    assertEquals(result.day1, s.day1)
  end assertSameEvents

  test("Scenarr.empty carries numberIterations, day1 and threshold") {
    val e = Scenarr.empty(7, LocalDate.of(2021, 3, 4), threshold = 2.5)
    assertEquals(e.amounts.length, 0)
    assertEquals(e.numberIterations, 7)
    assertEquals(e.day1, LocalDate.of(2021, 3, 4))
    assertEquals(e.threshold, 2.5)
    assert(e.isSorted)
  }

  test("combineAll of several scenarios") {
    val scenarios = List(
      new Scenarr(Array(1), Array(10), Array(100.0), Array(1L), numberIterations = 2),
      new Scenarr(Array(1), Array(20), Array(200.0), Array(2L), numberIterations = 2),
      new Scenarr(Array(2), Array(15), Array(150.0), Array(3L), numberIterations = 2)
    )

    val result = Scenarr.combineAll(scenarios)

    assertEquals(result.amounts.length, 3)
    assertEquals(result.amounts.sum, 450.0)
    assert(result.isSorted)
  }

  test("combineAll of nothing throws") {
    intercept[IllegalArgumentException](Scenarr.combineAll(Nil))
  }

  test("empty is left identity: empty |+| s = s") {
    val day1 = LocalDate.of(2020, 6, 15)
    val s = Scenarr
      .withGeneratedIds(Array(1, 2), Array(10, 20), Array(100.0, 200.0), 2, threshold = 5.0, day1 = day1, name = "test")
      .sorted

    assertSameEvents(Scenarr.empty(2, day1) |+| s, s)
  }

  test("empty is right identity: s |+| empty = s") {
    val day1 = LocalDate.of(2020, 6, 15)
    val s = Scenarr
      .withGeneratedIds(Array(1, 2), Array(10, 20), Array(100.0, 200.0), 2, threshold = 5.0, day1 = day1, name = "test")
      .sorted

    assertSameEvents(s |+| Scenarr.empty(2, day1), s)
  }

  test("an empty scenario's day1 and numberIterations are ignored") {
    val s = Scenarr.withGeneratedIds(Array(1), Array(10), Array(100.0), 2, day1 = LocalDate.of(2020, 6, 15)).sorted
    val e = Scenarr.empty(99, LocalDate.of(1999, 1, 1))

    assertSameEvents(e |+| s, s)
    assertSameEvents(s |+| e, s)
  }

  test("empty |+| empty takes the left day1 and the larger threshold") {
    val left = Scenarr.empty(3, LocalDate.of(2020, 1, 1), threshold = 1.0)
    val right = Scenarr.empty(4, LocalDate.of(2021, 1, 1), threshold = 2.0)

    val result = left |+| right
    assertEquals(result.amounts.length, 0)
    assertEquals(result.day1, LocalDate.of(2020, 1, 1))
    assertEquals(result.numberIterations, 3)
    assertEquals(result.threshold, 2.0)
    assert(result.isSorted)
  }

  test("an empty scenario with a higher threshold raises the threshold and filters") {
    val s =
      new Scenarr(Array(1, 1), Array(10, 20), Array(6.0, 20.0), Array(1L, 2L), numberIterations = 2, threshold = 5.0)
    val e = Scenarr.empty(2, Day1, threshold = 8.0)

    for result <- Seq(s |+| e, e |+| s) do
      assertEquals(result.threshold, 8.0)
      assertEquals(result.amounts.toSeq, Seq(20.0))
      assertEquals(result.ids.toSeq, Seq(2L))
    end for
  }

  test("combining disjoint events concatenates them") {
    val s1 = Scenarr.withGeneratedIds(Array(1, 1), Array(10, 20), Array(100.0, 200.0), 2, name = "s1")
    val s2 = Scenarr.withGeneratedIds(Array(2, 2), Array(15, 25), Array(150.0, 250.0), 2, name = "s2")

    val result = s1 |+| s2
    assertEquals(result.amounts.length, 4)
    assert(result.isSorted)
    assertEquals(result.iterations.toSeq, Seq(1, 1, 2, 2))
  }

  test("events with same ID aggregate their amounts") {
    val sharedId = 12345L
    val s1 = new Scenarr(Array(1), Array(10), Array(100.0), Array(sharedId), numberIterations = 2, name = "s1")
    val s2 = new Scenarr(Array(1), Array(10), Array(50.0), Array(sharedId), numberIterations = 2, name = "s2")

    val result = s1 |+| s2
    assertEquals(result.amounts.length, 1)
    assertEquals(result.amounts(0), 150.0)
    assertEquals(result.ids(0), sharedId)
  }

  test("same ID with different iteration throws exception") {
    val sharedId = 12345L
    val s1 = new Scenarr(Array(1), Array(10), Array(100.0), Array(sharedId), numberIterations = 2)
    val s2 = new Scenarr(Array(2), Array(10), Array(50.0), Array(sharedId), numberIterations = 2)

    intercept[IllegalArgumentException](s1 |+| s2)
  }

  test("same ID with different day throws exception") {
    val sharedId = 12345L
    val s1 = new Scenarr(Array(1), Array(10), Array(100.0), Array(sharedId), numberIterations = 2)
    val s2 = new Scenarr(Array(1), Array(20), Array(50.0), Array(sharedId), numberIterations = 2)

    intercept[IllegalArgumentException](s1 |+| s2)
  }

  test("different numberIterations throws exception") {
    val s1 = Scenarr.withGeneratedIds(Array(1), Array(10), Array(100.0), numberIterations = 2)
    val s2 = Scenarr.withGeneratedIds(Array(1), Array(10), Array(100.0), numberIterations = 3)

    intercept[IllegalArgumentException](s1 |+| s2)
  }

  test("different day1 throws exception") {
    val s1 = Scenarr.withGeneratedIds(Array(1), Array(5), Array(100.0), 1, day1 = LocalDate.of(2019, 1, 1))
    val s2 = Scenarr.withGeneratedIds(Array(1), Array(1), Array(200.0), 1, day1 = LocalDate.of(2019, 1, 11))

    intercept[IllegalArgumentException](s1 |+| s2)
    intercept[IllegalArgumentException](s2 |+| s1)
  }

  test("thresholds are summed when scenarios share an ID") {
    val s1 = new Scenarr(Array(1, 1), Array(10, 20), Array(30.0, 20.0), Array(1L, 2L), 2, threshold = 10.0)
    val s2 = new Scenarr(Array(1, 2), Array(10, 15), Array(40.0, 24.0), Array(1L, 3L), 2, threshold = 15.0)

    val result = s1 |+| s2
    assertEquals(result.threshold, 25.0)
    // id 1: 70 survives; id 2: 20 <= 25 dropped; id 3: 24 <= 25 dropped
    assertEquals(result.ids.toSeq, Seq(1L))
    assertEquals(result.amounts.toSeq, Seq(70.0))
  }

  test("the larger threshold applies when scenarios share no ID") {
    val s1 = new Scenarr(Array(1, 1), Array(10, 20), Array(12.0, 100.0), Array(1L, 2L), 2, threshold = 10.0)
    val s2 = new Scenarr(Array(2), Array(15), Array(50.0), Array(3L), 2, threshold = 15.0)

    val result = s1 |+| s2
    assertEquals(result.threshold, 15.0)
    // 12 <= 15 dropped
    assertEquals(result.amounts.toSeq, Seq(100.0, 50.0))
  }

  test("threshold groups chain through shared IDs") {
    // a shares with b, b shares with c, d shares with nobody
    val a = new Scenarr(Array(1), Array(1), Array(100.0), Array(1L), 2, threshold = 1.0)
    val b = new Scenarr(Array(1, 1), Array(1, 2), Array(100.0, 100.0), Array(1L, 2L), 2, threshold = 2.0)
    val c = new Scenarr(Array(1), Array(2), Array(100.0), Array(2L), 2, threshold = 3.0)
    val d = new Scenarr(Array(2), Array(1), Array(100.0), Array(3L), 2, threshold = 5.0)

    assertEquals(Scenarr.combineAll(Seq(a, b, c, d)).threshold, 6.0)
    assertEquals(Scenarr.combineAll(Seq(a, d)).threshold, 5.0)
  }

  test("combineAll applies the threshold once, after summing") {
    val a = new Scenarr(Array(1), Array(10), Array(5.0), Array(1L), 2)
    val b = new Scenarr(Array(1), Array(10), Array(5.0), Array(1L), 2)
    val c = Scenarr.empty(2, Day1, threshold = 8.0)

    val all = Scenarr.combineAll(Seq(a, b, c))
    assertEquals(all.amounts.toSeq, Seq(10.0))
    assertEquals(all.threshold, 8.0)

    // Pairwise folding filters early: this documents why combineAll is preferred.
    assertEquals((a |+| (b |+| c)).amounts.length, 0)
  }

  test("result is always sorted by iteration then day") {
    val s1 = Scenarr.withGeneratedIds(Array(2, 1), Array(30, 10), Array(200.0, 100.0), numberIterations = 2)
    val s2 = Scenarr.withGeneratedIds(Array(1, 2), Array(20, 5), Array(150.0, 50.0), numberIterations = 2)

    val result = s1 |+| s2
    assert(result.isSorted)
    assertEquals(result.iterations.toSeq, Seq(1, 1, 2, 2))
    assertEquals(result.days.toSeq, Seq(10, 20, 5, 30))
  }

  test("name is formatted as concat: [s1 + s2]") {
    val s1 = Scenarr.withGeneratedIds(Array(1), Array(10), Array(100.0), numberIterations = 1, name = "alpha")
    val s2 = Scenarr.withGeneratedIds(Array(1), Array(20), Array(200.0), numberIterations = 1, name = "beta")

    assertEquals((s1 |+| s2).name, "concat: [alpha + beta]")
  }

  test("associativity with zero thresholds: (a |+| b) |+| c = a |+| (b |+| c)") {
    val a = new Scenarr(Array(1), Array(10), Array(100.0), Array(1L), numberIterations = 2)
    val b = new Scenarr(Array(1), Array(20), Array(200.0), Array(2L), numberIterations = 2)
    val c = new Scenarr(Array(2), Array(15), Array(150.0), Array(3L), numberIterations = 2)

    assertSameEvents((a |+| b) |+| c, a |+| (b |+| c))
  }

  test("ID aggregation across multiple combines") {
    val sharedId = 999L
    val a = new Scenarr(Array(1), Array(10), Array(100.0), Array(sharedId), numberIterations = 1)
    val b = new Scenarr(Array(1), Array(10), Array(50.0), Array(sharedId), numberIterations = 1)
    val c = new Scenarr(Array(1), Array(10), Array(25.0), Array(sharedId), numberIterations = 1)

    for result <- Seq(a |+| b |+| c, Scenarr.combineAll(Seq(a, b, c))) do
      assertEquals(result.amounts.length, 1)
      assertEquals(result.amounts(0), 175.0)
    end for
  }

end ScenarrCombineSuite
