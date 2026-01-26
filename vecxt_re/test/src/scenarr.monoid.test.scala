package vecxt_re

import munit.FunSuite
import java.time.LocalDate
import cats.kernel.Monoid

class ScenarrMonoidSuite extends FunSuite:

  test("cats Monoid instance is available via given") {
    val monoid = summon[Monoid[Scenarr]]
    assertEquals(monoid.empty.amounts.length, 0)
    assertEquals(monoid.empty.isSorted, true)
  }

  test("cats Monoid.combine works like |+|") {
    val s1 = Scenarr.withGeneratedIds(Array(1), Array(10), Array(100.0), numberIterations = 1)
    val s2 = Scenarr.withGeneratedIds(Array(1), Array(20), Array(200.0), numberIterations = 1)

    val monoid = summon[Monoid[Scenarr]]
    val result = monoid.combine(s1, s2)

    assertEquals(result.amounts.length, 2)
    assertEquals(result.isSorted, true)
  }

  test("cats Monoid.combineAll works for multiple scenarios") {
    val scenarios = List(
      new Scenarr(Array(1), Array(10), Array(100.0), Array(1L), numberIterations = 2),
      new Scenarr(Array(1), Array(20), Array(200.0), Array(2L), numberIterations = 2),
      new Scenarr(Array(2), Array(15), Array(150.0), Array(3L), numberIterations = 2)
    )

    val monoid = Scenarr.monoidForIterations(2)
    val result = monoid.combineAll(scenarios)

    assertEquals(result.amounts.length, 3)
    assertEquals(result.amounts.sum, 450.0)
  }

  test("empty is left identity: empty |+| s = s") {
    val s = Scenarr.withGeneratedIds(
      Array(1, 2),
      Array(10, 20),
      Array(100.0, 200.0),
      numberIterations = 2,
      threshold = 5.0,
      day1 = LocalDate.of(2020, 6, 15),
      name = "test"
    )

    val result = Scenarr.empty |+| s
    assertEquals(result.iterations.toSeq, s.iterations.toSeq)
    assertEquals(result.days.toSeq, s.days.toSeq)
    assertEquals(result.amounts.toSeq, s.amounts.toSeq)
    assertEquals(result.numberIterations, s.numberIterations)
    assertEquals(result.threshold, s.threshold)
    assertEquals(result.day1, s.day1)
  }

  test("empty is right identity: s |+| empty = s") {
    val s = Scenarr.withGeneratedIds(
      Array(1, 2),
      Array(10, 20),
      Array(100.0, 200.0),
      numberIterations = 2,
      threshold = 5.0,
      day1 = LocalDate.of(2020, 6, 15),
      name = "test"
    )

    val result = s |+| Scenarr.empty
    assertEquals(result.iterations.toSeq, s.iterations.toSeq)
    assertEquals(result.days.toSeq, s.days.toSeq)
    assertEquals(result.amounts.toSeq, s.amounts.toSeq)
    assertEquals(result.numberIterations, s.numberIterations)
    assertEquals(result.threshold, s.threshold)
    assertEquals(result.day1, s.day1)
  }

  test("empty |+| empty = empty") {
    val result = Scenarr.empty |+| Scenarr.empty
    assertEquals(result.amounts.length, 0)
    assertEquals(result.isSorted, true)
  }

  test("combining disjoint events concatenates them") {
    val s1 = Scenarr.withGeneratedIds(
      Array(1, 1),
      Array(10, 20),
      Array(100.0, 200.0),
      numberIterations = 2,
      threshold = 0.0,
      name = "s1"
    )
    val s2 = Scenarr.withGeneratedIds(
      Array(2, 2),
      Array(15, 25),
      Array(150.0, 250.0),
      numberIterations = 2,
      threshold = 0.0,
      name = "s2"
    )

    val result = s1 |+| s2
    assertEquals(result.amounts.length, 4)
    assertEquals(result.isSorted, true)
    // Should be sorted by (iteration, day)
    assertEquals(result.iterations.toSeq, Seq(1, 1, 2, 2))
  }

  test("events with same ID aggregate their amounts") {
    val sharedId = 12345L
    val s1 = new Scenarr(
      Array(1),
      Array(10),
      Array(100.0),
      Array(sharedId),
      numberIterations = 2,
      threshold = 0.0,
      day1 = LocalDate.of(2019, 1, 1),
      name = "s1"
    )
    val s2 = new Scenarr(
      Array(1),
      Array(10),
      Array(50.0),
      Array(sharedId),
      numberIterations = 2,
      threshold = 0.0,
      day1 = LocalDate.of(2019, 1, 1),
      name = "s2"
    )

    val result = s1 |+| s2
    assertEquals(result.amounts.length, 1)
    assertEquals(result.amounts(0), 150.0) // 100 + 50
    assertEquals(result.ids(0), sharedId)
  }

  test("same ID with different iteration throws exception") {
    val sharedId = 12345L
    val s1 = new Scenarr(
      Array(1),
      Array(10),
      Array(100.0),
      Array(sharedId),
      numberIterations = 2,
      threshold = 0.0,
      day1 = LocalDate.of(2019, 1, 1),
      name = "s1"
    )
    val s2 = new Scenarr(
      Array(2), // different iteration!
      Array(10),
      Array(50.0),
      Array(sharedId),
      numberIterations = 2,
      threshold = 0.0,
      day1 = LocalDate.of(2019, 1, 1),
      name = "s2"
    )

    intercept[IllegalArgumentException] {
      s1 |+| s2
    }
  }

  test("same ID with different day throws exception") {
    val sharedId = 12345L
    val s1 = new Scenarr(
      Array(1),
      Array(10),
      Array(100.0),
      Array(sharedId),
      numberIterations = 2,
      threshold = 0.0,
      day1 = LocalDate.of(2019, 1, 1),
      name = "s1"
    )
    val s2 = new Scenarr(
      Array(1),
      Array(20), // different day!
      Array(50.0),
      Array(sharedId),
      numberIterations = 2,
      threshold = 0.0,
      day1 = LocalDate.of(2019, 1, 1),
      name = "s2"
    )

    intercept[IllegalArgumentException] {
      s1 |+| s2
    }
  }

  test("different numberIterations throws exception") {
    val s1 = Scenarr.withGeneratedIds(Array(1), Array(10), Array(100.0), numberIterations = 2)
    val s2 = Scenarr.withGeneratedIds(Array(1), Array(10), Array(100.0), numberIterations = 3)

    intercept[IllegalArgumentException] {
      s1 |+| s2
    }
  }

  test("thresholds are summed and claims filtered") {
    val s1 = Scenarr.withGeneratedIds(
      Array(1, 1),
      Array(10, 20),
      Array(30.0, 100.0),
      numberIterations = 2,
      threshold = 10.0
    )
    val s2 = Scenarr.withGeneratedIds(
      Array(2),
      Array(15),
      Array(50.0),
      numberIterations = 2,
      threshold = 15.0
    )

    val result = s1 |+| s2
    assertEquals(result.threshold, 25.0) // 10 + 15
    // Only claims > 25 survive: 30.0, 100.0, 50.0 all > 25
    assertEquals(result.amounts.length, 3)
  }

  test("threshold filtering removes small claims") {
    val s1 = Scenarr.withGeneratedIds(
      Array(1),
      Array(10),
      Array(20.0), // will be filtered: 20 <= 25
      numberIterations = 2,
      threshold = 10.0
    )
    val s2 = Scenarr.withGeneratedIds(
      Array(2),
      Array(15),
      Array(50.0), // survives: 50 > 25
      numberIterations = 2,
      threshold = 15.0
    )

    val result = s1 |+| s2
    assertEquals(result.threshold, 25.0)
    assertEquals(result.amounts.length, 1)
    assertEquals(result.amounts(0), 50.0)
  }

  test("day1 is the earlier of the two and days are adjusted") {
    val earlierDay1 = LocalDate.of(2019, 1, 1)
    val laterDay1 = LocalDate.of(2019, 1, 11) // 10 days later

    val s1 = Scenarr.withGeneratedIds(
      Array(1),
      Array(5), // day 5 relative to 2019-01-01 = Jan 5
      Array(100.0),
      numberIterations = 1,
      day1 = earlierDay1
    )
    val s2 = Scenarr.withGeneratedIds(
      Array(1),
      Array(1), // day 1 relative to 2019-01-11 = Jan 11, which is day 11 relative to Jan 1
      Array(200.0),
      numberIterations = 1,
      day1 = laterDay1
    )

    val result = s1 |+| s2
    assertEquals(result.day1, earlierDay1)
    // s1's day 5 stays as 5
    // s2's day 1 becomes 1 + 10 = 11
    assert(result.days.contains(5))
    assert(result.days.contains(11))
  }

  test("day1 adjustment works when s2 has earlier day1") {
    val earlierDay1 = LocalDate.of(2019, 1, 1)
    val laterDay1 = LocalDate.of(2019, 1, 11)

    val s1 = Scenarr.withGeneratedIds(
      Array(1),
      Array(1), // day 1 relative to Jan 11 = Jan 11
      Array(100.0),
      numberIterations = 1,
      day1 = laterDay1
    )
    val s2 = Scenarr.withGeneratedIds(
      Array(1),
      Array(5), // day 5 relative to Jan 1 = Jan 5
      Array(200.0),
      numberIterations = 1,
      day1 = earlierDay1
    )

    val result = s1 |+| s2
    assertEquals(result.day1, earlierDay1)
    // s1's day 1 becomes 1 + 10 = 11
    // s2's day 5 stays as 5
    assert(result.days.contains(5))
    assert(result.days.contains(11))
  }

  test("result is always sorted by iteration then day") {
    val s1 = Scenarr.withGeneratedIds(
      Array(2, 1),
      Array(30, 10),
      Array(200.0, 100.0),
      numberIterations = 2
    )
    val s2 = Scenarr.withGeneratedIds(
      Array(1, 2),
      Array(20, 5),
      Array(150.0, 50.0),
      numberIterations = 2
    )

    val result = s1 |+| s2
    assertEquals(result.isSorted, true)
    // Expected order: (1,10), (1,20), (2,5), (2,30)
    assertEquals(result.iterations.toSeq, Seq(1, 1, 2, 2))
    assertEquals(result.days.toSeq, Seq(10, 20, 5, 30))
  }

  test("name is formatted as concat: [s1 + s2]") {
    val s1 = Scenarr.withGeneratedIds(Array(1), Array(10), Array(100.0), numberIterations = 1, name = "alpha")
    val s2 = Scenarr.withGeneratedIds(Array(1), Array(20), Array(200.0), numberIterations = 1, name = "beta")

    val result = s1 |+| s2
    assertEquals(result.name, "concat: [alpha + beta]")
  }

  test("associativity: (a |+| b) |+| c = a |+| (b |+| c)") {
    // Use explicit IDs to avoid ID collisions that could cause issues
    val a = new Scenarr(
      Array(1),
      Array(10),
      Array(100.0),
      Array(1L),
      numberIterations = 2,
      threshold = 0.0
    )
    val b = new Scenarr(
      Array(1),
      Array(20),
      Array(200.0),
      Array(2L),
      numberIterations = 2,
      threshold = 0.0
    )
    val c = new Scenarr(
      Array(2),
      Array(15),
      Array(150.0),
      Array(3L),
      numberIterations = 2,
      threshold = 0.0
    )

    val leftAssoc = (a |+| b) |+| c
    val rightAssoc = a |+| (b |+| c)

    // Core data should match
    assertEquals(leftAssoc.iterations.toSeq, rightAssoc.iterations.toSeq)
    assertEquals(leftAssoc.days.toSeq, rightAssoc.days.toSeq)
    assertEquals(leftAssoc.amounts.toSeq, rightAssoc.amounts.toSeq)
    assertEquals(leftAssoc.ids.sorted.toSeq, rightAssoc.ids.sorted.toSeq)
    assertEquals(leftAssoc.numberIterations, rightAssoc.numberIterations)
    assertEquals(leftAssoc.threshold, rightAssoc.threshold)
    assertEquals(leftAssoc.day1, rightAssoc.day1)
  }

  test("ID aggregation across multiple combines") {
    val sharedId = 999L
    val a = new Scenarr(Array(1), Array(10), Array(100.0), Array(sharedId), numberIterations = 1)
    val b = new Scenarr(Array(1), Array(10), Array(50.0), Array(sharedId), numberIterations = 1)
    val c = new Scenarr(Array(1), Array(10), Array(25.0), Array(sharedId), numberIterations = 1)

    val result = a |+| b |+| c
    assertEquals(result.amounts.length, 1)
    assertEquals(result.amounts(0), 175.0) // 100 + 50 + 25
  }

end ScenarrMonoidSuite
