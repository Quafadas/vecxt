package vecxt_re

import munit.FunSuite
import java.time.LocalDate

class ScenarioCombineSuite extends FunSuite:

  private val Day1 = LocalDate.of(2019, 1, 1)

  private def ev(id: Long, iter: Int, day: Int, loss: Double) = Event(id, iter, day, loss)

  test("Scenario.empty carries numberIterations, day1 and threshold") {
    val e = Scenario.empty(7, LocalDate.of(2021, 3, 4), threshold = 2.5)
    assert(e.events.isEmpty)
    assertEquals(e.numberIterations, 7)
    assertEquals(e.day1, LocalDate.of(2021, 3, 4))
    assertEquals(e.threshold, 2.5)
  }

  test("empty is a two-sided identity and keeps name and id") {
    val s = Scenario(Vector(ev(2L, 2, 20, 200.0), ev(1L, 1, 10, 100.0)), 2, 5.0, Day1, "s", 42L)
    val e = Scenario.empty(2, Day1)

    for result <- Seq(e |+| s, s |+| e) do
      assertEquals(result.events.toSeq, s.eventsSorted.toSeq)
      assertEquals(result.threshold, 5.0)
      assertEquals(result.name, "s")
      assertEquals(result.id, 42L)
    end for
  }

  test("an empty scenario's day1 and numberIterations are ignored") {
    val s = Scenario(Vector(ev(1L, 1, 10, 100.0)), 2, 0.0, LocalDate.of(2020, 6, 15))
    val result = Scenario.empty(99, LocalDate.of(1999, 1, 1)) |+| s
    assertEquals(result.day1, LocalDate.of(2020, 6, 15))
    assertEquals(result.numberIterations, 2)
  }

  test("an empty scenario with a higher threshold raises the threshold and filters") {
    val s = Scenario(Vector(ev(1L, 1, 10, 6.0), ev(2L, 1, 20, 20.0)), 2, 5.0, Day1)
    val result = s |+| Scenario.empty(2, Day1, threshold = 8.0)
    assertEquals(result.threshold, 8.0)
    assertEquals(result.events.map(_.eventId), Vector(2L))
  }

  test("events with the same eventId sum their losses") {
    val s1 = Scenario(Vector(ev(7L, 1, 10, 100.0)), 2, 0.0, Day1)
    val s2 = Scenario(Vector(ev(7L, 1, 10, 50.0)), 2, 0.0, Day1)
    assertEquals((s1 |+| s2).events, Vector(ev(7L, 1, 10, 150.0)))
  }

  test("thresholds are summed when scenarios share an ID") {
    val s1 = Scenario(Vector(ev(1L, 1, 10, 30.0), ev(2L, 1, 20, 20.0)), 2, 10.0, Day1)
    val s2 = Scenario(Vector(ev(1L, 1, 10, 40.0), ev(3L, 2, 15, 24.0)), 2, 15.0, Day1)

    val result = s1 |+| s2
    assertEquals(result.threshold, 25.0)
    assertEquals(result.events, Vector(ev(1L, 1, 10, 70.0)))
  }

  test("the larger threshold applies when scenarios share no ID") {
    val s1 = Scenario(Vector(ev(1L, 1, 10, 12.0), ev(2L, 1, 20, 100.0)), 2, 10.0, Day1)
    val s2 = Scenario(Vector(ev(3L, 2, 15, 50.0)), 2, 15.0, Day1)

    val result = s1 |+| s2
    assertEquals(result.threshold, 15.0)
    assertEquals(result.events.map(_.loss), Vector(100.0, 50.0))
  }

  test("combineAll applies the threshold once, after summing") {
    val a = Scenario(Vector(ev(1L, 1, 10, 5.0)), 2, 0.0, Day1)
    val b = Scenario(Vector(ev(1L, 1, 10, 5.0)), 2, 0.0, Day1)
    val c = Scenario.empty(2, Day1, threshold = 8.0)

    assertEquals(Scenario.combineAll(Seq(a, b, c)).events.map(_.loss), Vector(10.0))
  }

  test("mismatches throw") {
    val s = Scenario(Vector(ev(1L, 1, 10, 100.0)), 2, 0.0, Day1)
    intercept[IllegalArgumentException](s |+| Scenario(Vector(ev(2L, 1, 10, 1.0)), 3, 0.0, Day1))
    intercept[IllegalArgumentException](s |+| Scenario(Vector(ev(2L, 1, 10, 1.0)), 2, 0.0, Day1.plusDays(1)))
    intercept[IllegalArgumentException](s |+| Scenario(Vector(ev(1L, 1, 11, 1.0)), 2, 0.0, Day1))
    intercept[IllegalArgumentException](Scenario.combineAll(Nil))
  }

  test("result is sorted by iteration then day") {
    val s1 = Scenario(Vector(ev(1L, 2, 30, 1.0), ev(2L, 1, 10, 1.0)), 2, 0.0, Day1)
    val s2 = Scenario(Vector(ev(3L, 1, 20, 1.0), ev(4L, 2, 5, 1.0)), 2, 0.0, Day1)

    val result = s1 |+| s2
    assertEquals(result.events.map(e => (e.iteration, e.day)), Vector((1, 10), (1, 20), (2, 5), (2, 30)))
  }

end ScenarioCombineSuite
