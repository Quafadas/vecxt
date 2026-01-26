package vecxt_re

import cats.kernel.Eq
import cats.kernel.laws.discipline.MonoidTests
import munit.DisciplineSuite
import org.scalacheck.{Arbitrary, Gen}
import java.time.LocalDate

/** Law-based tests for Scenarr Monoid using cats-kernel-laws and discipline.
  *
  * These tests verify that the Scenarr Monoid satisfies all required laws:
  *   - Left identity: empty |+| a = a
  *   - Right identity: a |+| empty = a
  *   - Associativity: (a |+| b) |+| c = a |+| (b |+| c)
  */
class ScenarrMonoidLawsSpec extends DisciplineSuite:

  // Fixed parameters for testing
  private val TestIterations = 50
  private val TestDay1 = LocalDate.of(2019, 1, 1)

  // Generator for positive amounts (above any reasonable threshold)
  private val amountGen: Gen[Double] = Gen.choose(100.0, 10000.0)

  // Generator for days (1-365)
  private val dayGen: Gen[Int] = Gen.choose(1, 365)

  // Generator for iterations (1-TestIterations)
  private val iterationGen: Gen[Int] = Gen.choose(1, TestIterations)

  // Generator for event count (0-20 events)
  private val eventCountGen: Gen[Int] = Gen.choose(0, 25)

  // Small ID space to encourage clashes across Scenarrs being combined
  private val idGen: Gen[Long] = Gen.choose(1L, 100L)

  /** Deterministically derive (iteration, day) from an ID.
    * This ensures that when the same ID appears in different Scenarrs,
    * it always has the same iteration and day - making the combine valid.
    * Amounts can differ and will be aggregated.
    */
  private def iterationForId(id: Long): Int = ((id % TestIterations) + 1).toInt
  private def dayForId(id: Long): Int = ((id % 365) + 1).toInt

  /** Generate a valid Scenarr with fixed numberIterations and day1.
    *
    * Uses a small ID space (1-100) to encourage clashes across Scenarrs.
    * Iteration and day are derived deterministically from ID, so clashing
    * IDs always have consistent (iteration, day) pairs - the amounts get
    * aggregated as expected by the monoid.
    */
  private val scenarrrGen: Gen[Scenarr] = for
    n <- eventCountGen
    ids <- Gen.listOfN(n, idGen).map(_.distinct) // unique within this Scenarr
    amounts <- Gen.listOfN(ids.length, amountGen)
    threshold <- Gen.const(0.0) // Use 0 threshold to avoid filtering
  yield
    val iterations = ids.map(iterationForId).toArray
    val days = ids.map(dayForId).toArray
    new Scenarr(
      iterations,
      days,
      amounts.toArray,
      ids.toArray,
      TestIterations,
      threshold,
      TestDay1,
      s"test-${ids.length}",
      scala.util.Random.nextLong(),
      isSorted = false
    )

  given Arbitrary[Scenarr] = Arbitrary(scenarrrGen)

  /** Equality for Scenarr that compares the semantic content.
    *
    * Two Scenarrs are equal if they have:
    *   - Same numberIterations
    *   - Same threshold
    *   - Same day1
    *   - Same events (id -> (iteration, day, amount)) regardless of order
    */
  given Eq[Scenarr] = Eq.instance { (a, b) =>
    if a.numberIterations != b.numberIterations then false
    else if Math.abs(a.threshold - b.threshold) > 1e-10 then false
    else if a.day1 != b.day1 then false
    else if a.ids.length != b.ids.length then false
    else
      // Compare events by creating a map of id -> (iter, day, amount)
      a.iterations.sameElements(b.iterations) &&
      a.days.sameElements(b.days) &&
      a.amounts.zip(b.amounts).forall((x, y) => Math.abs(x - y) < 1e-10) &&
      a.ids.sameElements(b.ids)
  }

  // Use the fixed-iteration monoid for law testing
  given cats.kernel.Monoid[Scenarr] = Scenarr.monoidForIterations(TestIterations)

  // Run all Monoid law tests
  checkAll("Scenarr.MonoidLaws", MonoidTests[Scenarr].monoid)

end ScenarrMonoidLawsSpec
