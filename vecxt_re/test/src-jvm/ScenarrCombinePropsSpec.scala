package vecxt_re

import munit.FunSuite
import org.scalacheck.{Gen, Prop, Test}
import org.scalacheck.Prop.forAll
import java.time.LocalDate

/** Property tests for `Scenarr.combine` / `Scenarr.combineAll`.
  *
  * With zero thresholds, combine is associative, `Scenarr.empty(n, day1)` is its identity, and `combineAll` equals a
  * fold. With non-zero thresholds associativity does not hold (each binary combine filters), so we check the threshold
  * contract instead.
  */
class ScenarrCombinePropsSpec extends FunSuite:

  private val TestIterations = 50
  private val TestDay1 = LocalDate.of(2019, 1, 1)

  private val amountGen: Gen[Double] = Gen.choose(1.0, 10000.0)
  private val eventCountGen: Gen[Int] = Gen.choose(0, 25)

  // Small ID space to encourage clashes across Scenarrs being combined
  private val idGen: Gen[Long] = Gen.choose(1L, 100L)

  /** Clashing IDs must sit at the same (iteration, day), so derive both from the ID. */
  private def iterationForId(id: Long): Int = ((id % TestIterations) + 1).toInt
  private def dayForId(id: Long): Int = ((id % 365) + 1).toInt

  private def scenarrGen(thresholdGen: Gen[Double]): Gen[Scenarr] = for
    n <- eventCountGen
    ids <- Gen.listOfN(n, idGen).map(_.distinct)
    amounts <- Gen.listOfN(ids.length, amountGen)
    threshold <- thresholdGen
  yield
    // A scenario only holds events above its own threshold.
    val kept = ids.zip(amounts).filter(_._2 > threshold)
    new Scenarr(
      kept.map(e => iterationForId(e._1)).toArray,
      kept.map(e => dayForId(e._1)).toArray,
      kept.map(_._2).toArray,
      kept.map(_._1).toArray,
      TestIterations,
      threshold,
      TestDay1,
      s"test-${kept.length}",
      isSorted = false
    )

  private val zeroThreshold: Gen[Scenarr] = scenarrGen(Gen.const(0.0))
  private val anyThreshold: Gen[Scenarr] = scenarrGen(Gen.oneOf(Gen.const(0.0), Gen.choose(0.0, 5000.0)))

  /** Same metadata and the same events (id -> (iteration, day, amount)), regardless of order within a slot. */
  private def sameContent(a: Scenarr, b: Scenarr): Boolean =
    def events(s: Scenarr) = s.ids.indices.map(i => s.ids(i) -> (s.iterations(i), s.days(i), s.amounts(i))).toMap
    val ea = events(a)
    val eb = events(b)
    a.numberIterations == b.numberIterations &&
    Math.abs(a.threshold - b.threshold) < 1e-6 &&
    a.day1 == b.day1 &&
    a.ids.length == b.ids.length &&
    ea.keySet == eb.keySet &&
    ea.forall { case (id, (i, d, amt)) =>
      val (i2, d2, amt2) = eb(id)
      i == i2 && d == d2 && Math.abs(amt - amt2) < 1e-6
    }
  end sameContent

  private def check(prop: Prop)(using munit.Location): Unit =
    val result = Test.check(Test.Parameters.default.withMinSuccessfulTests(200), prop)
    assert(result.passed, org.scalacheck.util.Pretty.pretty(result))
  end check

  test("zero thresholds: associativity") {
    check(forAll(zeroThreshold, zeroThreshold, zeroThreshold) { (a, b, c) =>
      sameContent((a |+| b) |+| c, a |+| (b |+| c))
    })
  }

  test("zero thresholds: combineAll equals a fold") {
    check(forAll(zeroThreshold, zeroThreshold, zeroThreshold) { (a, b, c) =>
      sameContent(Scenarr.combineAll(Seq(a, b, c)), (a |+| b) |+| c)
    })
  }

  test("any threshold: empty is a two-sided identity") {
    val e = Scenarr.empty(TestIterations, TestDay1)
    check(forAll(anyThreshold) { a =>
      sameContent(e |+| a, a) && sameContent(a |+| e, a)
    })
  }

  test("any threshold: combineAll does not depend on input order") {
    check(forAll(anyThreshold, anyThreshold, anyThreshold) { (a, b, c) =>
      sameContent(Scenarr.combineAll(Seq(a, b, c)), Scenarr.combineAll(Seq(c, a, b)))
    })
  }

  test("any threshold: result is sorted, above its threshold, and its threshold dominates every input's") {
    check(forAll(anyThreshold, anyThreshold, anyThreshold) { (a, b, c) =>
      val r = Scenarr.combineAll(Seq(a, b, c))
      val ordered = r.ids.indices.drop(1).forall { i =>
        r.iterations(i - 1) < r.iterations(i) || (r.iterations(i - 1) == r.iterations(i) && r.days(i - 1) <= r.days(i))
      }
      r.isSorted && ordered && r.amounts.forall(_ > r.threshold) && Seq(a, b, c).forall(_.threshold <= r.threshold)
    })
  }

  test("any threshold: binary threshold is the sum when IDs are shared, the max otherwise") {
    check(forAll(anyThreshold, anyThreshold) { (a, b) =>
      val shared = a.amounts.nonEmpty && b.amounts.nonEmpty && a.ids.toSet.intersect(b.ids.toSet).nonEmpty
      val expected = if shared then a.threshold + b.threshold else math.max(a.threshold, b.threshold)
      Math.abs((a |+| b).threshold - expected) < 1e-9
    })
  }

end ScenarrCombinePropsSpec
