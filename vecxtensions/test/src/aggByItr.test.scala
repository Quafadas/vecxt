package vecxtensions

import munit.FunSuite
import vecxt.reinsurance.Layer

class AggregateByItrSpec extends FunSuite:

  private val layerA: Layer = null.asInstanceOf[Layer]
  private val layerB: Layer = null.asInstanceOf[Layer]

  test("aggregateByItr sums per group for each split") {
    val years = Array(1, 1, 2, 2, 2, 3)
    val amountsA = Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0) // sums: g0=3, g1=12, g2=6
    val amountsB = Array(2.0, 3.0, 5.0, 7.0, 11.0, 13.0) // sums: g0=5, g1=23, g2=13

    val result = aggregateByItr(years, IndexedSeq(layerA -> amountsA, layerB -> amountsB), numItrs = 3)

    assertEquals(result.size, 2)
    val (_, aggA) = result(0)
    val (_, aggB) = result(1)

    assertVecEquals(aggA, Array(3.0, 12.0, 6.0))
    assertVecEquals(aggB, Array(5.0, 23.0, 13.0))
  }

  test("aggregateByItr returns zeros for empty input") {
    val years = Array.emptyIntArray
    val amountsA = Array.emptyDoubleArray
    val result = aggregateByItr(years, IndexedSeq(layerA -> amountsA), numItrs = 4)
    val (_, aggA) = result.head

    assertVecEquals(aggA, Array.fill(4)(0.0))
  }

  test("aggregateByItr keeps zeroes for missing group indices up to numItrs") {
    val years = Array(1, 1, 3) // groups 1 and 3 only
    val amounts = Array(10.0, 5.0, 7.5) // sums: g1=15, g3=7.5
    val result = aggregateByItr(years, IndexedSeq(layerA -> amounts), numItrs = 5)
    val (_, agg) = result.head

    assertVecEquals(agg, Array(15.0, 0.0, 7.5, 0.0, 0.0))
  }
end AggregateByItrSpec
