package vecxt_re

class ScenarioSuite extends munit.FunSuite:

  test("Events") {

    val event = Event.random

  }

  test("Random Scenario") {
    val numItr = 10
    val s = Scenario(
      Vector.fill(10)(Event.random(maxIter = numItr)),
      numItr
    )

    assertEquals(s.iterations.length, 10)
    assertEquals(s.amounts.length, 10)

    assert(s.hasOccurence)
  }

  test("Some scenario stats") {
    val e1 = Event(1, 15.0)
    val e2 = Event(4, 25.0)
    val e3 = Event(4, 1.0)
    val e4 = Event(4, 1.0)
    val e5 = Event(4, 1.0)
    val numItr = 5

    val s = Scenario(
      Vector(e2, e3, e4, e5, e1),
      numItr
    )

    assertVecEquals(s.freq, Array(1, 0, 0, 4, 0))
    assertVecEquals(s.agg, Array(15.0, 0, 0, 28.0, 0))
    assertEqualsDouble(s.meanFreq, (1 + 4) / 5.0, 0.00000001)
    assertEqualsDouble(s.clusterCoeff, 2.0, 0.000001)
    assertEqualsDouble(s.varianceMeanRatio, 3, 0.00001)

  }

end ScenarioSuite
