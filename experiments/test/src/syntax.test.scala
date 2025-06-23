package vecxt.experiments


import narr.*
import java.lang.foreign.Arena

import vecxt.experiments.*
import vecxt.experiments.DoubleVector.*

class SyntaxSuite extends munit.FunSuite:

  given arena: Arena = Arena.global()

  test("DoubleVector apply and update"):

    val vec = DoubleVector(5)(using arena)
    vec(0) = 1.0
    vec(1) = 2.0
    vec(2) = 3.0
    vec(3) = 4.0
    vec(4) = 5.0

    assertEquals(vec(0), 1.0)
    assertEquals(vec(1), 2.0)
    assertEquals(vec(2), 3.0)
    assertEquals(vec(3), 4.0)
    assertEquals(vec(4), 5.0)

    println(vec)


  test("DoubleVector length"):

    val vec = DoubleVector(10)(using arena)
    assertEquals(vec.length, 10L)

  test("DoubleVector from Array"):
    val arr = Vector(1.0, 2.0, 3.0)
    val vec = DoubleVector(arr)(using arena)
    assertEquals(vec(0), 1.0)
    assertEquals(vec(1), 2.0)
    assertEquals(vec(2), 3.0)

end SyntaxSuite
