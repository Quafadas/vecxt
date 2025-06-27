package vecxt.experiments

import narr.*
import java.lang.foreign.Arena
import java.lang.foreign.MemorySegment
import java.lang.foreign.ValueLayout

import vecxt.experiments.*
import vecxt.experiments.DoubleVector.*
import blis_typed.blis_h as blis

class SyntaxSuite extends munit.FunSuite:

  given arena: BlisArena = BlisArena(Arena.global())

  test("DoubleVector apply and update"):

    val vec = DoubleVector(5)
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

    val vec = DoubleVector(10)
    assertEquals(vec.length, 10L)

  test("DoubleVector from Array"):
    val arr = Vector(1.0, 2.0, 3.0)
    val vec = DoubleVector(arr)
    assertEquals(vec(0), 1.0)
    assertEquals(vec(1), 2.0)
    assertEquals(vec(2), 3.0)

  test("blissable - BLIS integration working!"):
    val arr = Vector(1.0, 2.0, 3.0)
    val vec = DoubleVector(arr)
    val vec2 = DoubleVector(arr)

    vec += vec2

    assertEqualsDouble(vec2(0), 1.0, 0.0001)
    assertEqualsDouble(vec(0), 2.0, 0.0001)
    assertEqualsDouble(vec(1), 4.0, 0.0001)
    assertEqualsDouble(vec(2), 6.0, 0.0001)

    val v3 = vec + vec

    assertEqualsDouble(v3(0), 4.0, 0.0001)
    assertEqualsDouble(v3(1), 8.0, 0.0001)
    assertEqualsDouble(v3(2), 12.0, 0.0001)

end SyntaxSuite
