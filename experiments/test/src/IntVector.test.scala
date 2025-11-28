package vecxt.experiments

import narr.*
import java.lang.foreign.Arena
import java.lang.foreign.MemorySegment
import java.lang.foreign.ValueLayout

import vecxt.experiments.*
import vecxt.experiments.IntVector.*
import blis_typed.blis_h as blis

class IntVectorSuite extends munit.FunSuite:

  given arena: BlisArena = BlisArena(Arena.global())

  test("IntVector apply and update"):

    val vec = IntVector(5)
    vec(0) = 1
    vec(1) = 2
    vec(2) = 3
    vec(3) = 4
    vec(4) = 5

    assertEquals(vec(0), 1)
    assertEquals(vec(1), 2)
    assertEquals(vec(2), 3)
    assertEquals(vec(3), 4)
    assertEquals(vec(4), 5)

  test("IntVector length"):

    val vec = IntVector(10)
    assertEquals(vec.length, 10L)

  test("IntVector from Array"):
    val arr = Vector(1, 2, 3)
    val vec = IntVector(arr)
    assertEquals(vec(0), 1)
    assertEquals(vec(1), 2)
    assertEquals(vec(2), 3)

  // test("blissable - BLIS integration working!"):
  //   val arr = Vector(1, 2, 3)
  //   val vec = IntVector(arr)
  //   val vec2 = IntVector(arr)

  //   vec += vec2

  //   assertEquals(vec2(0), 1, 0.0001)
  //   assertEquals(vec(0), 2, 0.0001)
  //   assertEquals(vec(1), 4, 0.0001)
  //   assertEquals(vec(2), 6, 0.0001)

  //   val v3 = vec + vec

  //   assertEquals(v3(0), 4, 0.0001)
  //   assertEquals(v3(1), 8, 0.0001)
  //   assertEquals(v3(2), 12, 0.0001)

end IntVectorSuite
