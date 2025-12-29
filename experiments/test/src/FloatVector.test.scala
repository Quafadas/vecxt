package vecxt.experiments

import narr.*
import java.lang.foreign.Arena
import java.lang.foreign.MemorySegment
import java.lang.foreign.ValueLayout

import vecxt.experiments.*
import vecxt.experiments.FloatVector.*
import blis_typed.blis_h as blis

class FloatVectorSuite extends munit.FunSuite:

  given arena: BlisArena = BlisArena(Arena.global())

  test("FloatVector apply and update"):

    val vec = FloatVector(5)
    vec(0) = 1f
    vec(1) = 2f
    vec(2) = 3f
    vec(3) = 4f
    vec(4) = 5f

    assertEquals(vec(0), 1f)
    assertEquals(vec(1), 2f)
    assertEquals(vec(2), 3f)
    assertEquals(vec(3), 4f)
    assertEquals(vec(4), 5f)

  test("FloatVector length"):

    val vec = FloatVector(10)
    assertEquals(vec.length, 10L)

  test("FloatVector from Array"):
    val arr = Vector(1f, 2f, 3f)
    val vec = FloatVector(arr)
    assertEquals(vec(0), 1f)
    assertEquals(vec(1), 2f)
    assertEquals(vec(2), 3f)

  test("blissable - BLIS Floategration working!"):
    val arr = Vector(1f, 2f, 3f)
    val vec = FloatVector(arr)
    val vec2 = FloatVector(arr)

    vec += vec2

    assertEqualsFloat(vec2(0), 1f, 0.0001f)
    assertEqualsFloat(vec(0), 2f, 0.0001f)
    assertEqualsFloat(vec(1), 4f, 0.0001f)
    assertEqualsFloat(vec(2), 6f, 0.0001f)

    val v3 = vec + vec

    assertEqualsFloat(v3(0), 4f, 0.0001f)
    assertEqualsFloat(v3(1), 8f, 0.0001f)
    assertEqualsFloat(v3(2), 12f, 0.0001f)

end FloatVectorSuite
