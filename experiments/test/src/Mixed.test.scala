package vecxt.experiments

import narr.*
import java.lang.foreign.Arena
import java.lang.foreign.MemorySegment
import java.lang.foreign.ValueLayout

import vecxt.experiments.*
import vecxt.experiments.IntVector.*
import blis_typed.blis_h as blis

class MixedSuite extends munit.FunSuite:

  given arena: BlisArena = BlisArena(Arena.global())

  test("blissable - BLIS integration working!"):
    val arr = Array(1, 2, 3)
    val vecI = IntVector(arr)
    val arrD = Array(1.0, 2.0, 3.0)
    val vecD = DoubleVector(arrD)

    // mixed array matmul here...
    // vecI += vecD

    // // assertEquals(vec2(0), 1, 0.0001)
    // // assertEquals(vec(0), 2, 0.0001)
    // // assertEquals(vec(1), 4, 0.0001)
    // // assertEquals(vec(2), 6, 0.0001)

    // println(vecI)
    // println(vecD)

end MixedSuite
