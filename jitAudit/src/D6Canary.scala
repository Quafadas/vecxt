package vecxt.jit

import jdk.incubator.vector.DoubleVector
import jdk.incubator.vector.VectorOperators
import jdk.incubator.vector.VectorSpecies

/** A deliberately non-intrinsifiable kernel, used by D6 as a harness canary.
  *
  * The {@code VectorSpecies} is passed as a method parameter instead of being read from a {@code static final} field.
  * C2 requires the species to be a compile-time constant in order to select the right SIMD intrinsic and to scalarize
  * the transient {@code Vector} objects via escape analysis. When the species is a parameter, C2 cannot constant-fold
  * it, so:
  *
  *   1. Every {@code DoubleVector.fromArray} and {@code acc.add} call produces a real heap-allocated object — roughly
  *      64–128 bytes each, depending on lane count.
  *   2. The correct intrinsic for the hardware width cannot be selected at compile time.
  *   3. The reduction at the end similarly allocates, because the intermediate vector escapes.
  *
  * D6Suite asserts that this kernel allocates significantly under D1's measurement, confirming the harness would have
  * caught any real {@code @AllocFree} kernel that stopped vectorizing correctly. A harness whose canary does *not*
  * detect allocation cannot be trusted.
  *
  * This is the dynamic counterpart of the C7 static check (species constancy from a getstatic of a static-final field).
  * C7 catches it at bytecode-read time; D6 confirms the runtime consequence.
  */
object D6Canary:

  /** The canary sum. Species passed as a parameter — not scalarized by C2. */
  def sumWithSpeciesParam(arr: Array[Double], species: VectorSpecies[java.lang.Double]): Double =
    var acc = DoubleVector.zero(species)
    val len = species.loopBound(arr.length)
    var i = 0
    while i < len do
      acc = acc.add(DoubleVector.fromArray(species, arr, i))
      i += species.length()
    end while
    var result = acc.reduceLanes(VectorOperators.ADD)
    while i < arr.length do
      result += arr(i)
      i += 1
    end while
    result
  end sumWithSpeciesParam

end D6Canary
