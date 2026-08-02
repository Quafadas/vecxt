package vecxt.jit

import munit.FunSuite
import jdk.incubator.vector.DoubleVector

/** D3 — Vector species reporting.
  *
  * Records the SIMD species HotSpot selected at runtime and hard-asserts that the incubator module is genuinely loaded.
  *
  * What is asserted:
  *   - {@code SPECIES_PREFERRED} for double, float and int is not the 64-bit (1-lane) fallback. {@code SPECIES_64} for
  *     doubles means one lane — it is the species selected when {@code jdk.incubator.vector} is absent from the module
  *     path or when species resolution fails entirely. Finding it here means every other check (D1, D4) passes for the
  *     wrong reason: the kernels would still run, but as unvectorized scalar code that happens to give correct results.
  *   - The lane count is reported in the test output so any report can be interpreted without knowing which runner
  *     produced it.
  *
  * What is NOT asserted: the exact lane count. GitHub-hosted runners span AVX2 (4-lane double, 256-bit) and AVX-512
  * (8-lane double, 512-bit). Hard-asserting either would make CI runner- dependent. Record-and-report is the right call
  * here; promote to FAIL only if a specific lane count becomes a correctness requirement.
  */
class D3Suite extends FunSuite:

  test("D3: jdk.incubator.vector module is loaded") {
    // The simplest possible check: can we even call SPECIES_PREFERRED without a NCDFE?
    // If the module is absent, the first reference to a Vector class throws
    // NoClassDefFoundError, and we want that as a clear failure rather than an NPE deep in D1.
    val species = DoubleVector.SPECIES_PREFERRED
    assert(species != null, "DoubleVector.SPECIES_PREFERRED is null — incubator module absent?")
  }

  test("D3: double species is not SPECIES_64 (1-lane fallback)") {
    val info = SpeciesInfo.discover()
    println(s"[D3] vector species: ${info.render}")
    assert(
      info.doubleLanes > 1,
      s"D3 github.com/Quafadas/vecxt/issues/105: DoubleVector.SPECIES_PREFERRED has only " +
        s"${info.doubleLanes} lane(s) — this is the 1-lane SPECIES_64 fallback. " +
        s"Check that --add-modules jdk.incubator.vector is on the JVM command line " +
        s"and that species resolution did not silently fall back to a scalar path."
    )
  }

  test("D3: float species is not SPECIES_32 (1-lane fallback)") {
    val info = SpeciesInfo.discover()
    assert(
      info.floatLanes > 1,
      s"D3 github.com/Quafadas/vecxt/issues/105: FloatVector.SPECIES_PREFERRED has only " +
        s"${info.floatLanes} lane(s). Check --add-modules jdk.incubator.vector."
    )
  }

  test("D3: int species is not SPECIES_32 (1-lane fallback)") {
    val info = SpeciesInfo.discover()
    assert(
      info.intLanes > 1,
      s"D3 github.com/Quafadas/vecxt/issues/105: IntVector.SPECIES_PREFERRED has only " +
        s"${info.intLanes} lane(s). Check --add-modules jdk.incubator.vector."
    )
  }

  test("D3: double lane count is a power of two") {
    val info = SpeciesInfo.discover()
    val l = info.doubleLanes
    assert(
      l > 0 && (l & (l - 1)) == 0,
      s"D3: doubleLanes=$l is not a power of two — unexpected species selected"
    )
  }

end D3Suite
