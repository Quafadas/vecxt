package vecxt.jit

import jdk.incubator.vector.DoubleVector
import jdk.incubator.vector.FloatVector
import jdk.incubator.vector.IntVector

/** Vector species discovery for D3.
  *
  * Records what SIMD width the JVM selected at runtime. The plan says to report rather than hard-assert lane counts,
  * because GitHub-hosted runners span AVX2 (4-lane double) and AVX-512 (8-lane double), and a lane-count assertion
  * would flip CI on a runner upgrade.
  *
  * What *is* hard-asserted (by D3Suite) is that we are not on SPECIES_64. SPECIES_64 is the 64-bit species: one double
  * lane. It is the species selected when the incubator module is not on the module path, or when the JVM chooses a
  * hardware-unvectorized path. Finding it at runtime means the {@code --add-modules jdk.incubator.vector} flag was
  * missing or the species resolution regressed entirely — and in that case every other check passes for the wrong
  * reason.
  */
object SpeciesInfo:

  final case class Info(
      doubleSpecies: String,
      doubleLanes: Int,
      floatSpecies: String,
      floatLanes: Int,
      intSpecies: String,
      intLanes: Int
  ):
    def render: String =
      s"double=$doubleSpecies(${doubleLanes}L) float=$floatSpecies(${floatLanes}L) int=$intSpecies(${intLanes}L)"
  end Info

  def discover(): Info =
    val dsp = DoubleVector.SPECIES_PREFERRED
    val fsp = FloatVector.SPECIES_PREFERRED
    val isp = IntVector.SPECIES_PREFERRED
    Info(
      doubleSpecies = dsp.toString,
      doubleLanes = dsp.length(),
      floatSpecies = fsp.toString,
      floatLanes = fsp.length(),
      intSpecies = isp.toString,
      intLanes = isp.length()
    )
  end discover

end SpeciesInfo
