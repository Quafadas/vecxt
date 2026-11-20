package vecxt_re
import vecxt.all.*
import vecxt_re.Limits.*
import vecxt_re.Retentions.*

import jdk.incubator.vector.DoubleVector
object rpt:
  private val SPECIES = DoubleVector.SPECIES_PREFERRED
  private val SPECIES_LENGTH = SPECIES.length()

  private def reinsuranceBoth(
      vec: Array[Double],
      limitVal: Double,
      retentionVal: Double,
      share: Double
  ): Unit =
    val len = vec.length
    val upperBound = SPECIES.loopBound(len)
    val vLimit = DoubleVector.broadcast(SPECIES, limitVal)
    val vRetention = DoubleVector.broadcast(SPECIES, retentionVal)
    val vShare = DoubleVector.broadcast(SPECIES, share)
    var i = 0

    while i < upperBound do
      val v = DoubleVector.fromArray(SPECIES, vec, i)
      // Use fma to combine the final multiply with a zero add (more efficient than separate mul)
      val vResult = v.sub(vRetention).max(0.0).min(vLimit).fma(vShare, DoubleVector.zero(SPECIES))
      vResult.intoArray(vec, i)
      i += SPECIES_LENGTH
    end while

    while i < len do
      val tmp = vec(i) - retentionVal
      val result =
        if tmp < 0.0 then 0.0
        else if tmp > limitVal then limitVal
        else tmp
      vec(i) = result * share
      i += 1
    end while
  end reinsuranceBoth

  private def reinsuranceRetentionOnly(vec: Array[Double], retentionVal: Double, share: Double): Unit =
    val len = vec.length
    val upperBound = SPECIES.loopBound(len)
    val vRetention = DoubleVector.broadcast(SPECIES, retentionVal)
    val vShare = DoubleVector.broadcast(SPECIES, share)
    var i = 0

    while i < upperBound do
      val v = DoubleVector.fromArray(SPECIES, vec, i)
      // Use fma to combine the final multiply with a zero add (more efficient than separate mul)
      val vResult = v.sub(vRetention).max(0.0).fma(vShare, DoubleVector.zero(SPECIES))
      vResult.intoArray(vec, i)
      i += SPECIES_LENGTH
    end while

    while i < len do
      val tmp = vec(i) - retentionVal
      val result = if tmp < 0.0 then 0.0 else tmp
      vec(i) = result * share
      i += 1
    end while
  end reinsuranceRetentionOnly

  private def reinsuranceLimitOnly(vec: Array[Double], limitVal: Double, share: Double): Unit =
    val len = vec.length
    val upperBound = SPECIES.loopBound(len)
    val vLimit = DoubleVector.broadcast(SPECIES, limitVal)
    val vShare = DoubleVector.broadcast(SPECIES, share)
    var i = 0

    while i < upperBound do
      val v = DoubleVector.fromArray(SPECIES, vec, i)
      // Use fma to combine the final multiply with a zero add (more efficient than separate mul)
      val vResult = v.min(vLimit).fma(vShare, DoubleVector.zero(SPECIES))
      vResult.intoArray(vec, i)
      i += SPECIES_LENGTH
    end while

    while i < len do
      val result = if vec(i) > limitVal then limitVal else vec(i)
      vec(i) = result * share
      i += 1
    end while
  end reinsuranceLimitOnly

  private def scaleInPlace(vec: Array[Double], share: Double): Unit =
    val len = vec.length
    val upperBound = SPECIES.loopBound(len)
    val vShare = DoubleVector.broadcast(SPECIES, share)
    var i = 0

    while i < upperBound do
      DoubleVector.fromArray(SPECIES, vec, i).mul(vShare).intoArray(vec, i)
      i += SPECIES_LENGTH
    end while

    while i < len do
      vec(i) = vec(i) * share
      i += 1
    end while
  end scaleInPlace

  private def franchiseBoth(vec: Array[Double], limitVal: Double, retentionVal: Double): Unit =
    val maxLim = limitVal + retentionVal
    val len = vec.length
    val upperBound = SPECIES.loopBound(len)
    val vRetention = DoubleVector.broadcast(SPECIES, retentionVal)
    val vMaxLim = DoubleVector.broadcast(SPECIES, maxLim)
    val vZero = DoubleVector.zero(SPECIES)
    var i = 0

    while i < upperBound do
      val v = DoubleVector.fromArray(SPECIES, vec, i)
      val mask = v.lt(vRetention)
      val vZeroed = v.blend(vZero, mask)
      val vResult = vZeroed.min(vMaxLim)
      vResult.intoArray(vec, i)
      i += SPECIES_LENGTH
    end while

    while i < len do
      val tmp = vec(i)
      vec(i) =
        if tmp < retentionVal then 0.0
        else if tmp > maxLim then maxLim
        else tmp
      i += 1
    end while
  end franchiseBoth

  private def franchiseRetentionOnly(vec: Array[Double], retentionVal: Double): Unit =
    val len = vec.length
    val upperBound = SPECIES.loopBound(len)
    val vRetention = DoubleVector.broadcast(SPECIES, retentionVal)
    val vZero = DoubleVector.zero(SPECIES)
    var i = 0

    while i < upperBound do
      val v = DoubleVector.fromArray(SPECIES, vec, i)
      val mask = v.lt(vRetention)
      val vResult = v.blend(vZero, mask)
      vResult.intoArray(vec, i)
      i += SPECIES_LENGTH
    end while

    while i < len do
      if vec(i) < retentionVal then vec(i) = 0.0
      end if
      i += 1
    end while
  end franchiseRetentionOnly

  private def franchiseLimitOnly(vec: Array[Double], limitVal: Double): Unit =
    val len = vec.length
    val upperBound = SPECIES.loopBound(len)
    val vLimit = DoubleVector.broadcast(SPECIES, limitVal)
    var i = 0

    while i < upperBound do
      val v = DoubleVector.fromArray(SPECIES, vec, i)
      val vResult = v.min(vLimit)
      vResult.intoArray(vec, i)
      i += SPECIES_LENGTH
    end while

    while i < len do
      if vec(i) > limitVal then vec(i) = limitVal
      end if
      i += 1
    end while
  end franchiseLimitOnly

  extension (vec: Array[Double])

    def reinsuranceFunction(limitOpt: Option[Limit], retentionOpt: Option[Retention]): Unit =
      reinsuranceFunction(limitOpt, retentionOpt, 1.0)

    def reinsuranceFunction(limitOpt: Option[Limit], retentionOpt: Option[Retention], share: Double): Unit =
      (limitOpt, retentionOpt) match
        case (Some(limit), Some(retention)) => reinsuranceBoth(vec, limit.limit, retention.retention, share)
        case (None, Some(retention))        => reinsuranceRetentionOnly(vec, retention.retention, share)
        case (Some(limit), None)            => reinsuranceLimitOnly(vec, limit.limit, share)
        case (None, None)                   => if share != 1.0 then scaleInPlace(vec, share)
    end reinsuranceFunction

    def franchiseFunction(limitOpt: Option[Limit], retentionOpt: Option[Retention]): Unit =
      (limitOpt, retentionOpt) match
        case (Some(limit), Some(retention)) => franchiseBoth(vec, limit.limit, retention.retention)
        case (None, Some(retention))        => franchiseRetentionOnly(vec, retention.retention)
        case (Some(limit), None)            => franchiseLimitOnly(vec, limit.limit)
        case (None, None)                   => ()
    end franchiseFunction

  end extension

  // Matrix extensions that delegate to the underlying array
  extension (mat: Matrix[Double])

    /** These operate on `mat.raw` directly, so they are only sound when the matrix owns its whole backing array in a
      * dense layout. A view (submatrix, transpose, offset) would otherwise silently modify elements outside it.
      */
    private def requireDenseContiguous(op: String): Unit =
      if !mat.hasSimpleContiguousMemoryLayout then
        throw new IllegalArgumentException(
          s"$op requires a dense, contiguous matrix (no views or offsets); got ${mat.layoutString}. Copy the matrix first."
        )
      end if
    end requireDenseContiguous

    /** Apply reinsurance function to entire matrix via underlying array */
    def applyReinsurance(limitOpt: Option[Limit], retentionOpt: Option[Retention]): Unit =
      requireDenseContiguous("applyReinsurance")
      mat.raw.reinsuranceFunction(limitOpt, retentionOpt)
    end applyReinsurance

    /** Apply reinsurance function with share to entire matrix via underlying array */
    def applyReinsurance(limitOpt: Option[Limit], retentionOpt: Option[Retention], share: Double): Unit =
      requireDenseContiguous("applyReinsurance")
      mat.raw.reinsuranceFunction(limitOpt, retentionOpt, share)
    end applyReinsurance

    /** Apply franchise function to entire matrix via underlying array */
    def applyFranchise(limitOpt: Option[Limit], retentionOpt: Option[Retention]): Unit =
      requireDenseContiguous("applyFranchise")
      mat.raw.franchiseFunction(limitOpt, retentionOpt)
    end applyFranchise

  end extension
end rpt
