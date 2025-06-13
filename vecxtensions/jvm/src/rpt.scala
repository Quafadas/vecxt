package vecxt.reinsurance
import vecxt.reinsurance.Limits.*
import vecxt.reinsurance.Retentions.*

/*
  Retention and limit are known constants

  f(X;retention, limit) = MIN(MAX(X - retention, 0), limit))

  Note: mutates the input array
 */
import jdk.incubator.vector.DoubleVector
import jdk.incubator.vector.VectorSpecies
object rpt:
  private val SPECIES = DoubleVector.SPECIES_PREFERRED
  private val SPECIES_LENGTH = SPECIES.length()

  // Add specialized inlined methods for hot paths
  private inline def reinsuranceBoth(
      vec: Array[Double],
      limitVal: Double,
      retentionVal: Double,
      share: Double = 1.0
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

  private inline def reinsuranceRetentionOnly(vec: Array[Double], retentionVal: Double, share: Double = 1.0): Unit =
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

  private inline def reinsuranceLimitOnly(vec: Array[Double], limitVal: Double, share: Double = 1.0): Unit =
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

  // Franchise specialized methods (new optimized versions)
  private inline def franchiseBoth(vec: Array[Double], limitVal: Double, retentionVal: Double): Unit =
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

  private inline def franchiseRetentionOnly(vec: Array[Double], retentionVal: Double): Unit =
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

  private inline def franchiseLimitOnly(vec: Array[Double], limitVal: Double): Unit =
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

    inline def reinsuranceFunction(inline limitOpt: Option[Limit], inline retentionOpt: Option[Retention]): Unit =
      (limitOpt, retentionOpt) match
        case (Some(limit), Some(retention)) => reinsuranceBoth(vec, limit.limit, retention.retention)
        case (None, Some(retention))        => reinsuranceRetentionOnly(vec, retention.retention)
        case (Some(limit), None)            => reinsuranceLimitOnly(vec, limit.limit)
        case (None, None)                   => ()

    inline def reinsuranceFunction(
        inline limitOpt: Option[Limit],
        inline retentionOpt: Option[Retention],
        share: Double
    ): Unit =
      (limitOpt, retentionOpt) match
        case (Some(limit), Some(retention)) => reinsuranceBoth(vec, limit.limit, retention.retention, share)
        case (None, Some(retention))        => reinsuranceRetentionOnly(vec, retention.retention, share)
        case (Some(limit), None)            => reinsuranceLimitOnly(vec, limit.limit, share)
        case (None, None)                   =>
          if share != 1.0 then
            val len = vec.length
            val upperBound = SPECIES.loopBound(len)
            val vShare = DoubleVector.broadcast(SPECIES, share)
            var i = 0

            while i < upperBound do
              val v = DoubleVector.fromArray(SPECIES, vec, i)
              // Use fma to combine the multiply with a zero add (more efficient than separate mul)
              val vResult = v.fma(vShare, DoubleVector.zero(SPECIES))
              vResult.intoArray(vec, i)
              i += SPECIES_LENGTH
            end while

            while i < len do
              vec(i) = vec(i) * share
              i += 1
            end while
          end if

    inline def franchiseFunction(inline limitOpt: Option[Limit], inline retentionOpt: Option[Retention]): Unit =
      (limitOpt, retentionOpt) match
        case (Some(limit), Some(retention)) => franchiseBoth(vec, limit.limit, retention.retention)
        case (None, Some(retention))        => franchiseRetentionOnly(vec, retention.retention)
        case (Some(limit), None)            => franchiseLimitOnly(vec, limit.limit)
        case (None, None)                   => ()

  end extension
end rpt
