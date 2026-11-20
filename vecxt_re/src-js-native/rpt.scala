package vecxt_re
import vecxt.all.*
import vecxt_re.Limits.*
import vecxt_re.Retentions.*

/*

  Retention and limit are known constants

  f(X;retention, limit) = MIN(MAX(X - retention, 0), limit))

  Note: mutates the input array
 */
object rpt:

  private def reinsuranceBoth(vec: Array[Double], limit: Limit, retention: Retention, share: Double): Unit =
    var i = 0
    while i < vec.length do
      val tmp = vec(i) - retention
      val result =
        if tmp < 0.0 then 0.0
        else if tmp > limit then limit.limit
        else tmp
      vec(i) = result * share
      i += 1
    end while
  end reinsuranceBoth

  private def reinsuranceRetentionOnly(vec: Array[Double], retention: Retention, share: Double): Unit =
    var i = 0
    while i < vec.length do
      val tmp = vec(i) - retention
      vec(i) = (if tmp < 0.0 then 0.0 else tmp) * share
      i += 1
    end while
  end reinsuranceRetentionOnly

  private def reinsuranceLimitOnly(vec: Array[Double], limit: Limit, share: Double): Unit =
    var i = 0
    while i < vec.length do
      val tmp = vec(i)
      vec(i) = (if tmp > limit then limit.limit else tmp) * share
      i += 1
    end while
  end reinsuranceLimitOnly

  private def scaleInPlace(vec: Array[Double], share: Double): Unit =
    var i = 0
    while i < vec.length do
      vec(i) = vec(i) * share
      i += 1
    end while
  end scaleInPlace

  private def franchiseBoth(vec: Array[Double], limit: Limit, retention: Retention): Unit =
    val maxLim = limit.limit + retention.retention
    var i = 0
    while i < vec.length do
      val tmp = vec(i)
      if tmp < retention then vec(i) = 0.0
      else if tmp > maxLim then vec(i) = maxLim
      end if
      i += 1
    end while
  end franchiseBoth

  private def franchiseRetentionOnly(vec: Array[Double], retention: Retention): Unit =
    var i = 0
    while i < vec.length do
      if vec(i) < retention then vec(i) = 0.0
      end if
      i += 1
    end while
  end franchiseRetentionOnly

  private def franchiseLimitOnly(vec: Array[Double], limit: Limit): Unit =
    var i = 0
    while i < vec.length do
      if vec(i) > limit then vec(i) = limit.limit
      end if
      i += 1
    end while
  end franchiseLimitOnly

  extension (vec: Array[Double])

    def reinsuranceFunction(limitOpt: Option[Limit], retentionOpt: Option[Retention]): Unit =
      reinsuranceFunction(limitOpt, retentionOpt, 1.0)

    def reinsuranceFunction(limitOpt: Option[Limit], retentionOpt: Option[Retention], share: Double): Unit =
      (limitOpt, retentionOpt) match
        case (Some(limit), Some(retention)) => reinsuranceBoth(vec, limit, retention, share)
        case (None, Some(retention))        => reinsuranceRetentionOnly(vec, retention, share)
        case (Some(limit), None)            => reinsuranceLimitOnly(vec, limit, share)
        case (None, None)                   => if share != 1.0 then scaleInPlace(vec, share)
    end reinsuranceFunction

    /*

    Retention and limit are known constants

    In excel f(x) = if(x < retention, 0, if(x > limit, limit, x)

     */
    def franchiseFunction(limitOpt: Option[Limit], retentionOpt: Option[Retention]): Unit =
      (limitOpt, retentionOpt) match
        case (Some(limit), Some(retention)) => franchiseBoth(vec, limit, retention)
        case (None, Some(retention))        => franchiseRetentionOnly(vec, retention)
        case (Some(limit), None)            => franchiseLimitOnly(vec, limit)
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
