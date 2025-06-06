package vecxt.reinsurance
import vecxt.reinsurance.Limits.Limit
import vecxt.reinsurance.Retentions.Retention

/*

  Retention and limit are known constants

  f(X;retention, limit) = MIN(MAX(X - retention, 0), limit))

  Note: mutates the input array
 */
object rpt:
  extension (vec: Array[Double])
    inline def reinsuranceFunction(limitOpt: Option[Limit], retentionOpt: Option[Retention]): Unit =
      (limitOpt, retentionOpt) match
        case (Some(limit), Some(retention)) =>
          var i = 0;
          while i < vec.length do
            val tmp = vec(i) - retention
            val result =
              if tmp < 0.0 then 0.0
              else if tmp > limit then limit.limit
              else tmp
            vec(i) = result
            i = i + 1
          end while

        case (None, Some(retention)) =>
          var i = 0;
          while i < vec.length do
            val tmp = vec(i) - retention
            val result = if tmp < 0.0 then 0.0 else tmp
            vec(i) = result
            i = i + 1
          end while

        case (Some(limit), None) =>
          var i = 0;
          while i < vec.length do
            val tmp = vec(i)
            val result = if tmp > limit then limit.limit else tmp
            vec(i) = result
            i = i + 1
          end while

        case (None, None) =>
          ()
    end reinsuranceFunction
    
    inline def reinsuranceFunction(limitOpt: Option[Limit], retentionOpt: Option[Retention], share: Double): Unit =
      (limitOpt, retentionOpt) match
        case (Some(limit), Some(retention)) =>
          var i = 0;
          while i < vec.length do
            val tmp = vec(i) - retention
            val result =
              if tmp < 0.0 then 0.0
              else if tmp > limit then limit.limit
              else tmp
            vec(i) = result * share
            i = i + 1
          end while

        case (None, Some(retention)) =>
          var i = 0;
          while i < vec.length do
            val tmp = vec(i) - retention
            val result = if tmp < 0.0 then 0.0 else tmp
            vec(i) = result * share
            i = i + 1
          end while

        case (Some(limit), None) =>
          var i = 0;
          while i < vec.length do
            val tmp = vec(i)
            val result = if tmp > limit then limit.limit else tmp
            vec(i) = result * share
            i = i + 1
          end while

        case (None, None) =>
          if share != 1.0 then
            var i = 0;
            while i < vec.length do
              vec(i) = vec(i) * share
              i = i + 1
            end while
          end if
    end reinsuranceFunction

    /*

    Retention and limit are known constants

    In excel f(x) = if(x < retention, 0, if(x > limit, limit, x)

     */
    inline def franchiseFunction(inline limitOpt: Option[Limit], inline retentionOpt: Option[Retention]): Unit =
      (limitOpt, retentionOpt) match
        case (None, None) => ()

        case (Some(limit), Some(retention)) =>
          var i = 0;
          val maxLim = limit.limit + retention.retention
          while i < vec.length do
            val tmp = vec(i)
            if tmp < retention then vec(i) = 0.0
            else if tmp > maxLim then vec(i) = maxLim
            else vec(i) = tmp
            end if
            i = i + 1
          end while

        case (Some(limit), None) =>
          var i = 0;
          while i < vec.length do
            val tmp = vec(i)
            if tmp > limit.limit then vec(i) = limit.limit
            else vec(i) = tmp
            end if
            i = i + 1
          end while
        case (None, Some(retention)) =>
          var i = 0;
          while i < vec.length do
            val tmp = vec(i)
            if tmp >= retention.retention then vec(i) = tmp
            else vec(i) = 0.0
            end if
            i = i + 1
          end while
    end franchiseFunction

  end extension
end rpt
