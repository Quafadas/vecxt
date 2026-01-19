package vecxt_re

import vecxt_re.Retentions.Retention

object Retentions:
  opaque type Retention = Double

  object Retention:
    inline def apply(d: Double): Retention = d
  end Retention

  extension (x: Retention) inline def retention: Double = x
  end extension

  extension (loss: Double)
    inline def -(l: Retention): Double = loss - l
    inline def <(l: Retention): Boolean = loss < l
  end extension
end Retentions

object Limits:
  opaque type Limit = Double

  object Limit:
    inline def apply(d: Double): Limit = d
  end Limit

  extension (x: Limit) inline def limit: Double = x
  end extension

  extension (in: Double)
    inline def >(l: Limit): Boolean = in > l
    inline def +(l: Retention): Double = in + l.retention

  end extension
end Limits
