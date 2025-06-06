/*
 * Copyright 2023 quafadas
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package vecxt.reinsurance

import vecxt.reinsurance.Retentions.Retention

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

enum LossCalc:
  case Agg, Occ
end LossCalc

enum DeductibleType:
  case Retention, Franchise, ReverseFranchise
end DeductibleType
