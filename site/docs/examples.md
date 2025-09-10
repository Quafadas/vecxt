# Vector Examples

Some basic examples with doubles.

```scala mdoc
import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

val v1 = Array[Double](1, 2, 3)
val v2 = Array[Double](4, 5, 6)

v1.dot(v2)

v1.sumSIMD // .sum std is slow. We can't squat on `sum` so we need a new name

v1.mean

v1.variance

v1.productSIMD

v1.norm

v1.clampMin(1.5).printArr

v2.clampMax(1.5).printArr

v1.clamp(1.5, 2.5 ).printArr

v1.maxSIMD

v1.minSIMD

v1.corr(v2)

v1.spearmansRankCorrelation(v2)

cosineSimilarity(v1, v2)

v1.productExceptSelf.printArr

v1.increments.printArr

v1.logSumExp

v1.cumsum.printArr

(v1 + 1.0).printArr

(v1 + v2).printArr

(v1 - 1.0).printArr

(v1 - v2).printArr

(v1 * 2.0).printArr

(v1 * v2).printArr

(v1 / 2.0).printArr

(v1 / v2).printArr

(v1.exp).printArr
(v1.log).printArr

(v1 > 2).printArr
(v1 >= 2).printArr

(v1 < 2).printArr
(v1 <= 2).printArr

(v1(v1 <= 2)).printArr

(v1.outer(v2)).printMat

v1.exp.printArr

v1.log.printArr

v1.sin.printArr

(-v1).printArr

// Many of the urnary ops also have in place version, which would prevent an extra allocation. They have a `!` in their name by convention, and return `Unit`
// Most trig operations are available but not listed here.

v1.`exp!`
v1.printArr

v1.`log!`
v1.printArr

v1.`sin!`
v1.printArr

v1.`cos!`
v1.printArr

v1.sin.printArr
v1.cos.printArr

```
And Ints. Note that the API here is more limited at the moment.

```scala mdoc:reset
import vecxt.all.*
import narr.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

val v1 = NArray(1, 2, 3)
val v2 = NArray(4, 5, 6)


v1.dot(v2)

(v1 + v2).printArr

(v1 - v2).printArr

(v1 > 2).printArr
(v1 >= 2).printArr

(v1 < 2).printArr
(v1 <= 2).printArr

(v1(v1 <= 2)).printArr

```