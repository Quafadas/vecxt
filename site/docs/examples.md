# Array Examples

Some basic examples with doubles.

```scala mdoc
import vecxt.all.{*, given}
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
import vecxt.all.{*, given}
import narr.*

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

## Risk Measures (TVaR/VaR)

The library includes methods for calculating Tail Value at Risk (TVaR) and Value at Risk (VaR), which are commonly used in reinsurance and risk management.

```scala mdoc:reset
import vecxt.all.{*, given}
import narr.*
import vecxt.reinsurance.*

// Create a sample loss distribution
val losses = NArray[Double](10.0, 25.0, 15.0, 50.0, 5.0, 30.0, 20.0, 8.0, 45.0, 12.0)

// Calculate Value at Risk (VaR) at 90% confidence level
// VaR represents the threshold value - 90% of losses are above this value
val var90 = losses.VaR(0.90)

// Calculate Tail Value at Risk (TVaR) at 90% confidence level
// TVaR is the expected loss in the worst 10% of cases (average of the tail)
val tvar90 = losses.tVar(0.90)

// Calculate both TVaR and VaR together (more efficient)
val result = losses.tVarWithVaR(0.90)
val confidenceLevel = result.cl  // = 0.10 (1 - 0.90)
val varValue = result.VaR
val tvarValue = result.TVaR

// Calculate multiple confidence levels at once (most efficient for batch analysis)
val alphas = NArray[Double](0.85, 0.90, 0.95, 0.99)
val results = losses.tVarWithVaRBatch(alphas)
// Each result contains (cl, VaR, TVaR)
results(0).cl   // Confidence level for first alpha
results(0).VaR  // VaR for 85% confidence
results(0).TVaR // TVaR for 85% confidence

// Get a boolean mask indicating which values are in the tail
val tailMask = losses.tVarIdx(0.90)
// tailMask(i) is true if losses(i) is in the worst 10%

// Calculate tail dependence between two distributions
// Measures how often extreme values occur together
val losses1 = NArray[Double](5.0, 10.0, 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0)
val losses2 = NArray[Double](8.0, 12.0, 18.0, 22.0, 28.0, 32.0, 38.0, 42.0, 48.0, 52.0)
val tailDep = losses1.qdep(0.90, losses2)
// Returns proportion of tail observations that are shared (0.0 to 1.0)

```