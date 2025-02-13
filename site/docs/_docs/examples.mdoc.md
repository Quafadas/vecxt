---
title: Vector Examples
---
# Vector Examples

Some basic examples with doubles.

```scala mdoc
import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

val v1 = Array[Double](1, 2, 3)
val v2 = Array[Double](4, 5, 6)

v1.dot(v2)

cosineSimilarity(v1, v2)

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