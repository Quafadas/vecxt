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

(v1 + v2).printArr

(v1 - v2).printArr

(v1 * 2.0).printArr

(v1 / 2.0).printArr

(v1 > 2).printArr
(v1 >= 2).printArr

(v1 < 2).printArr
(v1 <= 2).printArr

(v1(v1 <= 2)).printArr

```
And Ints

```scala mdoc
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