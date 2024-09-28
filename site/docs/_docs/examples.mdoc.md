---
title: Vector Examples
---
# Vector Examples

Some basic exampeles.

```scala mdoc
import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

val v1 = Array[Double](1, 2, 3)
val v2 = Array[Double](4, 5, 6)

v1.dot(v2)

cosineSimilarity(v1, v2)

(v1 + v2).print

(v1 - v2).print

(v1 * 2.0).print

(v1 / 2.0).print

(v1 > 2).print
(v1 >= 2).print

(v1 < 2).print
(v1 <= 2).print

```
