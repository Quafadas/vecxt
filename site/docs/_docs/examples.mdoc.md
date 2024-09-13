# Vector Examples

Some basic exampeles.

```scala mdoc

extension (a: Array[Double])
  def printString = println(a.mkString("[",",","]"))

extension (a: Array[Boolean])
  def printString = println(a.mkString("[",",","]"))

import vecxt.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

val v1 = Array[Double](1, 2, 3)
val v2 = Array[Double](4, 5, 6)

v1.dot(v2)

cosineSimilarity(v1, v2)

(v1 + v2).printString

(v1 - v2).printString

(v1 * 2.0).printString

(v1 / 2.0).printString

(v1 > 2).printString
(v1 >= 2).printString

(v1 < 2).printString
(v1 <= 2).printString

```
