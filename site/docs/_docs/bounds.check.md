---
title: Bounds Checks
---
In performance sensitive vector applications, bounds checking may be an unwelcome overhead.

It is left to the developer, to decide whether, or where this is wanted or not. A boolean is required as a context (given) parameter to some operations.

In this case, we disable bounds checks, maximising performance.

```scala
import vecxt._
import BoundsCheck.DoBoundsCheck.no

val v1 = Array[Double](1, 2, 3)
val v2 = Array[Double](4, 5, 6)

println(v1 + v2)

```

In this case, we disable bounds checks, maximising performance, and generate undefined runtime behaviour. It may fail, it may not, but the results will be unpredictable, wrong and potentially hard to track - it is your responsibility.

```scala
import vecxt._
import BoundsCheck.DoBoundsCheck.no

val v1 = Array[Double](1, 2, 3, 7)
val v2 = Array[Double](4, 5, 6)

println(v1 + v2)

```

Will throw a `VectorDimensionException` at runtime - which hopefully, will be easy to track down.

```scala
import vecxt._
import BoundsCheck.DoBoundsCheck.yes

val v1 = Array[Double](1, 2, 3, 7)
val v2 = Array[Double](4, 5, 6)

println(v1 + v2)

```

If you seek compile time dimensional safety, consider using [slash](https://github.com/dragonfly-ai/slash).

Finally, one may opt in, or out at any individual callsite, should it be desirable, at the inconvenience of mangling the syntax.

```scala
import vecxt._
import BoundsCheck.DoBoundsCheck.no

val v1 = Array[Double](1, 2, 3, 7)
val v2 = Array[Double](4, 5, 6)

println(v1.+(v2)(using BoundsCheck.DoBoundsCheck.yes) )

```