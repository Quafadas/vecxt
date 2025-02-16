---
title: Bounds Checks
---
# Rationale

In performance sensitive vector applications, bounds checking may be an unwelcome overhead.

# Mechanism

This bounds checking mechansim is an `inline given Boolean`, which reduces to an inline `if` _at compile time_. So if you turn bounds checking off, the compiler doesn't generate the code. No code => zero runtime overhead.

# Implications and use

It is left to the developer, to decide whether, or where BoundsChecks are desirable in their application or not. A boolean is required as a context (given) parameter.

In this case, we disable bounds checks, maximising performance.

```scala
import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.no

val v1 = Array[Double](1, 2, 3)
val v2 = Array[Double](4, 5, 6)

println(v1 + v2)

```

In this case, we disable bounds checks, maximising performance, and generate undefined runtime behaviour. It may fail, it may not, but the results will be unpredictable, wrong and potentially hard to track - it is _your_ responsibility.

```scala
import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.no

val v1 = Array[Double](1, 2, 3, 7)
val v2 = Array[Double](4, 5, 6)

println(v1 + v2)

```

Whereas the example below Will throw a `VectorDimensionException` at runtime - which hopefully, will be easy to track down.

```scala
import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

val v1 = Array[Double](1, 2, 3, 7)
val v2 = Array[Double](4, 5, 6)

println(v1 + v2)

```

If you seek compile time dimensional safety, consider using [slash](https://github.com/dragonfly-ai/slash).

Finally, one may opt in, or out at any individual callsite, should it be desirable, at the inconvenience of mangling the syntax.

```scala
import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.no

val v1 = Array[Double](1, 2, 3, 7)
val v2 = Array[Double](4, 5, 6)

println(v1.+(v2)(using vecxt.BoundsCheck.DoBoundsCheck.yes) )

```