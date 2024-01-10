---
title:  Motivation
---

# why vecxt

It is often prefereable to be able to perform sub-parts of a numeric algorithm to be executed as close to the user as possible. Everyone loves performance, no one wants to think about it :-).

For example, your data acquisition is serverside, but do parts of a calculation in browser or in response to user input, saving the latency of network traffc and improving user experience in scalaJS.

[[vecxt]] provides easy access to platform native common vector operations via intuitive and common syntax. Guaranteeing platform native performance - wherever you want to do your numerics.

# Cross Platform

[[vecxt]] is cross platform, and can be used on the JVM, JS and Native platforms.

```scala mdoc

import vecxt.*
import narr.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

def algo(a: NArray[Double], b :NArray[Double], c: Double ) = (a + b)  / c

val a = Array[Double](1, 2, 3)
val b = Array[Double](4, 5, 6)
val c = 2.0

println(algo(a, b, c).mkString(", "))

```
You can place the `algo` method in the shared part of your project, and use it on all platforms.