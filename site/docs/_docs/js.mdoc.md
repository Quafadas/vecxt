---
title: Cross Platform
---

# Cross Platform

This has the same algorithm as the [motivation](motivation.mdoc.md) example, but is expressed in a cross platform `NArray`, and runs in scalaJS - check your browser console to observer the output.

<script type="text/javascript" src="https://cdn.jsdelivr.net/gh/stdlib-js/blas@umd/browser.js"></script>

```scala mdoc:js sc:nocompile

import vecxt.*
import narr.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

def algo(a: NArray[Double], b :NArray[Double], c: Double ) = (a + b)  / c

val a = NArray(1.0, 2.0, 3.0, 2.0)
val b = NArray(4.0, 5.0, 6.0, 2.0)
val c = 2.0

// you'll have to look in the browser console to see this.
println("BLAS in browser!")
println(algo(a, b, c).mkString("[",",","]"))



// Need to be able to disable the snippet compiler, for this to work.
// node.innerHTML = algo(a, b, c).mkString(", ")

```
You can place the `algo` method in the shared part of your project, and use it on all platforms.