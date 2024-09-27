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
import org.scalajs.dom

def algo(a: NArray[Double], b :NArray[Double], c: Double ) = (a + b)  / c

val a = NArray(1.0, 2.0, 3.0, 2.0)
val b = NArray(4.0, 5.0, 6.0, 2.0)
val c = 2.0

// you'll have to look in the browser console to see this.
val p1 = dom.document.createElement("p")
val p2 = dom.document.createElement("p")

println("boo")

p1.innerHTML = a.mkString("BLAS in browser! ")
p2.innerHTML = b.mkString(algo(a, b, c).mkString("[",", ", "]"))

node.appendChild(p1)
node.appendChild(p2)


```
You can place the `algo` method in the shared part of your project, and use it on all platforms.