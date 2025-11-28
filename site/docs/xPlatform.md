# Cross Platform

Expressed in a cross platform `NArray`, and runs in scalaJS - check your browser console to observer the output.


```scala mdoc:js

import vecxt.all.{*, given}
import narr.*
import org.scalajs.dom

def algo(a: NArray[Double], b :NArray[Double], c: Double ) = (a + b)  / c

val a = NArray[Double](1.0, 2.0, 3.0, 2.0)
val b = NArray[Double](4.0, 5.0, 6.0, 2.0)
val c = 2.0

// you'll have to look in the browser console to see this.
val p1 = dom.document.createElement("p")
val p2 = dom.document.createElement("p")

println("boo")

p1.textContent = "BLAS in browser!"
p2.textContent = algo(a, b, c).mkString("[",", ", "]")

node.appendChild(p1)
node.appendChild(p2)


```
This simple `algo` method is expressed in terms of `NArray`. It can be shared across JS, native and the JVM.

# Math ML

https://developer.mozilla.org/en-US/docs/Web/API/WebGL_API/Matrix_math_for_the_web

It should be possible, to make lovely renders of our matricies in browser, for the purposes of communication.

Although it currently requires a rather hacky workaround, PRs are in flight for (hopefully scala js and laminar)

```scala mdoc:js
import org.scalajs.dom
import com.raquo.laminar.api.L._
import com.raquo.laminar.DomApi

import narr.*

import vecxt.all.{*, given}
import vecxtensions.MathTagsLaminar.*

val base = NArray[Double](11, 12, 13, 14, 15)
val mat1 = Matrix.fromRows[Double](
        base,
        base + 10.0,
        base + 20.0,
        base + 30.0,
        base + 40.0
)
val nodeId = "matrixExample"
node.id = "nodeId"

render(dom.document.querySelector("#nodeId"),
    foreignHtmlElement(
        DomApi.unsafeParseHtmlString(
            p(
                math(
                    xmlns1 := "http://www.w3.org/1998/Math/MathML",
                    mat1.printMl
                )
            ).toString.dropRight(1).drop(20)
        )
    )

)
```