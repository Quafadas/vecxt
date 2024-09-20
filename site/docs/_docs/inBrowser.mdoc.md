---
title: Beautiful in Browser
---

# Math ML

https://developer.mozilla.org/en-US/docs/Web/API/WebGL_API/Matrix_math_for_the_web

It should be possible, to make lovely renders of our matricies in browser, for the purposes of communication.

Although it currently requires a rather hacky workaround.

```scala mdoc:js sc:nocompile
import org.scalajs.dom
import com.raquo.laminar.api.L._
import com.raquo.laminar.DomApi

import narr.*

import vecxt.Matrix.*
import vecxt.extensions.*
import vecxt_extensions.MathTagsLaminar.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

val base = NArray[Double](11, 12, 13, 14, 15)
val mat1 = Matrix.fromRows(
    NArray(
        base,
        base +:+ 10.0,
        base +:+ 20.0,
        base +:+ 30.0,
        base +:+ 40.0
    )
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