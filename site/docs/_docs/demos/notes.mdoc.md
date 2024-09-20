https://developer.mozilla.org/en-US/docs/Web/API/WebGL_API/Matrix_math_for_the_web

This is not going well.


```scala mdoc:js sc:nocompile
import vecxt.Matrix.*
import narr.*
import vecxt.extensions.*

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

println("what's going on here")

// val out = mat.printMl

node.innerHTML = "where has this div gone"

```

<div>
    <math>
    <mrow>
    <msup>
        <mfenced>
        <mrow>
            <mi>a</mi>
            <mo>+</mo>
            <mi>b</mi>
        </mrow>
        </mfenced>
        <mn>2</mn>
    </msup>
    </mrow>
    </math>
</div>
