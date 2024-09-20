---
title: Linear Algebra in Browser
---

One of the more exciting opportunities in `vecxt` is that it can run in the browser - and that can make things more fun.

I wanted to be able to display linear algebra in a canoncail way. Fortunately, we have things like [MathML](https://developer.mozilla.org/en-US/docs/Web/MathML/Examples).

<math xmlns= "http://www.w3.org/1998/Math/MathML">>
<mfenced>
<mtable>
	<mtr>
		<mtd> <mn>1</mn> </mtd>
		<mtd> <mn>5</mn> </mtd>
		<mtd> <mn>3</mn> </mtd>
	</mtr>
	<mtr>
		<mtd> <mn>8</mn> </mtd>
		<mtd> <mn>2</mn> </mtd>
		<mtd> <mn>6</mn> </mtd>
	</mtr>
	<mtr>
		<mtd> <mn>7</mn> </mtd>
		<mtd> <mn>9</mn> </mtd>
		<mtd> <mn>0</mn> </mtd>
	</mtr>
</mtable>
</mfenced>
</math>

As it's just xml, it feels like we should be able to write an extension method for our [[vecxt.Matrix]] type, which  can render it nicely in browser. Even cooler, because it's just part of the DOM, we should be able to wheel in Laminar, and get beautiful reactive rendering, interactive calculations, plots... all that stuff.

Practically, it turns out to be more challenging. Like `SVG`, apparntly MathML is not part of the standard html set, and so requires a different set of tags. Or something. There is an [initial PR to scala-dom-types](https://github.com/raquo/scala-dom-types/pull/105), which I hope could turn into a way to do this propertly.

For now, we proceed with a nasty, nasty hack...

```scala sc:nocompile
object MathTagsLaminar:

  extension (m: Matrix)
    def printMl =
      mfenced(
        mtable(
          for (i <- 0 until m.rows)
            yield mtr(
              for (j <- 0 until m.cols)
                yield mtd(
                  mn(m.raw(i * m.cols + j))
                )
            )
        )
      )

  val math = CustomHtmlTag("math")
  // Basic content elements
  val mi = CustomHtmlTag("mi")
  val mn = CustomHtmlTag("mn")
  val mo = CustomHtmlTag("mo")
//etc


foreignHtmlElement(
  DomApi.unsafeParseHtmlString(
    laminarElements.toString.dropRight(1).drop(20)
  )
)
```
Probably don't try this at home :-).



