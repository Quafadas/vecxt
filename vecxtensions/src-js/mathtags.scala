package vecxtensions

import scalatags.Text.all.* // Imports commonly used ScalaTags elements like `Tag`, `attrs`, etc.
import scalatags.Text.tags
import vecxt.all.*
import narr.*

import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.tags.*
import com.raquo.laminar.codecs.StringAsIsCodec
import vecxt.BoundsCheck.DoBoundsCheck.no

object MathTagsLaminar:

  extension (m: Matrix[Double])
    def printMl =
      mfenced(
        mtable(
          for i <- 0 until m.rows
          yield mtr(
            for j <- 0 until m.cols
            yield mtd(
              mn(m((j, i)))
            )
          )
        )
      )
  end extension

  val xmlns1 = htmlAttr[String]("xmlns", StringAsIsCodec)
  val math = htmlTag("math")
  // Basic content elements
  val mi = CustomHtmlTag("mi")
  val mn = CustomHtmlTag("mn")
  val mo = CustomHtmlTag("mo")

  val mtext = CustomHtmlTag("mtext")
  val mfrac = CustomHtmlTag("mfrac")
  val msup = CustomHtmlTag("msup")
  val msub = CustomHtmlTag("msub")
  val msupsub = CustomHtmlTag("msubsup")
  val msqrt = CustomHtmlTag("msqrt")
  val mroot = CustomHtmlTag("mroot")
  val mfenced = CustomHtmlTag("mfenced")
  val menclose = CustomHtmlTag("menclose")
  val mtable = CustomHtmlTag("mtable")
  val mtr = CustomHtmlTag("mtr")
  val mtd = CustomHtmlTag("mtd")
  val maligngroup = CustomHtmlTag("maligngroup")
  val malignmark = CustomHtmlTag("malignmark")
  val mspace = CustomHtmlTag("mspace")
  val mrow = CustomHtmlTag("mrow")
  val mphantom = CustomHtmlTag("mphantom")
  val merror = CustomHtmlTag("merror")
  val munderover = CustomHtmlTag("munderover")
  val mover = CustomHtmlTag("mover")
  val munder = CustomHtmlTag("munder")
  val msubsup = CustomHtmlTag("msubsup")
  val munder_accent = CustomHtmlTag("munder")
  val mover_accent = CustomHtmlTag("mover")
  val mmultiscripts = CustomHtmlTag("mmultiscripts")
  val mstyle = CustomHtmlTag("mstyle")
  val mtag = CustomHtmlTag("mtag")
  val mlongdiv = CustomHtmlTag("mlongdiv")
  val mprescripts = CustomHtmlTag("mprescripts")
  val none = CustomHtmlTag("none")
  val semantics = CustomHtmlTag("semantics")
  val annotation = CustomHtmlTag("annotation")
  val annotation_xml = CustomHtmlTag("annotation-xml")
  val msum = CustomHtmlTag("msum")
  val mprod = CustomHtmlTag("mprod")
  val mint = CustomHtmlTag("mint")
end MathTagsLaminar
