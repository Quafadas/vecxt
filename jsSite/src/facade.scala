/*
 * Copyright 2024 quafadas
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package vecxt.facades

import org.scalajs.dom
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import org.scalajs.dom.HTMLElement
import scala.scalajs.js.JSON
import scala.scalajs.js.Promise
import org.scalajs.dom.URL
import org.scalajs.dom.Element
import scala.util.Random
import org.scalajs.dom.Element
import org.scalajs.dom.XMLHttpRequest
import scala.annotation.experimental

@experimental()
object showJsDocs:
  def apply(path: String, node: Element, width: Int = 50) =
    val child = dom.document.createElement("div")
    val anId = "vega" + Random.alphanumeric.take(8).mkString("")
    child.id = anId
    node.appendChild(child)
    child.setAttribute("style", s"width:${width}vmin;height:${width}vmin")

    val opts = EmbedOptions()
    val xhr = new XMLHttpRequest()
    xhr.open("GET", s"$path", false)
    xhr.send()
    val text = xhr.responseText
    val parsed = JSON.parse(text)
    embed(child, parsed, opts)
    ()
  end apply

  def fromSpec(spec: String, node: Element, width: Int = 50) =
    val child = dom.document.createElement("div")
    val anId = "vega" + Random.alphanumeric.take(8).mkString("")
    child.id = anId
    node.appendChild(child)
    child.setAttribute("style", s"width:${width}vmin;height:${width}vmin")

    val opts = EmbedOptions()
    val parsed = JSON.parse(spec)
    embed(child, parsed, opts)
    ()
  end fromSpec

end showJsDocs

type Theme = "excel" | "ggplot2" | "quartz" | "vox" | "dark"

@js.native
trait Actions extends js.Object:

  var compiled: js.UndefOr[Boolean] = js.native

  var editor: js.UndefOr[Boolean] = js.native

  var `export`: js.UndefOr[Boolean | ExportAction] = js.native

  var source: js.UndefOr[Boolean] = js.native
end Actions

@js.native
trait ExportAction extends js.Object:
  var svg: js.UndefOr[Boolean] = js.native
  var png: js.UndefOr[Boolean] = js.native
end ExportAction

type renderer = "canvas" | "svg"

class EmbedOptions(
    var actions: js.UndefOr[Boolean | Actions] = true,
    var ast: js.UndefOr[Boolean] = js.undefined,
    var bind: js.UndefOr[String] = js.undefined,
    var config: js.UndefOr[Any] = js.undefined,
    var defaultStyle: js.UndefOr[Boolean | String] = js.undefined,
    var downloadFileName: js.UndefOr[String] = js.undefined,
    var editorUrl: js.UndefOr[String] = js.undefined,
    var height: js.UndefOr[Double] = js.undefined,
    var hover: js.UndefOr[Boolean] = true,
    var loader: js.UndefOr[Any] = js.undefined,
    var logLevel: js.UndefOr[Double] = js.undefined,
    var mode: js.UndefOr[String] = js.undefined,
    var padding: js.UndefOr[Double] = js.undefined,
    var scaleFactor: js.UndefOr[Double] = js.undefined,
    var sourceFooter: js.UndefOr[String] = js.undefined,
    var sourceHeader: js.UndefOr[String] = js.undefined,
    var theme: js.UndefOr[Theme] = js.undefined,
    var tooltip: js.UndefOr[Any] = js.undefined,
    var viewClass: js.UndefOr[Any] = js.undefined,
    var width: js.UndefOr[Double] = js.undefined,
    var renderer: js.UndefOr[renderer] = js.undefined
) extends js.Object

@js.native
@JSImport("https://cdn.jsdelivr.net/npm/vega-embed@6.26.0/+esm", JSImport.Default, "vegaEmbed")
object embed extends js.Object:
  def apply(element: Element, spec: js.Dynamic, options: EmbedOptions): js.Promise[EmbedResult] = js.native

  // def embedChart(element: HTMLElement, spec: viz.Spec , options: EmbedOptions): js.Promise[EmbedResult] = js.native

  // def embed(clz: String, spec: js.Dynamic, opts: EmbedOptions): js.Promise[EmbedResult] = js.native
end embed

@js.native
trait EmbedResult extends js.Object:
  val view: VegaView = js.native
  val spec: js.Object = js.native
  val vgSpec: js.Object = js.native
  override def finalize(): Unit = js.native
end EmbedResult

@js.native
trait Options extends js.Object:
  // var loader: js.UndefOr[Loader] = js.native
  var logLevel: js.UndefOr[Int] = js.native
  var renderer: js.UndefOr[String] = js.native
  // var tooltip: js.UndefOr[Tooltip] = js.native
  // var functions: js.UndefOr[js.Array[Function]] = js.native
end Options

/** https://vega.github.io/vega/docs/api/view/
  *
  * https://vega.github.io/vega/docs/api/view/#data-and-scales
  *
  * @param parsedSpec
  * @param config
  */
@js.native
@annotation.nowarn("msg=unused explicit parameter")
@JSImport("https://cdn.jsdelivr.net/npm/vega-view@5.13.0/+esm", JSImport.Default, "vega.View")
class VegaView(parsedSpec: js.Dynamic, config: js.Dynamic) extends js.Object:

  def runAsync(): Unit = js.native

  // def data(s: String, j: js.Dynamic): Unit = js.native

  // Most likely, a js.Array[js.Object]
  def data(s: String, data: js.UndefOr[js.Any]): Unit = js.native
  def data(s: String): Unit = js.native

  // Insert only
  def insert(s: String, data: js.Dynamic): Unit = js.native

  def toImageUrl(tpe: String, scale: js.UndefOr[Double] = 1): Promise[URL] = js.native

  // Resolves to an SVG string
  def toSVG(scale: js.UndefOr[Double] = 1): Promise[String] = js.native

  def container(): Element = js.native

  def width(): Int = js.native
  def width(value: Int): VegaView = js.native
  def height(): Int = js.native
  def height(value: Int): VegaView = js.native

  def origin(): js.Array[Double] = js.native

  def signal(s: String, j: js.Dynamic): js.Dynamic = js.native

  def getState(): js.Dynamic = js.native

  def addSignalListener(s: String, handler: js.Function2[String, js.Dynamic, js.UndefOr[js.Dynamic]]): VegaView =
    js.native

  // def data(s:String, j: js.Array[js.Dynamic]): js.Dynamic = js.native

  def addEventListener(s: String, handler: js.Function2[js.Dynamic, js.Dynamic, js.UndefOr[js.Dynamic]]): VegaView =
    js.native

  override def finalize(): Unit = js.native

end VegaView

object Helpers:

  val dataPrintOnlyClickHandler: js.Function2[js.Dynamic, js.Dynamic, js.UndefOr[js.Dynamic]] =
    (event: js.Dynamic, item: js.Dynamic) =>
      val tmp = item.datum
      if tmp == js.undefined then println("No data found")
      else println(JSON.stringify(tmp))
      end if

  val dataClickHandler: js.Function2[js.Dynamic, js.Dynamic, js.UndefOr[js.Dynamic]] =
    (event: js.Dynamic, item: js.Dynamic) => item.datum

  extension (vv: VegaView)
    def printState(): Unit =
      println(JSON.stringify(vv.getState(), space = 2))

    def safeAddSignalListener(
        forSignal: String,
        handler: (x: String, y: js.Dynamic) => Unit
    ): VegaView =
      val signals = getSignals()
      assert(
        signals.contains(forSignal),
        s"Signal $forSignal not found in this graph - try the getSignals method or getState() to view the list of current signals"
      )
      vv.addSignalListener(forSignal, handler)
    end safeAddSignalListener

    def getSignals(): List[String] =
      val tmp = vv.getState()
      js.Object.keys(tmp.signals.asInstanceOf[js.Object]).toList
    end getSignals

    def printSignalEventHandler(forSignal: String): VegaView =
      val signals = getSignals()
      assert(
        signals.contains(forSignal),
        s"Signal $forSignal not found in this graph - try the getSignals method or getState() to view the list of current signals"
      )
      val handler: js.Function2[String, js.Dynamic, js.Dynamic] = (str: String, dyn: js.Dynamic) =>
        println(s"Signal ${str.toString()} fired with value")
        println(JSON.stringify(dyn, space = 1))
        js.Dynamic.literal()

      vv.addSignalListener(forSignal, handler)
    end printSignalEventHandler

    def getSignalEventHandler(forSignal: String): VegaView =
      val signals = getSignals()
      assert(
        signals.contains(forSignal),
        s"Signal $forSignal not found in this graph - try the getSignals method or getState() to view the list of current signals"
      )
      val handler: js.Function2[String, js.Dynamic, js.Dynamic] = (str: String, dyn: js.Dynamic) => dyn
      vv.addSignalListener(forSignal, handler)
    end getSignalEventHandler

    def printEventHandler(forEvent: String = "click"): VegaView =

      val getCircularReplacer: js.Function0[js.Function2[String, js.Any, js.Any]] = () =>
        val seen = new js.Set[js.Object]()

        { (key: String, value: js.Any) =>
          value match
            case v: js.Object =>
              if seen.contains(v) then js.undefined
              else
                seen.add(v)
                v
            case _ => value
        }

      val handler: js.Function2[js.Dynamic, js.Dynamic, js.Dynamic] = (event: js.Dynamic, item: js.Dynamic) =>
        println(JSON.stringify(event, space = 1))
        println(JSON.stringify(item, getCircularReplacer()))
        js.Dynamic.literal()

      vv.addEventListener(forEvent, handler)
    end printEventHandler

  end extension

end Helpers
