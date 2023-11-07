package vecxt

import scala.scalajs.js.typedarray.Float64Array

type vecxting = Float64Array with vecxt

given Conversion[Float64Array, vecxting] with
  def apply(in: Float64Array): vecxting = in.asInstanceOf[vecxting]