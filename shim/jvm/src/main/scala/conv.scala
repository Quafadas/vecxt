package vecxt

// import scala.language.implicitConversions


type vecxting = Array[Double] with vecxt

// given Conversion[Array[Double], vecxting] with
//   def apply(in: Array[Double]): vecxting = in.asInstanceOf[vecxting]

extension (inline a: Array[Double])
  inline def vecxtable = a.asInstanceOf[vecxting]

// extension (inline a: vecxting)
//   inline def vecxtable = a