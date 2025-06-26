import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

@main def validate =
  val mnist = os.read.lines(os.resource / "mnist_test.csv").drop(1)

  val labels = mnist.map(_.split(",").head.toInt)
  val data = mnist.map(_.split(",").tail.map(_.toDouble / 255.0))

  val x = Matrix.fromRows(data*)
  println(x.shape)

  val weights1 = os.read.lines(os.resource / "weights1.csv").map(_.split(",").map(_.toDouble))
  val weights2 = os.read.lines(os.resource / "weights2.csv").map(_.split(",").map(_.toDouble))

  val w1 = Matrix.fromRows(weights1*).transpose
  println(w1.shape)
  val w2 = Matrix.fromRows(weights2*)

  val biases1 = os.read.lines(os.resource / "biases1.csv").map(_.split(",").map(_.toDouble))
  val biases2 = os.read.lines(os.resource / "biases2.csv").map(_.split(",").map(_.toDouble))

  val b1 = biases1.head
  val b2 = biases2.head

  val predictions = foward_prop(
    w1,
    b1,
    w2,
    b2,
    x
  )

  println(s"Predictions: ${mostLikely(predictions.a2).take(10).mkString(", ")}")
