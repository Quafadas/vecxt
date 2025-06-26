import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

@main def validate(): Unit = {
  import scala.io.Source

  def readMatrix(filename: String, rows: Int, cols: Int): Matrix[Double] =
    val data = Source.fromResource(filename).getLines().flatMap(_.split(",")).map(_.toDouble).toArray
    Matrix(data, (rows, cols))

  def readArray(filename: String): Array[Double] =
    Source.fromResource(filename).getLines().flatMap(_.split(",")).map(_.toDouble).toArray

  val w1 = readMatrix("weights1.csv", 28 * 28, 10)
  val w2 = readMatrix("weights2.csv", 10, 10)
  val b1 = readArray("biases1.csv")
  val b2 = readArray("biases2.csv")

  println(s"w1: ${w1.shape}, w2: ${w2.shape}, b1: ${b1.length}, b2: ${b2.length}")

  val testDataRaw = Source.fromResource("mnist_test.csv").getLines().drop(1).map { line =>
    val values = line.split(",").map(_.toDouble)
    val label = values.head.toInt
    val features = values.tail.map(_/255.0)
    (label, features)
  }.toArray

  val testLabels: Array[Int] = testDataRaw.map(_._1)
  val testFeatures: IndexedSeq[Array[Double]] = testDataRaw.map(_._2).toIndexedSeq

  println(s"Loaded ${testLabels.length} test samples from mnist_test.csv")

  val testData = Matrix.fromRows(testFeatures*)

  val predictrions = foward_prop(
    w1,
    b1,
    w2,
    b2,
    testData
  )
  val predict = mostLikely(predictrions.a2)
  val correctPredictions = (predict =:= testLabels).trues
  val accuracy = correctPredictions.toDouble / testLabels.length * 100

  println(s"Sanity: ${predict.take(10).mkString(", ")} ")
  println(s"Accuracy: $accuracy% (${correctPredictions} out of ${testLabels.length} correct)")

}