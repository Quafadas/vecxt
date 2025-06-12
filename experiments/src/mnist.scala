import io.github.quafadas.table.*
import scala.util.chaining.*

import viz.PlotTargets.desktopBrowser
import viz.vegaFlavour
import viz.Plottable.*
import viz.Macros.Implicits.given_Writer_T
import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

// File can be dowloaded from Kaggle at:
// https://www.kaggle.com/datasets/quangphota/mnist-csv/data?select=train.csv
// Not included to avoid repository bloat, you'll have to download to the resource directory and name it train.csv

@main def mnist =
  def traindata = CSV.resource("train.csv")

  val samplePlot = true

  val labels = traindata.column["label"].map(_.toInt).toSeq // y data
  val others = traindata.dropColumn["label"].map(_.toList.toArray.map(_.toDouble))

  println(labels.take(10).toSeq.mkString(", "))

  if samplePlot then
    (os.resource / "hist.vg.json").plot(
      List { spec =>
        spec("data")(0)("values") = upickle.default.writeJs(labels.map(i => (u = i)))
      }
    )
  end if

  if samplePlot then
    others
      .take(5)
      .foreach(data =>
        val d = dataToCoords(data)
        (os.resource / "pixelPlot.vg.json").plot(
          List(spec => spec("data")("values") = upickle.default.writeJs(d))
        )
      )
  end if

  val weight1 = Matrix(Array.fill(28 * 28 * 10)(scala.util.Random.nextDouble() *0.1), (10, 28*28))
  val bias1 = Matrix(Array.fill(10)(0.0), (10, 1))
  val weight2 = Matrix(Array.tabulate(10 * 10)(i => i.toDouble * 0.1), (10, 10))
  val bias2 = Matrix(Array.fill(10)(0.0), (10, 1))
  

  // -- Below here is the neural network machinery


  def reluM(z: Matrix[Double]): Matrix[Double] = Matrix(z.raw.clampMax(0.0), z.shape)

  def softmaxCols(z: Matrix[Double]): Matrix[Double] =    
    z.mapCols { col =>
      val exps = (col - col.max).tap(_.`exp!`)
      exps / exps.sum
    }
    

  def foward_prop(w1: Matrix[Double], b1: Matrix[Double], w2: Matrix[Double], b2: Matrix[Double], X: Matrix[Double]) =
    val z1 = (w1 @@ X) + b1
    val a1 = reluM(z1)
    val z2 = (w2 @@ a1) + b2
    val a2 = softmaxCols(z2)
    (z1 = z1, a1 = a1, z2 = z2, a2 = a2)

  def back_prop(w1: Matrix[Double], b1: Matrix[Double], w2: Matrix[Double], b2: Matrix[Double], z1: Matrix[Double], a1: Matrix[Double], z2: Matrix[Double], a2: Matrix[Double], X: Matrix[Double], Y: Seq[Int]) =
    val m = Y.cols
    val m_inv = 1.0 / m
    val dz2 = a2 - Y
    val dw2 = m_inv * (dz2 @@  1.transpose) //(10, 10)
    val db2 = m_inv * dz2.mapRowsToScalar(_.sum) // (10, 1)
    val dz1 = (w2.transpose @@ dz2) * (z1 > 0)
    val dw1 = m_inv * (dz1 @@ X.transpose) //(10, 784)
    val db1 = m_inv * dz1.mapRowsToScalar(_.sum) // (10, 1)
    (dw1 = dw1, db1 = db1, dw2 = dw2, db2 = db2)

  def oneHotEncode(labels: Seq[Int]): Matrix[Double] =
    val n = labels.length
    val m = 10 // number of classes
    val oneHot = Array.fill(n * m)(0.0)
    var i = 0
    // column major. Could also be done with Matrix.fromRows... but that would be less efficient
    while (i < labels.length) {
      oneHot(i * m + labels(i)) = 1.0
      i += 1
    }
    Matrix(oneHot, (n, m))

  def mostLikely(weights: Matrix[Double]): Matrix[Int] = 
    val m = weights.raw
    weights.mapColsToScalar[Int](_.argmax) // we can take advantage that our classes are 0-9 so argmax here returns the class label directly

  def loss(predicted: Array[Int], actual: Array[Int])= 
    (predicted =:= actual).trues.toDouble / predicted.length

  def update_params(w1: Matrix[Double], b1: Matrix[Double], w2: Matrix[Double], b2: Matrix[Double], dw1: Matrix[Double], db1: Matrix[Double], dw2: Matrix[Double], db2: Matrix[Double], alpha: Double) =
    val w1_ = w1 - (alpha * dw1)
    val b1_ = b1 - (alpha * db1)
    val w2_ = w2 - (alpha * dw2)
    val b2_ = b2 - (alpha * db2)
    (w1= w1_, b1= b1_, w2= w2_, b2= b2_)

  def gradient_decent( x: Matrix[Double] , y: Matrix[Double], iterations: Int, alpha: Double, decay_rate: 0.001, w1:Matrix[Double], b1:Matrix[Double], w2:Matrix[Double], b2:Matrix[Double]) =  
    var alpha_ = alpha
    var w1_ = w1
    var b1_ = b1
    var w2_ = w2
    var b2_ = b2

    
    for (i <- 1.until( iterations+1)) {      
      val one_hot_Y = one_hot_encoding(y)
      val (z1, a1, z2, a2) = foward_prop(w1, b1, w2, b2, x)
      val (dw1, db1, dw2, db2) = back_prop(w1, b1, w2, b2, z1, a1, z2, a2, X, one_hot_Y)
      var (w1_, b1_, w2_, b2_) = update_params(w1_, b1_, w2_, b2_, dw1, db1, dw2, db2, alpha_)
      // decay the learning rate, can experiment with different rates here.
      if ((i + 1) % 50 == 0)
        alpha_ = initial_alpha - decay_rate

      if (i % 10 == 0)
        println("iteration number: ", i)        
        val (_, _, _, a2) = foward_prop(w1, b1, w2, b2, x_train)
        val acc = loss(predictions(a2), y_train)
        println(s"Accuracy : $acc")
    }

    (w1 = w1_, b1 = b1_, w2 = w2_, b2 = b2_)


end mnist

/** Note that the y co-ordinate needs to be inverted as we are reading it here.
  */
def dataToCoords(data: Array[Double]): IndexedSeq[(x: Int, y: Int, opacity: Double)] =
  for (i <- 0.until(28); j <- 0.until(28)) yield
    val value = data(i * 28 + j)
    (x = j, y = 28 - i, opacity = value)
