import io.github.quafadas.table.*
import scala.util.chaining.*

import viz.PlotTargets.desktopBrowser
import viz.vegaFlavour
import viz.Plottable.*
import viz.Macros.Implicits.given_Writer_T
import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes
import vecxt.BoundsCheck
import scala.reflect.ClassTag
import narr.*

// File can be dowloaded from Kaggle at:
// https://www.kaggle.com/datasets/quangphota/mnist-csv/data?select=train.csv
// Not included to avoid repository bloat, you'll have to download to the resource directory and name it train.csv
// Follows the code here;
//

@main def mnist =
  def traindata = LazyList.from(os.read.lines(os.resource / "train.csv").iterator.drop(1).map { line =>
    line.split(",")
  })

  val samplePlot = false
  val trainSize = 60000

  val l1Size = 128
  val l2Size = 64
  val l3Size = 10 // output layer size, number of classes
  val imageWidth = 28
  val imageHeight = 28

  val labels = traindata.map(_.head.toInt)
  val others =
    traindata
      .map(_.tail.map(_.toDouble))
      .map(_.toList.toArray.map(_.toDouble / 255.0)) // x data, normalised to [0, 1]

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

  val weight1 = Matrix(
    Array.fill(imageHeight * imageWidth * l1Size)(scala.util.Random.nextDouble() * 0.2),
    (imageWidth * imageHeight, l1Size)
  )
  val bias1 = Array.fill(l1Size)(0.0)
  val weight2 = Matrix(Array.fill(l1Size * l3Size)(scala.util.Random.nextDouble() * 0.2), (l1Size, l3Size))
  val bias2 = Array.fill(l3Size)(0.0)

  val x = Matrix.fromRows(others.toArray*)
  println(s"x layout ${x.layout}")

  println(s"weight1 shape: ${weight1.shape}, weight1 rows: ${weight1.rows}, weight1 cols: ${weight1.cols}")
  println(s"weight2 shape: ${weight2.shape}, weight2 rows: ${weight2.rows}, weight2 cols: ${weight2.cols}")
  // val i = x @@ weight1 // This is just to check that the matrix multiplication works
  // println(s"i shape: ${i.shape}, i rows: ${i.rows}, i cols: ${i.cols}")
  // println(labels)
  val one_hot_Y = oneHotEncode(labels)

  // println(s"one_hot_Y shape: ${one_hot_Y.shape}, one_hot_Y rows: ${one_hot_Y.rows}, one_hot_Y cols: ${one_hot_Y.cols}")
  // println(one_hot_Y.printMat)

  // println(s"x shape: ${x.shape}, x rows: ${x.rows}, x cols: ${x.cols}")
  // println(s"10 rows, 100-150 cols of x: ${x(0 until 10, 100 until 150).printMat}")

  // println(s"initial weights: ${weight1(0 to 10, ::).printMat}")
  // println(s"initial biases: ${bias1.mkString(", ")}")
  // println(s"initial weights2: ${weight2(0 until 10, ::).printMat}")
  // println(s"initial biases2: ${bias2.mkString(", ")}")
  // println(s"labels: ${labels.take(10).mkString(", ")}")

  val arg = gradient_decent(
    x = x,
    y = one_hot_Y,
    labels = labels.toArray,
    iterations = 10,
    batchSize = 120,
    alpha = 0.05,
    decayRate = 0.0125,
    w1 = weight1,
    b1 = bias1,
    w2 = weight2,
    b2 = bias2
  )

  println(arg)

end mnist

def dataToCoords(data: Array[Double]): IndexedSeq[(x: Int, y: Int, opacity: Double)] =
  for (i <- 0.until(28); j <- 0.until(28)) yield
    val value = data(i * 28 + j)
    (x = j, y = 28 - i, opacity = value)

// -- Below here is the neural network machinery

inline def reluM(z: Matrix[Double]): Matrix[Double] = Matrix(z.raw.clampMin(0.0), z.shape)

inline def softmaxRows(z: Matrix[Double]): Matrix[Double] =
  z.mapRows { row =>
    row -= row.max
    row.`exp!`
    row /= row.sum
    row
  }

def foward_prop(w1: Matrix[Double], b1: Array[Double], w2: Matrix[Double], b2: Array[Double], x: Matrix[Double]) =
  // println("forward propagation ----")
  // println(s"x shape: ${x.shape}, x rows: ${x.rows}, x cols: ${x.cols}")
  // println(s"weight1 shape: ${w1.shape}, weight1 rows: ${w1.rows}, weight1 cols: ${w1.cols}")
  // println(s"weight2 shape: ${w2.shape}, weight2 rows: ${w2.rows}, weight2 cols: ${w2.cols}")

  // println(s"m:  ${x.layout}")
  // println(s"b: ${w1.layout}")
  val z1 = (x @@ w1)
  z1.mapRowsInPlace(r => r.tap(_ += b1))
  //  println(s"z1 shape: ${z1.shape}, z1 rows: ${z1.rows}, z1 cols: ${z1.cols}")
  //  println(s"z1 mean: ${z1.raw.mean}, min: ${z1.raw.minSIMD}, max: ${z1.raw.maxSIMD}")
  //  println(s"z1: ${z1(0 to 10, ::).printMat}")
  val a1 = reluM(z1) // get rid of negative values
  // println(s"a1 shape: ${a1.shape}, a1 rows: ${a1.rows}, a1 cols: ${a1.cols}")
  // println(s"a1: ${a1(0 to 10, ::).printMat}")
  val z2 = (a1 @@ w2)
  z2.mapRowsInPlace(r => r.tap(_ += b2)) // results [(rows, 10) @ (10, 10)] = (rows, 10)
  // println(s"z2 shape: ${z2.shape}, z2 rows: ${z2.rows}, z2 cols: ${z2.cols}")
  // println(s"z2: ${z2(0 to 10, ::).printMat}")
  // println(s"z2: ${z2.raw.take(10).mkString(", ")}")
  val a2 = softmaxRows(z2)
  // println(s"a2 shape: ${a2.shape}, a2 rows: ${a2.rows}, a2 cols: ${a2.cols}, a2 rowStride: ${a2.rowStride}, a2 colStride: ${a2.colStride} a2 offset: ${a2.offset}")
  // println("forward propagation done ----")
  (z1 = z1, a1 = a1, z2 = z2, a2 = a2)
end foward_prop

def back_prop(
    w1: Matrix[Double],
    b1: Array[Double],
    w2: Matrix[Double],
    b2: Array[Double],
    z1: Matrix[Double],
    a1: Matrix[Double],
    z2: Matrix[Double],
    a2: Matrix[Double],
    X: Matrix[Double],
    Y: Matrix[Double]
) =
  // println("back propagation ----")
  val m = Y.rows
  val m_inv = 1.0 / m
  // println(s"m: $m, m_inv: $m_inv")
  // println(s"Y shape: ${Y.shape}, Y rows: ${Y.rows}, Y cols: ${Y.cols} y colStride: ${Y.colStride}, y rowStride: ${Y.rowStride}, y offset: ${Y.offset}")
  // println(a2.layout)
  // println(Y.layout)

  val dz2 = a2 - Y
  val dw2 = m_inv * (a1.transpose @@ dz2)
  // println(s"dz2 shape: ${dz2.shape}, dz2 rows: ${dz2.rows}, dz2 cols: ${dz2.cols}")
  // println(s"dw2 shape: ${dw2.shape}, dw2 rows: ${dw2.rows}, dw2 cols: ${dw2.cols}")
  val db2 = dz2.mapColsToScalar(_.sum).raw
  val dz1Check = (z1 > 0)
  // println(s"dz2 shape: ${dz2.shape}, dz2 rows: ${dz2.rows}, dz2 cols: ${dz2.cols}\n")
  // println(s"dz1Check: ${dz1Check.shape}, dz1Check rows: ${dz1Check.rows}, dz1Check cols: ${dz1Check.cols}\n"``)
  // println(s"dz1Check: ${dz1Check(0 to 10, ::).printMat}\n")
  val dz1 = (dz2 @@ w2.transpose)
  dz1 *:*= dz1Check // (10, 784)
  // print(s"dz1 shape: ${dz1.shape}, dz1 rows: ${dz1.rows}, dz1 cols: ${dz1.cols}\n")
  val dw1 = m_inv * (X.transpose @@ dz1)
  val db1 = dz1.mapColsToScalar(r => r.sumSIMD * m_inv).raw
  // println("back propagation done ----")
  (dw1 = dw1, db1 = db1, dw2 = dw2, db2 = db2)
end back_prop

inline def oneHot[T](int: Int, numClasses: Int)(using ct: ClassTag[T], f: Numeric[T]): Array[T] =
  val arr = NArray.fill[T](numClasses)(f.zero)
  if int >= 0 && int < numClasses then arr(int) = f.one
  end if
  arr
end oneHot

def oneHotEncode(labels: Seq[Int]): Matrix[Double] =
  val n = labels.length
  val m = 10 // number of classes
  val oneHot = Array.fill(n * m)(0.0)
  var i = 0
  // column major. Could also be done with Matrix.fromRows... but that would be less efficient
  while i < labels.length do
    oneHot(i + n * labels(i)) = 1.0
    i += 1
  end while
  println("One hot encoding done")
  Matrix(oneHot, (n, m))
end oneHotEncode

def mostLikely(weights: Matrix[Double]): Array[Int] =
  // val m = weights.raw
  weights
    .mapRowsToScalar[Int](_.argmax)
    .raw // we can take advantage that our classes are 0-9 so argmax here returns the class label directly
end mostLikely

def loss(predicted: Array[Int], actual: Array[Int]) =
  (predicted =:= actual).trues.toDouble / predicted.length

def gradient_decent(
    x: Matrix[Double],
    y: Matrix[Double],
    labels: Array[Int],
    iterations: Int,
    batchSize: Int,
    alpha: Double,
    decayRate: Double,
    w1: Matrix[Double],
    b1: Array[Double],
    w2: Matrix[Double],
    b2: Array[Double]
) =
  import BoundsCheck.DoBoundsCheck.yes
  println("Starting gradient descent...")
  println(s"alpha: $alpha, decay_rate: $decayRate, iterations: $iterations")
  val numEpochs = x.rows / batchSize
  var alpha_ = alpha
  var w1_ = w1.deepCopy
  var b1_ = b1.clone()
  var w2_ = w2.deepCopy
  var b2_ = b2.clone()

  for i <- 1.until(iterations + 1) do
    for j <- 0.until(numEpochs) do
      // println(s"Epoch: $j, iteration: $i, alpha: $alpha_")
      val start = j * batchSize
      val end = start + batchSize
      val range = start.until(end)
      // println(range.toArray.mkString(", "))
      val xBatch = x(range, ::)
      val yBatch = y(range, ::)
      // println(s"xBatch shape: ${xBatch.shape}, xBatch rows: ${xBatch.rows}, xBatch cols: ${xBatch.cols}")
      // println(s"yBatch shape: ${yBatch.shape}, yBatch rows: ${yBatch.rows}, yBatch cols: ${yBatch.cols}")

      // assert(xBatch.raw  == x.raw)
      // println("xbatch.raw: " + xBatch.raw.length)

      val (z1, a1, z2, a2) = foward_prop(w1_, b1_, w2_, b2_, xBatch)
      val (dw1, db1, dw2, db2) = back_prop(w1_, b1_, w2_, b2_, z1, a1, z2, a2, xBatch, yBatch)
      w1_ -= (dw1 * alpha_)
      // println(s"dw1 shape: ${dw1.shape}, dw1 rows: ${dw1.rows}, dw1 cols: ${dw1.cols}")
      // println(s"dw1: ${dw1(0 to 10, ::).printMat}")
      // println(s"w1_: ${w1_(0 to 10, ::).printMat}")

      b1_ -= (db1 * alpha_)
      w2_ -= (dw2 * alpha_)
      b2_ -= (db2 * alpha_)

    end for
    // decay the learning rate, can experiment with different rates here.
    if (i + 1) % 25 == 0 then alpha_ = alpha_ - decayRate
    end if

    if i % 1 == 0 then
      val (_, _, _, a2) = foward_prop(w1_, b1_, w2_, b2_, x)
      val acc = loss(mostLikely(a2), labels)
      println(s"Iteration: $i, alpha: $alpha_")
      println(s"Accuracy : $acc")
    end if
  end for

  val (_, _, _, a2) = foward_prop(w1_, b1_, w2_, b2_, x)
  println(s"iterations: $iterations, alpha: $alpha_, samples: ${x.rows}, classes: ${w2_.cols}")
  // println(s"w1_ shape: ${w1_.shape}, w1_ rows: ${w1_.rows}, w1_ cols: ${w1_.cols}, ${w1.raw.take(10).printArr}")
  // println(s"b1_ shape: ${b1_.length}, b1_ values: ${b1_.mkString(", ")}")
  // println(s"w2_ shape: ${w2_.shape}, w2_ rows: ${w2_.rows}, w2_ cols: ${w2_.cols}, ${w2.raw.take(10).printArr}")
  // println(s"b2_ shape: ${b2_.length}, b2_ values: ${b2_.mkString(", ")}")
  // println(s"a2 shape: ${a2.shape}, a2 rows: ${a2.rows}, a2 cols: ${a2.cols}")
  // println(s"a2: ${a2(::, ::).printMat}")

  println(s"Final accuracy: ${loss(mostLikely(a2), labels)}")

  println(s"weights1 first row: ${w1(Array(0), ::).printMat}")
  println(s"weights1 shape: ${w1_.shape}")
  println(s"weights2: ${w2_.printMat}")

  import java.io.PrintWriter

  def writeMatrixToFile(matrix: Matrix[Double], filename: String): Unit =
    val pw = new PrintWriter(filename)
    try
      matrix.raw.grouped(matrix.rows).foreach { col =>
        pw.println(col.mkString(","))
      }
    finally pw.close()
    end try
  end writeMatrixToFile

  writeMatrixToFile(w1_, "weights1.csv")
  writeMatrixToFile(w2_, "weights2.csv")
  def writeArrayToFile(array: Array[Double], filename: String): Unit =
    val pw = new PrintWriter(filename)
    try
      pw.println(array.mkString(","))
    finally pw.close()
    end try
  end writeArrayToFile

  writeArrayToFile(b1_, "biases1.csv")
  writeArrayToFile(b2_, "biases2.csv")

  println("Weights and biases saved to weights1.csv and weights2.csv")

  println("Gradient descent finished.")

  (w1 = w1_, b1 = b1_, w2 = w2_, b2 = b2_)
end gradient_decent
