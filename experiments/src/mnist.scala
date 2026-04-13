
import scala.util.chaining.*

import io.github.quafadas.plots.SetupVegaBrowser.{*, given}
import io.circe.syntax.*

import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes
import vecxt.BoundsCheck
import scala.reflect.ClassTag
import vecxt_io.MatrixIO
import vecxt_io.MatrixIO.*
import vecxt_io.ArrayIO.*
import vecxt.JvmIntMatrix./
import scala.annotation.targetName

// File can be dowloaded from Kaggle at:
// https://www.kaggle.com/datasets/quangphota/mnist-csv/data?select=train.csv
// Not included to avoid repository bloat, you'll have to download to the resource directory and name it train.csv
// Follows the code here;
//

@main def mnist =
  val traindata: Matrix[Int] = MatrixIO.fromResource[Int]("train.csv", ',', dropRows = 1)

  val samplePlot = false
  val shapeDiagnostic = true

  val trainSize = traindata.rows

  val l1Size = 128 //
  val l2Size = 64
  val l3Size = 10 // output layer size, number of classes
  val imageWidth = 28
  val imageHeight = 28

  val labels: Array[Int] = traindata.col(0) // y data, the labels are in the first column
  val pixelData : Matrix[Float] = traindata(::,1 until traindata.cols).deepCopy / 255.0f

  if samplePlot then
    VegaPlot.fromResource("hist.vg.json").plot(
      _.data._0.values := labels.map(label => (u = label)).asJson
    )
  end if

  val one_hot_Y = oneHotEncode[Float](labels)

  if samplePlot then
    val random = scala.util.Random

    Iterator.continually(random.nextInt(trainSize)).take(5).foreach { idx =>
      val data = pixelData.row(idx)
      val pixelSize = 10
      val d = dataToCoords(data, pixelSize)
      VegaPlot.fromResource("pixelPlot.vg.json").plot(
        _.data.values := d.asJson,
        _.mark.width := pixelSize,
        _.mark.height := pixelSize,
        _.width := imageWidth * pixelSize,
        _.height := imageHeight * pixelSize,
        _.title := s"Sample: $idx - Label ${labels(idx)}"
      )
      one_hot_Y.row(idx).mkString(", ").tap(str => println(s"One hot encoding for sample $idx:, label ${labels(idx)}: $str"))
    }

  end if

  val scale = 0.2f
  val startBias = 0.0f

  val weight1 = Matrix(
    Array.fill(imageHeight * imageWidth * l1Size)(scala.util.Random.nextFloat() * scale),
    (imageWidth * imageHeight, l1Size)
  )
  val bias1 = Array.fill(l1Size)(startBias)
  val weight2 = Matrix(Array.fill(l1Size * l3Size)(scala.util.Random.nextFloat() * scale), (l1Size, l3Size))
  val bias2 = Array.fill(l3Size)(startBias)

  if shapeDiagnostic then
    println(s"pixelData shape: ${pixelData.shape}, pixelData rows: ${pixelData.rows}, pixelData cols: ${pixelData.cols}, pixelData rowStride: ${pixelData.rowStride}, pixelData colStride: ${pixelData.colStride} pixelData offset: ${pixelData.offset}")

    println(s"x layout ${pixelData.layout}")

    println(s"weight1 shape: ${weight1.shape}, weight1 rows: ${weight1.rows}, weight1 cols: ${weight1.cols}")
    println(s"weight2 shape: ${weight2.shape}, weight2 rows: ${weight2.rows}, weight2 cols: ${weight2.cols}")

  end if
  // val i = x @@ weight1 // This is just to check that the matrix multiplication works
  // println(s"i shape: ${i.shape}, i rows: ${i.rows}, i cols: ${i.cols}")
  // println(labels)
  // println(s"one_hot_Y shape: ${one_hot_Y.shape}, one_hot_Y rows: ${one_hot_Y.rows}, one_hot_Y cols: ${one_hot_Y.cols}")
  // println(one_hot_Y.printMat)

  // println(s"x shape: ${x.shape}, x rows: ${x.rows}, x cols: ${x.cols}")
  // println(s"10 rows, 100-150 cols of x: ${x(0 until 10, 100 until 150).printMat}")

  // println(s"initial weights: ${weight1(0 to 10, ::).printMat}")
  // println(s"initial biases: ${bias1.mkString(", ")}")
  // println(s"initial weights2: ${weight2(0 until 10, ::).printMat}")
  // println(s"initial biases2: ${bias2.mkString(", ")}")
  // println(s"labels: ${labels.take(10).mkString(", ")}")

  gradient_descentf(
    x = pixelData,
    y = one_hot_Y,
    labels = labels.toArray,
    iterations = 5,
    batchSize = 120,
    alpha = 0.05f,
    decayRate = 0.0125f,
    w1 = weight1,
    b1 = bias1,
    w2 = weight2,
    b2 = bias2
  )


end mnist

// Helper methods.

def dataToCoords[A](data: Array[A], pixelSize: Int): IndexedSeq[(x: Int, y: Int, opacity: A)] =
  for i <- 0.until(28); j <- 0.until(28) yield
    val value = data(i * 28 + j)
    (x = j * pixelSize, y = (28 - i) * pixelSize, opacity = value)

// -- Below here is the neural network machinery

inline def reluM(z: Matrix[Double]): Matrix[Double] = Matrix(z.raw.clampMin(0.0), z.shape)

@targetName("reluMFloat")
inline def reluM(z: Matrix[Float]): Matrix[Float] = Matrix(z.raw.clampMin(0.0), z.shape)

inline def softmaxRows(z: Matrix[Double]): Matrix[Double] =
  z.mapRows { row =>
    row -= row.max
    row.`exp!`
    row /= row.sum
    row
  }

@targetName("softmaxRowsFloat")
inline def softmaxRows(z: Matrix[Float]): Matrix[Float] =
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
  // z1 += b1
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

// Float version
def foward_prop(w1: Matrix[Float], b1: Array[Float], w2: Matrix[Float], b2: Array[Float], x: Matrix[Float]) =
	val z1 = (x @@ w1)
	z1.mapRowsInPlace(r => r.tap(_ += b1))
	val a1 = reluM(z1) // get rid of negative values
	val z2 = (a1 @@ w2)
	z2.mapRowsInPlace(r => r.tap(_ += b2)) // results [(rows, 10) @ (10, 10)] = (rows, 10)
	val a2 = softmaxRows(z2)
	(z1 = z1, a1 = a1, z2 = z2, a2 = a2)
end foward_prop


def back_prop(
		w1: Matrix[Float],
		b1: Array[Float],
		w2: Matrix[Float],
		b2: Array[Float],
		z1: Matrix[Float],
		a1: Matrix[Float],
		z2: Matrix[Float],
		a2: Matrix[Float],
		X: Matrix[Float],
		Y: Matrix[Float]
) =
	println("back propagation ----")
	val m = Y.rows
	val m_inv = 1.0f / m
	println(s"m: $m, m_inv: $m_inv")
	println(s"Y shape: ${Y.shape}, Y rows: ${Y.rows}, Y cols: ${Y.cols} y colStride: ${Y.colStride}, y rowStride: ${Y.rowStride}, y offset: ${Y.offset}")
	println(a2.layout)
	println(Y.layout)

	val dz2 = a2 - Y
	val dw2 = m_inv * (a1.transpose @@ dz2)
	println(s"dz2 shape: ${dz2.shape}, dz2 rows: ${dz2.rows}, dz2 cols: ${dz2.cols}")
	println(s"dw2 shape: ${dw2.shape}, dw2 rows: ${dw2.rows}, dw2 cols: ${dw2.cols}")
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
  val arr = Array.fill[T](numClasses)(f.zero)
  if int >= 0 && int < numClasses then arr(int) = f.one
  end if
  arr
end oneHot

def oneHotEncode[A](labels: Seq[Int])(using tc: Numeric[A], ct: ClassTag[A]): Matrix[A] =
  val n = labels.length
  val m = 10 // number of classes
  val oneHot = Array.fill(n * m)(tc.zero)
  var i = 0
  // column major. Could also be done with Matrix.fromRows... but that would be less efficient
  while i < labels.length do
    oneHot(i + n * labels(i)) = tc.one
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

@targetName("mostLikelyFloat")
def mostLikely(weights: Matrix[Float]): Array[Int] =
  weights
    .mapRowsToScalar[Int](_.argmax)
    .raw // we can take advantage that our classes are 0-9 so argmax here returns the class label directly
end mostLikely

def loss(predicted: Array[Int], actual: Array[Int]) =
  (predicted =:= actual).trues.toDouble / predicted.length

def gradient_descentf(
  x: Matrix[Float],
  y: Matrix[Float],
  labels: Array[Int],
  iterations: Int,
  batchSize: Int,
  alpha: Float,
  decayRate: Float,
  w1: Matrix[Float],
  b1: Array[Float],
  w2: Matrix[Float],
  b2: Array[Float]
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

  // println(s"Final accuracy: ${loss(mostLikely(a2), labels)}")

  // println(s"weights1 first row: ${w1(Array(0), ::).printMat}")
  // println(s"weights1 shape: ${w1_.shape}")
  // println(s"weights2: ${w2_.printMat}")

  import java.io.PrintWriter
  val w1_tmp = os.temp.dir() / "weights1.csv"
  val w2_tmp = os.temp.dir() / "weights2.csv"
  val b1_tmp = os.temp.dir() / "biases1.csv"
  val b2_tmp = os.temp.dir() / "biases2.csv"

  println(s"Saving weights to ${w1_tmp} and ${w2_tmp}")

  w1_.write(w1_tmp)
  w2_.write(w2_tmp)

  b1_.write(b1_tmp)
  b2_.write(b2_tmp)

  println(s"Weights and biases saved to ${w1_tmp}, ${w2_tmp}, ${b1_tmp}, and ${b2_tmp}")

  println("Gradient descent finished.")

  (w1 = w1_, b1 = b1_, w2 = w2_, b2 = b2_)

end gradient_descentf

def gradient_descent(
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

  // println(s"Final accuracy: ${loss(mostLikely(a2), labels)}")

  // println(s"weights1 first row: ${w1(Array(0), ::).printMat}")
  // println(s"weights1 shape: ${w1_.shape}")
  // println(s"weights2: ${w2_.printMat}")

  import java.io.PrintWriter
  val w1_tmp = os.temp.dir() / "weights1.csv"
  val w2_tmp = os.temp.dir() / "weights2.csv"
  val b1_tmp = os.temp.dir() / "biases1.csv"
  val b2_tmp = os.temp.dir() / "biases2.csv"

  println(s"Saving weights to ${w1_tmp} and ${w2_tmp}")

  w1_.write(w1_tmp)
  w2_.write(w2_tmp)

  b1_.write(b1_tmp)
  b2_.write(b2_tmp)

  println(s"Weights and biases saved to ${w1_tmp}, ${w2_tmp}, ${b1_tmp}, and ${b2_tmp}")

  println("Gradient descent finished.")

  (w1 = w1_, b1 = b1_, w2 = w2_, b2 = b2_)
end gradient_descent
