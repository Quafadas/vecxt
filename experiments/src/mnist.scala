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

// Not ioncluded to aboid repository bloat. 
@main def mnist = 
  def traindata = CSV.resource("train.csv")

  val samplePlot = true
  
  
  val labels = traindata.column["label"].map(_.toInt).toSeq
  val others = traindata.dropColumn["label"].map(_.toList.toArray.map(_.toDouble))

  println(labels.take(10).toSeq.mkString(", "))

  if(samplePlot) {
    (os.resource / "hist.vg.json").plot(
      List(
        spec => {
          spec("data")(0)("values") = upickle.default.writeJs(labels.map(i => (u = i)))
        }
      )
    )
  }

  if(samplePlot) 
    others.take(5).foreach(
      data => 
        val d = dataToCoords(data)
        (os.resource / "pixelPlot.vg.json").plot(
          List(
            spec => spec("data")("values") = upickle.default.writeJs(d)
          )
        )
    )

  
  val network = Matrix(Array.fill(28*28)(scala.util.Random.nextDouble()), (28, 28))
  val b1 = Array.fill(10)(0.0)
  val w2 = Array.tabulate(10)( i => i.toDouble / 10.0)
  val b2 = Array.fill(10)(0.0)


  def relu(z: Array[Double]) = z.max(0.0)



/**
 * Note that the y co-ordinate needs to be inverted as we are reading it here.
 */
def dataToCoords(data: Array[Double]): IndexedSeq[(x: Int, y: Int, opacity: Double)] = 
  for (i <- 0.until( 28); j <- 0.until( 28)) yield {
    val value = data(i * 28 + j)
    (x = j, y = 28-i, opacity = value)
  }


