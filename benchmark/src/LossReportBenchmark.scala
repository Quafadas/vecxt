package vecxt.benchmark

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import scala.compiletime.uninitialized
import scala.util.Random
import vecxt_re.*
import vecxt_re.ReReporting.*
import vecxt.all.*

// ./mill benchmark.runJmh "vecxt.benchmark.LossReportBenchmark" -jvmArgs --add-modules=jdk.incubator.vector -rf json -wi 1 -i 3 -f 1

/**
  * 231] Benchmark                                (numEventsStr)  (numIterationsStr)   Mode  Cnt       Score       Error  Units
231] LossReportBenchmark.lossReport_fast               10000                 100  thrpt    3  177346.981 ± 24137.324  ops/s
231] LossReportBenchmark.lossReport_fast               10000                1000  thrpt    3  180400.504 ±  8719.687  ops/s
231] LossReportBenchmark.lossReport_fast              100000                 100  thrpt    3   11731.510 ±  1945.957  ops/s
231] LossReportBenchmark.lossReport_fast              100000                1000  thrpt    3   17443.246 ±   425.030  ops/s
231] LossReportBenchmark.lossReport_separate           10000                 100  thrpt    3   46850.187 ±  7232.734  ops/s
231] LossReportBenchmark.lossReport_separate           10000                1000  thrpt    3   49876.719 ±  5238.487  ops/s
231] LossReportBenchmark.lossReport_separate          100000                 100  thrpt    3    3360.234 ±   326.993  ops/s
231] LossReportBenchmark.lossReport_separate          100000                1000  thrpt    3    4706.819 ±   615.832  ops/s
  */



@State(Scope.Thread)
class LossReportBenchmark extends BLASBenchmark:

  @Param(Array("10000", "100000"))
  var numEventsStr: String = uninitialized

  @Param(Array("100", "1000"))
  var numIterationsStr: String = uninitialized

  var years: Array[Int] = uninitialized
  var ceded: Array[Double] = uninitialized
  var layerObj: Layer = uninitialized

  @Setup(Level.Trial)
  def setup: Unit =
    val rng = new Random(0)
    val numEvents = numEventsStr.toInt
    val numIterations = numIterationsStr.toInt

    val yrs = Array.ofDim[Int](numEvents)
    var i = 0
    while i < numEvents do
      yrs(i) = rng.nextInt(numIterations) + 1 // 1-based group indices
      i += 1
    end while

    java.util.Arrays.sort(yrs)

    years = yrs

    ceded = Array.ofDim[Double](numEvents)
    i = 0
    while i < numEvents do
      // random loss values between 0 and 100
      ceded(i) = rng.nextDouble() * 100.0
      i += 1
    end while

    // Choose a layer with a moderate aggLimit to cause some exhaustion hits
    layerObj = Layer(occLimit = Some(100.0), aggLimit = Some(50.0))
    ()
  end setup

  @Benchmark
  def lossReport_fast(bh: Blackhole) =
    val calcd = (layerObj, ceded)
    val r = calcd.lossReport(numIterationsStr.toInt, years, ReportDenominator.FirstLimit)
    // consume fields so JMH doesn't optimize away
    bh.consume(r.el)
    bh.consume(r.stdDev)
    bh.consume(r.attachProb)
    bh.consume(r.exhaustProb)
  end lossReport_fast

  @Benchmark
  def lossReport_separate(bh: Blackhole) =
    val calcd = (layerObj, ceded)
    val n = numIterationsStr.toInt
    val reportLimit = ReportDenominator.FirstLimit.fromlayer(layerObj)

    val el = calcd.expectedLoss(n) / reportLimit
    val std = calcd.std(n, years) / reportLimit
    val attach = calcd.attachmentProbability(n, years)
    val exhaust = calcd.exhaustionProbability(n, years)

    bh.consume(el)
    bh.consume(std)
    bh.consume(attach)
    bh.consume(exhaust)
  end lossReport_separate

end LossReportBenchmark
