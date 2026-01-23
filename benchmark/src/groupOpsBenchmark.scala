/*
 * Benchmark for isolated group operations (groupCumSum, groupDiff, groupSum)
 * Testing performance of these hot-path operations
 */

package vecxt.benchmark

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import scala.compiletime.uninitialized
import vecxtensions.*
import vecxt_re.*
import java.util.Random
import java.util.concurrent.TimeUnit

// ./mill benchmark.runJmh "vecxt.benchmark.LossReportBenchmark" -jvmArgs --add-modules=jdk.incubator.vector -rf json -wi 1 -i 3 -f 1

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class GroupOpsBenchmark:

  @Param(Array("100", "1000", "10000", "100000"))
  var size: String = uninitialized

  @Param(Array("10", "100", "1000"))
  var groupSize: String = uninitialized

  var groups: Array[Int] = uninitialized
  var values: Array[Double] = uninitialized
  var valuesCopy1: Array[Double] = uninitialized
  var valuesCopy2: Array[Double] = uninitialized

  @Setup(Level.Invocation)
  def setup: Unit =
    val n = size.toInt
    val avgGroupSize = groupSize.toInt
    val numGroups = math.max(1, n / avgGroupSize)

    val random = new Random(42) // Fixed seed for reproducibility

    // Generate sorted groups array with realistic group sizes
    groups = new Array[Int](n)
    var currentGroup = 0
    var i = 0
    while i < n do
      val remainingElements = n - i
      val remainingGroups = numGroups - currentGroup
      val targetGroupSize =
        if remainingGroups > 0 then remainingElements / remainingGroups
        else remainingElements

      val actualGroupSize = math.max(1, targetGroupSize + random.nextInt(avgGroupSize / 2 + 1) - avgGroupSize / 4)
      val groupEnd = math.min(i + actualGroupSize, n)

      while i < groupEnd do
        groups(i) = currentGroup
        i += 1
      end while

      currentGroup += 1
    end while

    // Generate random values
    values = Array.fill(n)(random.nextDouble() * 100.0)
    valuesCopy1 = values.clone()
    valuesCopy2 = values.clone()
  end setup

  @Benchmark
  def benchGroupCumSum(bh: Blackhole): Unit =
    val result = groupCumSum(groups, values)
    bh.consume(result)
  end benchGroupCumSum

  @Benchmark
  def benchGroupCumSumInPlace(bh: Blackhole): Unit =
    groupCumSumInPlace(groups, valuesCopy1)
    bh.consume(valuesCopy1)
  end benchGroupCumSumInPlace

  @Benchmark
  def benchGroupDiff(bh: Blackhole): Unit =
    val result = groupDiff(groups, values)
    bh.consume(result)
  end benchGroupDiff

  @Benchmark
  def benchGroupDiffInPlace(bh: Blackhole): Unit =
    groupDiffInPlace(groups, valuesCopy2)
    bh.consume(valuesCopy2)
  end benchGroupDiffInPlace

  // @Benchmark
  // def benchGroupSum(bh: Blackhole): Unit =
  //   // groupSum in vecxt_re takes nitr; compute number of groups then call it
  //   var maxGroup = 0
  //   var i = 0
  //   while i < groups.length do
  //     if groups(i) > maxGroup then maxGroup = groups(i)
  //     end if
  //     i += 1
  //   end while
  //   val numGroups = maxGroup + 1
  //   val sums = groupSum(groups, values, numGroups)
  //   val uniqueGroups = Array.tabulate(numGroups)(identity)
  //   bh.consume(uniqueGroups)
  //   bh.consume(sums)
  // end benchGroupSum

end GroupOpsBenchmark
