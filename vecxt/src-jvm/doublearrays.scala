package vecxt

import scala.reflect.ClassTag

import vecxt.annotations.AllocFree
import vecxt.annotations.HotPath
import vecxt.annotations.Thin

import vecxt.matrix.Matrix

import dev.ludovic.netlib.blas.JavaBLAS.getInstance as blas
import jdk.incubator.vector.ByteVector
import jdk.incubator.vector.DoubleVector
import jdk.incubator.vector.IntVector
import jdk.incubator.vector.VectorMask
import jdk.incubator.vector.VectorOperators

object doublearrays:

  final val spi = IntVector.SPECIES_PREFERRED
  final val spd = DoubleVector.SPECIES_PREFERRED
  final val spb = ByteVector.SPECIES_PREFERRED

  final val spdl = spd.length()
  final val spbl = spb.length()
  final val spil = spi.length()

  final val iota: DoubleVector = DoubleVector.fromArray(spd, Array.tabulate(spdl)(_.toDouble), 0)

  /** Zero-allocation: fills a caller-owned buffer. */
  @HotPath
  @AllocFree
  def fillLinspace(dest: Array[Double], a: Double, b: Double): Unit =
    val n = dest.length
    if n == 1 then dest(0) = a
    else if n > 1 then
      val increment = (b - a) / (n - 1)
      val step = iota.mul(increment) // loop-invariant: [0, inc, 2*inc, ...]
      val bound = spd.loopBound(n)

      var i = 0
      while i < bound do
        step.add(a + increment * i).intoArray(dest, i)
        i += spdl
      end while

      while i < n do
        dest(i) = a + increment * i
        i += 1
      end while

      dest(n - 1) = b // exact endpoint, not a * (n-1) rounding artefact
    end if
  end fillLinspace

  /** Generates a vector of linearly spaced values between a and b (inclusive). The returned vector will have length
    * elements, defaulting to 100.
    */
  inline def linspace(a: Double, b: Double, length: Int = 100): Array[Double] =
    val out = new Array[Double](length)
    fillLinspace(out, a, b)
    out
  end linspace

  extension (d: Double)
    def /(arr: Array[Double]) =
      val out = new Array[Double](arr.length)
      val bound = spd.loopBound(arr.length)
      var i = 0
      while i < bound do
        DoubleVector.broadcast(spd, d).div(DoubleVector.fromArray(spd, arr, i)).intoArray(out, i)
        i += spdl
      end while

      while i < arr.length do
        out(i) = d / arr(i)
        i = i + 1
      end while
      out
    end /

    inline def +(arr: Array[Double]): Array[Double] = arr.+(d)

    def -(arr: Array[Double]): Array[Double] =
      val out = new Array[Double](arr.length)
      var i = 0
      val bd = DoubleVector.broadcast(spd, d)
      val bound = spd.loopBound(arr.length)
      while i < bound do
        bd.sub(DoubleVector.fromArray(spd, arr, i)).intoArray(out, i)
        i += spdl
      end while

      while i < arr.length do
        out(i) = d - arr(i)
        i = i + 1
      end while
      out
    end -

    inline def *(arr: Array[Double]): Array[Double] = arr.*(d)

  end extension

  extension (vec: Array[Double])

    /** Apparently, left packing is hard problem in SIMD land.
      * https://stackoverflow.com/questions/79025873/selecting-values-from-java-simd-doublevector
      */

    // inline def apply(index: Array[Boolean]): Array[Double] =
    //   dimCheck(vec, index)
    //   val newVec: Array[Double] = new Array[Double](index.length)
    //   val out = new Array[Double](vec.length)
    //   val sp = Matrix.doubleSpecies
    //   val l = sp.length()

    //   var i = 0
    //   var j = 0
    //   while i < sp.loopBound(vec.length) do
    //     println(s"i: $i  || j: $j")
    //     val mask = VectorMask.fromArray[java.lang.Double](sp, index, i)

    //     val vals = DoubleVector
    //       .fromArray(sp, vec, i)

    //     // val selected = vals.selectFrom(vals, mask)

    //     println(s"mask: ${mask.toArray().print}")
    //     println(s"vals: ${vals.toArray().print}")
    //     vals.intoArray(newVec, j, mask)
    //     println(newVec.print)

    //     i += l
    //     j = j + mask.trueCount()

    //   end while

    //   while i < vec.length do
    //     if index(i) then
    //       newVec(j) = vec(i)
    //       j += 1
    //     end if
    //     i += 1
    //   end while

    //   newVec

    // end apply
    private inline def unaryOp(inline op: VectorOperators.Unary): Unit =
      var i = 0
      val bound = spd.loopBound(vec.length)
      while i < bound do
        DoubleVector
          .fromArray(spd, vec, i)
          .lanewise(op)
          .intoArray(vec, i)
        i += spdl
      end while

      if i < vec.length then
        val mask = VectorMask.fromLong(spd, (1L << (vec.length - i)) - 1)
        DoubleVector
          .fromArray(spd, vec, i, mask)
          .lanewise(op)
          .intoArray(vec, i, mask)
      end if
    end unaryOp

    @HotPath
    def unary_- : Array[Double] =
      val out = vec.clone()
      out.unaryOp(VectorOperators.NEG)
      out
    end unary_-

    @HotPath
    @AllocFree
    def -! : Unit =
      unaryOp(VectorOperators.NEG)

    @HotPath
    def abs: Array[Double] =
      val out = vec.clone()
      out.unaryOp(VectorOperators.ABS)
      out
    end abs

    @HotPath
    @AllocFree
    def `abs!`: Unit =
      unaryOp(VectorOperators.ABS)

    @HotPath
    def acos: Array[Double] =
      val out = vec.clone()
      out.unaryOp(VectorOperators.ACOS)
      out
    end acos

    @HotPath
    def `acos!`: Unit =
      unaryOp(VectorOperators.ACOS)

    @HotPath
    def asin: Array[Double] =
      val out = vec.clone()
      out.unaryOp(VectorOperators.ASIN)
      out
    end asin

    @HotPath
    def `asin!`: Unit =
      unaryOp(VectorOperators.ASIN)

    @HotPath
    def atan: Array[Double] =
      val out = vec.clone()
      out.unaryOp(VectorOperators.ATAN)
      out
    end atan

    @HotPath
    def `atan!`: Unit =
      unaryOp(VectorOperators.ATAN)

    @HotPath
    def cbrt: Array[Double] =
      val out = vec.clone()
      out.unaryOp(VectorOperators.CBRT)
      out
    end cbrt

    @HotPath
    def `cbrt!`: Unit =
      unaryOp(VectorOperators.CBRT)

    @HotPath
    def cos: Array[Double] =
      val out = vec.clone()
      out.unaryOp(VectorOperators.COS)
      out
    end cos

    @HotPath
    def `cos!`: Unit =
      unaryOp(VectorOperators.COS)

    @HotPath
    def cosh: Array[Double] =
      val out = vec.clone()
      out.unaryOp(VectorOperators.COSH)
      out
    end cosh

    @HotPath
    def `cosh!`: Unit =
      unaryOp(VectorOperators.COSH)

    @HotPath
    def exp: Array[Double] =
      val out = vec.clone()
      out.unaryOp(VectorOperators.EXP)
      out
    end exp

    @HotPath
    def `exp!`: Unit =
      unaryOp(VectorOperators.EXP)

    @HotPath
    def expm1: Array[Double] =
      val out = vec.clone()
      out.unaryOp(VectorOperators.EXPM1)
      out
    end expm1

    @HotPath
    def `expm1!`: Unit =
      unaryOp(VectorOperators.EXPM1)

    @HotPath
    def log: Array[Double] =
      val out = vec.clone()
      out.unaryOp(VectorOperators.LOG)
      out
    end log

    @HotPath
    def `log!`: Unit =
      unaryOp(VectorOperators.LOG)

    @HotPath
    def log10: Array[Double] =
      val out = vec.clone()
      out.unaryOp(VectorOperators.LOG10)
      out
    end log10

    @HotPath
    def `log10!`: Unit =
      unaryOp(VectorOperators.LOG10)

    @HotPath
    def log1p: Array[Double] =
      val out = vec.clone()
      out.unaryOp(VectorOperators.LOG1P)
      out
    end log1p

    @HotPath
    def `log1p!`: Unit =
      unaryOp(VectorOperators.LOG1P)

    @HotPath
    def sqrt: Array[Double] =
      val out = vec.clone()
      out.unaryOp(VectorOperators.SQRT)
      out
    end sqrt

    @HotPath
    def `sqrt!`: Unit =
      unaryOp(VectorOperators.SQRT)

    @HotPath
    def sin: Array[Double] =
      val out = vec.clone()
      out.unaryOp(VectorOperators.SIN)
      out
    end sin

    @HotPath
    def `sin!`: Unit =
      unaryOp(VectorOperators.SIN)

    @HotPath
    def sinh: Array[Double] =
      val out = vec.clone()
      out.unaryOp(VectorOperators.SINH)
      out
    end sinh

    @HotPath
    def `sinh!`: Unit =
      unaryOp(VectorOperators.SINH)

    @HotPath
    def `tan!`: Unit =
      unaryOp(VectorOperators.TAN)

    @HotPath
    def tan: Array[Double] =
      val out = vec.clone()
      out.unaryOp(VectorOperators.TAN)
      out
    end tan

    @HotPath
    def `tanh!`: Unit =
      unaryOp(VectorOperators.TANH)

    @HotPath
    def tanh: Array[Double] =
      val out = vec.clone()
      out.unaryOp(VectorOperators.TANH)
      out
    end tanh

    @HotPath
    def `**!`(power: Double): Unit =
      var i = 0
      val bp = DoubleVector.broadcast(spd, power)
      val bound = spd.loopBound(vec.length)
      while i < bound do
        DoubleVector
          .fromArray(spd, vec, i)
          .lanewise(VectorOperators.POW, bp)
          .intoArray(vec, i)
        i += spdl
      end while

      while i < vec.length do
        vec(i) = Math.pow(vec(i), power)
        i += 1
      end while
    end `**!`

    inline def **(power: Double): Array[Double] =
      val out = vec.clone()
      out.`**!`(power)
      out
    end **

    @HotPath
    def increments: Array[Double] =
      val out = new Array[Double](vec.length)
      val bound = spd.loopBound(vec.length - 2)
      var i = 1
      while i < bound do
        DoubleVector
          .fromArray(spd, vec, i)
          .sub(DoubleVector.fromArray(spd, vec, i - 1))
          .intoArray(out, i)
        i += spdl
      end while

      while i < vec.length do
        out(i) = vec(i) - vec(i - 1)
        i = i + 1
      end while
      out(0) = vec(0)
      out
    end increments

    inline def pearsonCorrelationCoefficient(thatVector: Array[Double]): Double =
      dimCheck(vec, thatVector)
      val n = vec.length
      var i = 0

      var sum_x = 0.0
      var sum_y = 0.0
      var sum_xy = 0.0
      var sum_x2 = 0.0
      var sum_y2 = 0.0

      while i < n do
        val x = vec(i)
        val y = thatVector(i)
        // Use fma to optimize multiply-add operations for better performance
        sum_x = sum_x + x
        sum_y = sum_y + y
        sum_xy = Math.fma(x, y, sum_xy) // x * y + sum_xy
        sum_x2 = Math.fma(x, x, sum_x2) // x * x + sum_x2
        sum_y2 = Math.fma(y, y, sum_y2) // y * y + sum_y2
        i = i + 1
      end while
      (n * sum_xy - (sum_x * sum_y)) / Math.sqrt(
        (sum_x2 * n - sum_x * sum_x) * (sum_y2 * n - sum_y * sum_y)
      )
    end pearsonCorrelationCoefficient

    inline def spearmansRankCorrelation(thatVector: Array[Double]): Double =
      dimCheck(vec, thatVector)
      val theseRanks = vec.elementRanks
      val thoseRanks = thatVector.elementRanks
      theseRanks.pearsonCorrelationCoefficient(thoseRanks)
    end spearmansRankCorrelation

    // An alias - pearson is the most commonly requested type of correlation
    inline def corr(thatVector: Array[Double]): Double =
      pearsonCorrelationCoefficient(thatVector)

    inline def elementRanks: Array[Double] =
      val indexed: Array[(Double, Int)] = vec.zipWithIndex
      indexed.sortInPlace()(using Ordering.by(_._1))

      val ranks: Array[Double] = new Array[Double](vec.length)
      ranks(indexed.last._2) = vec.length
      var currentValue: Double = indexed(0)._1
      var r0: Int = 0
      var rank: Int = 1
      while rank < vec.length do
        val temp: Double = indexed(rank)._1
        val end: Int =
          if temp != currentValue then rank
          else if rank == vec.length - 1 then rank + 1
          else -1
        if end > -1 then
          val avg: Double = (1.0 + (end + r0)) / 2.0
          var i: Int = r0;
          while i < end do
            ranks(indexed(i)._2) = avg
            i += 1
          end while
          r0 = rank
          currentValue = temp
        end if
        rank += 1
      end while
      ranks
    end elementRanks

    def outer(other: Array[Double])(using ClassTag[Double]): Matrix[Double] =
      val n = vec.length
      val m = other.length
      val out = new Array[Double](n * m)

      var j = 0
      while j < m do
        var i = 0
        val tmp = DoubleVector.broadcast(spd, other(j))
        val bound = spd.loopBound(n)
        while i < bound do
          DoubleVector.fromArray(spd, vec, i).mul(tmp).intoArray(out, j * n + i)
          i = i + spdl
        end while

        while i < n do
          out(j * n + i) = vec(i) * other(j)
          i = i + 1
        end while
        j = j + 1
      end while
      Matrix(out, (n, m))
    end outer

    inline def variance: Double = variance(VarianceMode.Population)

    /** `@Thin` again, and the history is the point.
      *
      * This was annotated `@Thin`, C3 measured it at 37 bytes against a 35-byte `MaxInlineSize`, and the annotation
      * came off with a note that ~30 of those bytes were the cost of destructuring a named tuple: reading one field out
      * of `(mean: Double, variance: Double)` meant an unbox out of a `Tuple2`. The same note guessed that the
      * allocation was the more interesting half and left it to check D1.
      *
      * D1 answered both. `variance(mode)` allocated **zero** bytes/op — the pair is dead here, so escape analysis
      * scalarizes it — while a caller that actually reads the pair allocated 59.33 bytes/op. So the tuple was never
      * costing this method anything at runtime; it was costing it 37 bytes of bytecode, and costing every real caller
      * of `meanAndVariance` an object per call.
      *
      * [[vecxt.MeanAndVariance]] replaced the tuple for the second reason and fixes this one as a side effect: a field
      * read off a `final class` with primitive fields is an `invokevirtual`, not an unbox. `@AllocFree` records the
      * measured zero so a future restructuring that lets the result escape is caught rather than discovered.
      *
      * ==What the `@AllocFree` here is load-bearing on==
      *
      * The zero depends on C2 inlining `meanAndVarianceTwoPass` into this method, because that is where the
      * `MeanAndVariance` is constructed and escape analysis only runs after C2's own inlining. A `LogCompilation` run
      * puts that method at 1688 bytes of machine code — 68% of `InlineSmallCode` (2500), the budget above which C2
      * declines to inline an already-compiled callee. Nothing measures that number, and nothing will: check D2 would
      * have, and was deliberately not built (see `jitAudit/package.mill`).
      *
      * So if `meanAndVarianceTwoPass` grows past the limit, the pair starts escaping, and the symptom is D1 failing on
      * *this* method with a message about allocation rather than about inlining. That indirection is the accepted cost
      * of not building D2. If it happens, look at the callee's compiled size before looking at anything here.
      */
    @Thin
    @AllocFree
    def variance(mode: VarianceMode): Double =
      meanAndVariance(mode).variance
    end variance

    inline def std: Double = std(VarianceMode.Population)

    inline def std(mode: VarianceMode): Double =
      Math.sqrt(vec.variance(mode))

    inline def stdDev: Double = stdDev(VarianceMode.Population)

    inline def stdDev(mode: VarianceMode): Double = std(mode)

    @Thin
    def meanAndVariance: MeanAndVariance =
      meanAndVariance(VarianceMode.Population)

    @Thin
    def meanAndVariance(mode: VarianceMode): MeanAndVariance =
      meanAndVarianceTwoPass(mode)
    end meanAndVariance

    /** True SIMD-optimized Welford's algorithm for computing mean and variance.
      *
      * Each SIMD lane maintains independent Welford accumulators (n, mean, M2). Lanes process strided elements: lane 0
      * gets [0,4,8,...], lane 1 gets [1,5,9,...], etc. At the end, all lanes are merged using the parallel Welford
      * merge formula:
      *
      * δ = meanB - meanA n = nA + nB mean = meanA + δ * nB / n M2 = M2A + M2B + δ² * nA * nB / n
      *
      * This algorimth is crushed by the simple two pass SIMD version.
      *
      * 231] Benchmark (len) Mode Cnt Score Error Units 231] VarianceBenchmark.var_simd_twopass 1000 thrpt 3 1087302.435
      * ± 16013.286 ops/s 231] VarianceBenchmark.var_simd_twopass 100000 thrpt 3 9578.869 ± 334.606 ops/s 231]
      * VarianceBenchmark.var_simd_welford 1000 thrpt 3 436244.559 ± 6158.585 ops/s 231]
      * VarianceBenchmark.var_simd_welford 100000 thrpt 3 4187.715 ± 203.266 ops/s
      */
    private def meanAndVarianceWelfordSIMD(mode: VarianceMode): MeanAndVariance =
      if vec.length == 0 then MeanAndVariance(0.0, 0.0)
      else
        // Per-lane accumulators
        var laneMeans = DoubleVector.zero(spd)
        var delta = DoubleVector.zero(spd)
        var delta2 = DoubleVector.zero(spd)
        var laneM2 = DoubleVector.zero(spd)

        var i = 0
        var j: Double = 1
        // ALl lanes will have processed J elements at the end of this loop
        val bound = spd.loopBound(vec.length)
        while i < bound do
          j = j + 1
          val values = DoubleVector.fromArray(spd, vec, i)
          delta = values.sub(laneMeans) // Use current mean
          laneMeans = laneMeans.add(delta.div(DoubleVector.broadcast(spd, j)))
          delta2 = values.sub(laneMeans) // Use updated mean
          laneM2 = laneM2.add(delta.mul(delta2))
          i += spdl
        end while

        // val laneSumA = laneSum.toArray()
        val laneMean = laneMeans.toArray()
        val laneM2A = laneM2.toArray()
        // Merge all lanes
        var globalN = j
        var globalMean = laneMean(0)
        var globalM2 = laneM2A(0)

        var lane = 1
        while lane < spdl do
          val delta = laneMean(lane) - globalMean
          val newN = globalN + j
          globalMean = globalMean + delta * j / newN
          globalM2 = globalM2 + laneM2A(lane) + delta * delta * globalN * j / newN
          globalN = newN

          lane += 1
        end while

        // Process tail elements
        while i < vec.length do
          val n = globalN + 1
          val delta = vec(i) - globalMean
          globalMean += delta / n
          val delta2 = vec(i) - globalMean
          globalM2 += delta * delta2
          globalN = n
          i += 1
        end while

        val denom = mode match
          case VarianceMode.Population => vec.length.toDouble
          case VarianceMode.Sample     => (vec.length - 1).toDouble

        MeanAndVariance(globalMean, globalM2 / denom)
      end if
    end meanAndVarianceWelfordSIMD

    /** Two-pass variance calculation (legacy, for comparison). First pass computes mean, second pass computes variance.
      */
    def meanAndVarianceTwoPass(mode: VarianceMode): MeanAndVariance =
      val μ = vec.mean
      val l = spd.length()
      var tmp = DoubleVector.zero(spd)
      val μVec = DoubleVector.broadcast(spd, μ)

      var i = 0
      val bound = spd.loopBound(vec.length)
      while i < bound do
        val v = DoubleVector.fromArray(spd, vec, i)
        val diff = v.sub(μVec)
        tmp = diff.fma(diff, tmp)
        i += spdl
      end while

      var sumSqDiff = tmp.reduceLanes(VectorOperators.ADD)

      while i < vec.length do
        val diff = vec(i) - μ
        sumSqDiff = Math.fma(diff, diff, sumSqDiff)
        i += 1
      end while

      val denom = mode match
        case VarianceMode.Population => vec.length.toDouble
        case VarianceMode.Sample     => (vec.length - 1).toDouble

      MeanAndVariance(μ, sumSqDiff / denom)
    end meanAndVarianceTwoPass

    inline def mean: Double = vec.sumSIMD / vec.length

    inline def sum: Double = sumSIMD

    @HotPath
    @AllocFree
    def sumSIMD: Double =
      var i: Int = 0
      var acc = DoubleVector.zero(spd)
      val bound = spd.loopBound(vec.length)

      while i < bound do
        acc = acc.add(DoubleVector.fromArray(spd, vec, i))
        i += spdl
      end while

      var temp = acc.reduceLanes(VectorOperators.ADD)
      // var temp = 0.0
      while i < vec.length do
        temp += vec(i)
        i += 1
      end while
      temp
    end sumSIMD

    /** Segment overload of [[sumSIMD]]: sums `len` elements starting at `from`. Whole-array `sumSIMD` above is now a
      * thin forwarder in spirit — kept separate (rather than rewritten to call this) since it is
      * `@HotPath`/`@AllocFree` audited and must stay a self-contained kernel; this overload follows the identical
      * vector-body/scalar-tail shape so both stay in lockstep.
      */
    @HotPath
    @AllocFree
    def sumSIMD(from: Int, len: Int): Double =
      var i: Int = from
      var acc = DoubleVector.zero(spd)
      val end = from + len
      val bound = from + spd.loopBound(len)

      while i < bound do
        acc = acc.add(DoubleVector.fromArray(spd, vec, i))
        i += spdl
      end while

      var temp = acc.reduceLanes(VectorOperators.ADD)
      while i < end do
        temp += vec(i)
        i += 1
      end while
      temp
    end sumSIMD

    inline def product: Double = productSIMD

    @HotPath
    @AllocFree
    def productSIMD: Double =
      var i: Int = 0
      var acc = DoubleVector.broadcast(spd, 1.0)
      val bound = spd.loopBound(vec.length)

      while i < bound do
        acc = acc.mul(DoubleVector.fromArray(spd, vec, i))
        i += spdl
      end while
      var temp = acc.reduceLanes(VectorOperators.MUL)
      // var temp = 0.0
      while i < vec.length do
        temp *= vec(i)
        i += 1
      end while
      temp
    end productSIMD

    /** Given an array `nums` of n integers where n > 1, returns an array `output` such that `output[i]` is equal to the
      * product of all the elements of `nums` except `nums[i]`.
      *
      * This method does not use division and runs in O(n) time complexity.
      *
      * @param nums
      *   An array of integers.
      * @return
      *   An array where each element is the product of all the elements of `nums` except the element at the same index.
      */
    def productExceptSelf: Array[Double] =
      val n = vec.length
      val leftProducts = new Array[Double](n)
      val rightProducts = new Array[Double](n)

      leftProducts(0) = 1.0
      rightProducts(n - 1) = 1.0

      var i = 1
      var j = n - 2
      while i < n do
        leftProducts(i) = leftProducts(i - 1) * vec(i - 1)
        rightProducts(j) = rightProducts(j + 1) * vec(j + 1)
        i += 1
        j -= 1
      end while

      i = 0
      val bound = spd.loopBound(vec.length)
      while i < bound do
        DoubleVector
          .fromArray(spd, leftProducts, i)
          .mul(DoubleVector.fromArray(spd, rightProducts, i))
          .intoArray(leftProducts, i)
        i += spdl
      end while

      while i < vec.length do
        leftProducts(i) = leftProducts(i) * rightProducts(i)
        i = i + 1
      end while

      leftProducts
    end productExceptSelf

    private inline def reduceOp(inline op: VectorOperators.Binary, inline initial: Double): Double =
      var i = 0
      var vecAcc = DoubleVector.broadcast(spd, initial)

      val bound = spd.loopBound(vec.length)
      while i < bound do
        vecAcc = vecAcc.lanewise(op, DoubleVector.fromArray(spd, vec, i))
        i += spdl
      end while

      var result = vecAcc.reduceLanes(op.asInstanceOf[VectorOperators.Associative])

      while i < vec.length do
        result = inline op match
          case VectorOperators.MAX => Math.max(result, vec(i))
          case VectorOperators.MIN => Math.min(result, vec(i))
          case _                   => scala.compiletime.error("reduceOp supports MAX and MIN only")
        i += 1
      end while

      result
    end reduceOp

    inline def max: Double = maxSIMD

    inline def min: Double = minSIMD

    inline def maxSIMD: Double =
      reduceOp(VectorOperators.MAX, Double.MinValue)

    inline def minSIMD: Double =
      reduceOp(VectorOperators.MIN, Double.MaxValue)

    private inline def `clampOp!`(inline op: VectorOperators.Comparison, inline initial: Double): Unit =
      var i = 0
      var vecAcc = DoubleVector.broadcast(spd, initial)

      while i < spd.loopBound(vec.length) do
        val values = DoubleVector.fromArray(spd, vec, i)
        val mask = values.compare(op, initial)
        vecAcc.intoArray(vec, i, mask)
        values.intoArray(vec, i, mask.not())
        i += spdl
      end while

      while i < vec.length do
        vec(i) = inline op match
          case VectorOperators.LT => Math.max(initial, vec(i))
          case VectorOperators.GT => Math.min(initial, vec(i))
          case _                  => scala.compiletime.error("reduceOp supports MAX and MIN only")
        i += 1
      end while

    end `clampOp!`

    /** Clamps the values in the array to a maximum value.
      *
      * @param floor
      *   The maximum value to clamp to.
      * @return
      *   A new array with values clamped to the specified maximum.
      */
    inline def clampMax(ceil: Double): Array[Double] =
      val out = vec.clone
      out.`clampOp!`(VectorOperators.GT, ceil)
      out
    end clampMax

    inline def maxClamp(ceil: Double): Array[Double] =
      val out = vec.clone
      out.`clampOp!`(VectorOperators.GT, ceil)
      out
    end maxClamp

    inline def `maxClamp!`(ceil: Double): Unit =
      vec.`clampOp!`(VectorOperators.GT, ceil)

    /** Clamps the values in the array to a minimum value.
      *
      * @param ceil
      *   The minimum value to clamp to.
      * @return
      *   A new array with values clamped to the specified minimum.
      */
    inline def clampMin(floor: Double): Array[Double] =
      val out = vec.clone
      out.`clampOp!`(VectorOperators.LT, floor)
      out
    end clampMin

    inline def minClamp(floor: Double): Array[Double] =
      val out = vec.clone
      out.`clampOp!`(VectorOperators.LT, floor)
      out
    end minClamp

    inline def `minClamp!`(floor: Double): Unit =
      vec.`clampOp!`(VectorOperators.LT, floor)

    /** Clamps the values in the array to a specified range.
      * @param ceil
      *   The maximum value to clamp to.
      * @param floor
      *   The minimum value to clamp to.
      * @return
      *   A new array with values clamped to the specified range.
      */
    @HotPath
    @AllocFree
    def `clamp!`(floor: Double, ceil: Double): Unit =
      var i = 0
      var vecCeil = DoubleVector.broadcast(spd, ceil)
      var vecFloor = DoubleVector.broadcast(spd, floor)
      val bound = spd.loopBound(vec.length)

      while i < bound do
        val values = DoubleVector.fromArray(spd, vec, i)
        val maskGt = values.compare(VectorOperators.GT, vecCeil)
        val maskLt = values.compare(VectorOperators.LT, vecFloor)
        vecCeil.intoArray(vec, i, maskGt)
        vecFloor.intoArray(vec, i, maskLt)
        values.intoArray(vec, i, maskGt.or(maskLt).not())
        i += spdl
      end while

      while i < vec.length do
        vec(i) = if vec(i) > ceil then ceil else if vec(i) < floor then floor else vec(i)
        i += 1
      end while

    end `clamp!`

    /** Clamps the values in the array to a specified range.
      * @param ceil
      *   The maximum value to clamp to.
      * @param floor
      *   The minimum value to clamp to.
      * @return
      *   A new array with values clamped to the specified range.
      */
    inline def clamp(floor: Double, ceil: Double): Array[Double] =
      val out = vec.clone
      out.`clamp!`(floor, ceil)
      out
    end clamp

    /** The formula for the logarithm of the sum of exponentials is:
      *
      * logSumExp(x) = log(sum(exp(x_i))) for i = 1 to n
      *
      * This is computed in a numerically stable way by subtracting the maximum value in the array before taking the
      * exponentials:
      *
      * logSumExp(x) = max(x) + log(sum(exp(x_i - max(x)))) for i = 1 to n
      */
    def logSumExp: Double =
      val maxVal = vec.max
      var sumExpVec = DoubleVector.zero(spd)
      var i = 0
      val bound = spd.loopBound(vec.length)

      while i < bound do
        val vecSegment = DoubleVector.fromArray(spd, vec, i)
        val expSegment = vecSegment.sub(maxVal).lanewise(VectorOperators.EXP)
        sumExpVec = sumExpVec.add(expSegment)
        i += spdl
      end while

      var sumExp = sumExpVec.reduceLanes(VectorOperators.ADD)

      while i < vec.length do
        sumExp += Math.exp(vec(i) - maxVal)
        i += 1
      end while

      maxVal + Math.log(sumExp)
    end logSumExp

    /** The one kernel in this file whose `@AllocFree` does not depend on intrinsification.
      *
      * A prefix sum carries a dependency — `vec(i)` needs the value just written to `vec(i - 1)` — so it cannot be
      * vectorised and there is no Vector API here at all. Nothing to intrinsify, therefore nothing whose failure to
      * intrinsify could leave a `DoubleVector` on the heap. Every other `@AllocFree` in this file is contingent on C2
      * applying `VectorSupport` intrinsics; this one is true because the body allocates nothing, full stop.
      */
    @HotPath
    @AllocFree
    def `cumsum!`: Unit =
      var i = 1
      while i < vec.length do
        vec(i) = vec(i - 1) + vec(i)
        i = i + 1
      end while
    end `cumsum!`

    @Thin
    def cumsum: Array[Double] =
      val out = vec.clone()
      out.`cumsum!`
      out
    end cumsum

    /** `@Thin`, not `@HotPath`, and the distinction is the annotation's own wording.
      *
      * `@HotPath` describes "code that runs once per element". The per-element loop here is inside netlib's `ddot`, not
      * in this method — what this method does is name the operation and dispatch to it, which is exactly `@Thin`'s
      * definition. C3's no-backward-branch assertion is satisfied for the same reason, and would fail if someone ever
      * open-coded the loop back into it, which is the right outcome.
      *
      * Not `@AllocFree`. `blas` is `JavaBLAS.getInstance`, so this is a call into a third-party pure-Java
      * implementation. Whether it allocates is not visible from here and has never been measured, and the annotation
      * exists to record measurements rather than expectations — the two that turned out to be false (`**!`, and
      * `intarrays.dot`) were both applied by inspection. Same for `norm` and the array-argument `-=` below.
      */
    @Thin
    def dot(v1: Array[Double]): Double =
      dimCheck(vec, v1)
      blas.ddot(vec.length, vec, 1, v1, 1)
    end dot

    @Thin
    def norm: Double = blas.dnrm2(vec.length, vec, 1)

    /** Segment overload of [[norm]]: computes the Euclidean norm of `len` elements starting at `from`, without
      * materialising the segment into its own array. Unlocks SIMD (via BLAS) for a strided view whose unit-stride axis
      * exposes contiguous column/row segments — see `Layout.contiguousSegments`.
      */
    inline def norm(from: Int, len: Int): Double = blas.dnrm2(len, vec, from, 1)

    @Thin
    def -(vec2: Array[Double]): Array[Double] =
      dimCheck(vec, vec2)
      val out = vec.clone
      out -= vec2
      out
    end -

    /** Note the asymmetry with the scalar overload below, which is `@HotPath @AllocFree`: that one is a hand-written
      * Vector API loop, this one delegates the whole operation to `daxpy`. Same name, different implementations, so
      * different annotations. Not an inconsistency to tidy up.
      */
    @Thin
    def -=(vec2: Array[Double]): Unit =
      dimCheck(vec, vec2)
      blas.daxpy(vec.length, -1.0, vec2, 1, vec, 1)
    end -=

    inline def add(d: Array[Double]) = vec + d

    def +(d: Double): Array[Double] =
      val out = new Array[Double](vec.length)
      val inc = DoubleVector.broadcast(spd, d)
      var i = 0
      val bound = spd.loopBound(vec.length)
      while i < bound do
        DoubleVector
          .fromArray(spd, vec, i)
          .add(inc)
          .intoArray(out, i)
        i += spdl
      end while

      while i < vec.length do
        out(i) = vec(i) + d
        i = i + 1
      end while
      out
    end +

    /** Segment overload of [[+]]: reads `len` elements starting at `from`, adds `d`, and writes the result into `dest`
      * starting at `destFrom`. Lets a caller (e.g. a strided `Matrix` view whose unit-stride axis exposes contiguous
      * column/row segments) route each segment through this SIMD kernel directly into its final position in a freshly
      * allocated destination array, without a separate segment-materialising copy.
      */
    @HotPath
    @AllocFree
    def +(d: Double, from: Int, len: Int, dest: Array[Double], destFrom: Int): Unit =
      val shift = destFrom - from
      val inc = DoubleVector.broadcast(spd, d)
      var i = from
      val end = from + len
      val bound = from + spd.loopBound(len)
      while i < bound do
        DoubleVector
          .fromArray(spd, vec, i)
          .add(inc)
          .intoArray(dest, i + shift)
        i += spdl
      end while

      while i < end do
        dest(i + shift) = vec(i) + d
        i = i + 1
      end while
    end +

    @HotPath
    @AllocFree
    def +=(d: Double): Unit =
      val inc = DoubleVector.broadcast(spd, d)
      var i = 0
      val bound = spd.loopBound(vec.length)
      while i < bound do
        DoubleVector
          .fromArray(spd, vec, i)
          .add(inc)
          .intoArray(vec, i)
        i += spdl
      end while

      while i < vec.length do
        vec(i) = vec(i) + d
        i = i + 1
      end while
    end +=

    def -(d: Double): Array[Double] =
      val out = new Array[Double](vec.length)
      val inc = DoubleVector.broadcast(spd, d)
      var i = 0
      val bound = spd.loopBound(vec.length)
      while i < bound do
        DoubleVector
          .fromArray(spd, vec, i)
          .sub(inc)
          .intoArray(out, i)
        i += spdl
      end while

      while i < vec.length do
        out(i) = vec(i) - d
        i = i + 1
      end while
      out
    end -

    /** Segment overload of [[-]]: mirrors [[+]]'s `(d, from, len, dest, destFrom)` overload. */
    @HotPath
    @AllocFree
    def -(d: Double, from: Int, len: Int, dest: Array[Double], destFrom: Int): Unit =
      val shift = destFrom - from
      val inc = DoubleVector.broadcast(spd, d)
      var i = from
      val end = from + len
      val bound = from + spd.loopBound(len)
      while i < bound do
        DoubleVector
          .fromArray(spd, vec, i)
          .sub(inc)
          .intoArray(dest, i + shift)
        i += spdl
      end while

      while i < end do
        dest(i + shift) = vec(i) - d
        i = i + 1
      end while
    end -

    @HotPath
    @AllocFree
    def `fma!`(multiply: Double, add: Double): Unit =
      var i = 0
      val bound = spd.loopBound(vec.length)
      while i < bound do
        DoubleVector
          .fromArray(spd, vec, i)
          .fma(multiply, add)
          .intoArray(vec, i)
        i += spdl
      end while

      while i < vec.length do
        vec(i) = Math.fma(vec(i), multiply, add)
        i = i + 1
      end while
    end `fma!`

    inline def fma(multiply: Double, add: Double): Array[Double] =
      val out = vec.clone()
      out `fma!` (multiply, add)
      out
    end fma

    @HotPath
    @AllocFree
    def -=(d: Double): Unit =
      val inc = DoubleVector.broadcast(spd, d)
      var i = 0
      val bound = spd.loopBound(vec.length)
      while i < bound do
        DoubleVector
          .fromArray(spd, vec, i)
          .sub(inc)
          .intoArray(vec, i)
        i += spdl
      end while

      while i < vec.length - 1 do
        vec(i) = vec(i) - d
        i = i + 1
      end while
    end -=

    inline def +(vec2: Array[Double]): Array[Double] =
      dimCheck(vec, vec2)
      val out = vec.clone
      out += vec2
      out
    end +

    inline def +=(vec2: Array[Double]): Unit =
      dimCheck(vec, vec2)
      blas.daxpy(vec.length, 1.0, vec2, 1, vec, 1)
    end +=

    inline def +:+(d: Double): Array[Double] =
      val out = vec.clone
      out +:+= d
      out
    end +:+

    inline def +:+=(d: Double): Unit =
      var i: Int = 0
      while i < vec.length do
        vec(i) += d
        i += 1
      end while
    end +:+=

    inline def multInPlace(d: Double) = vec *= d

    /** Segment overload of [[multInPlace]]: scales `len` elements starting at `from` in place. */
    inline def multInPlace(d: Double, from: Int, len: Int): Unit = vec.*=(d, from, len)

    def *(d: Array[Double]): Array[Double] =
      dimCheck(vec, d)
      val out = new Array[Double](vec.length)
      var i = 0
      val bound = spd.loopBound(vec.length)
      while i < bound do
        DoubleVector
          .fromArray(spd, vec, i)
          .mul(DoubleVector.fromArray(spd, d, i))
          .intoArray(out, i)
        i += spdl
      end while

      while i < vec.length do
        out(i) = vec(i) * d(i)
        i = i + 1
      end while
      out
    end *

    inline def *:*(d: Array[Double]): Array[Double] = vec.*(d)

    inline def *:*=(d: Array[Double]): Unit = vec.*=(d)

    @HotPath
    @AllocFree
    def *=(d: Array[Double]): Unit =
      dimCheck(vec, d)
      var i = 0
      val bound = spd.loopBound(vec.length)
      while i < bound do
        DoubleVector
          .fromArray(spd, vec, i)
          .mul(DoubleVector.fromArray(spd, d, i))
          .intoArray(vec, i)
        i += spdl
      end while

      while i < vec.length do
        vec(i) = vec(i) * d(i)
        i = i + 1
      end while
    end *=

    def /(d: Array[Double]): Array[Double] =
      dimCheck(vec, d)
      val out = new Array[Double](vec.length)
      var i = 0
      val bound = spd.loopBound(vec.length)
      while i < bound do
        DoubleVector
          .fromArray(spd, vec, i)
          .div(DoubleVector.fromArray(spd, d, i))
          .intoArray(out, i)
        i += spdl
      end while

      while i < vec.length do
        out(i) = vec(i) / d(i)
        i = i + 1
      end while
      out
    end /

    inline def /=(d: Double): Array[Double] =
      blas.dscal(vec.length, 1.0 / d, vec, 1)
      vec
    end /=

    /** Segment overload of [[/=]]: divides `len` elements starting at `from` in place by `d`. */
    inline def /=(d: Double, from: Int, len: Int): Unit =
      blas.dscal(len, 1.0 / d, vec, from, 1)
    end /=

    inline def /(d: Double): Array[Double] =
      val out = vec.clone
      out /= d
      out
    end /

    /** Segment overload of [[/]]: reads `len` elements starting at `from`, divides by `d`, and writes the result into
      * `dest` starting at `destFrom`. Mirrors the `(d, from, len, dest, destFrom)` shape of [[+]] and [[-]] — scales by
      * the reciprocal via `DoubleVector` rather than `blas.dscal`, since `dscal` only scales in place and cannot target
      * a separate destination array.
      */
    @HotPath
    @AllocFree
    def /(d: Double, from: Int, len: Int, dest: Array[Double], destFrom: Int): Unit =
      val shift = destFrom - from
      val recipScalar = 1.0 / d
      val recip = DoubleVector.broadcast(spd, recipScalar)
      var i = from
      val end = from + len
      val bound = from + spd.loopBound(len)
      while i < bound do
        DoubleVector
          .fromArray(spd, vec, i)
          .mul(recip)
          .intoArray(dest, i + shift)
        i += spdl
      end while

      while i < end do
        dest(i + shift) = vec(i) * recipScalar
        i = i + 1
      end while
    end /

    inline def *=(d: Double): Unit =
      blas.dscal(vec.length, d, vec, 1)
    end *=

    /** Segment overload of [[*=]]: scales `len` elements starting at `from` in place by `d`. This is what unlocks SIMD
      * (via BLAS `dscal`) for a strided matrix view whose unit-stride axis exposes contiguous segments — each segment
      * is `(offset + axisIdx * strideOfOtherAxis, contiguousLen)`.
      */
    inline def *=(d: Double, from: Int, len: Int): Unit =
      blas.dscal(len, d, vec, from, 1)
    end *=

    inline def *(d: Double): Array[Double] =
      val out = vec.clone
      out *= d
      out
    end *

    inline def =:=(num: Double): Array[Boolean] =
      logicalIdx(VectorOperators.EQ, num)

    inline def !:=(num: Double): Array[Boolean] =
      logicalIdx(VectorOperators.NE, num)

    inline def <(num: Double): Array[Boolean] =
      logicalIdx(VectorOperators.LT, num)

    inline def <=(num: Double): Array[Boolean] =
      logicalIdx(VectorOperators.LE, num)

    inline def >(num: Double): Array[Boolean] =
      logicalIdx(VectorOperators.GT, num)

    inline def >=(num: Double): Array[Boolean] =
      logicalIdx(VectorOperators.GE, num)

    private inline def logicalIdx(
        inline op: VectorOperators.Comparison,
        num: Double
    ): Array[Boolean] =
      val idx = new Array[Boolean](vec.length)
      var i = 0

      while i < spd.loopBound(vec.length) do
        DoubleVector.fromArray(spd, vec, i).compare(op, num).intoArray(idx, i)
        i += spdl
      end while

      inline op match
        case VectorOperators.EQ =>
          while i < vec.length do
            idx(i) = vec(i) == num
            i += 1
          end while
        case VectorOperators.NE =>
          while i < vec.length do
            idx(i) = vec(i) != num
            i += 1
          end while
        case VectorOperators.LT =>
          while i < vec.length do
            idx(i) = vec(i) < num
            i += 1
          end while

        case VectorOperators.LE =>
          while i < vec.length do
            idx(i) = vec(i) <= num
            i += 1
          end while

        case VectorOperators.GT =>
          while i < vec.length do
            idx(i) = vec(i) > num
            i += 1
          end while

        case VectorOperators.GE =>
          while i < vec.length do
            idx(i) = vec(i) >= num
            i += 1
          end while
        case _ => scala.compiletime.error("this method supports EQ, NE, LT, LE, GT, GE only")
      end match

      idx
    end logicalIdx
    def covariance(thatVector: Array[Double]): Double =
      val μThis = vec.mean
      val μThat = thatVector.mean
      val n = vec.length
      var i = 0
      var acc = DoubleVector.zero(spd)

      // SIMD loop
      while i < spd.loopBound(n) do
        val v1 = DoubleVector.fromArray(spd, vec, i).sub(μThis)
        val v2 = DoubleVector.fromArray(spd, thatVector, i).sub(μThat)
        acc = v1.fma(v2, acc)
        i += spdl
      end while

      // Remainder
      var cv = acc.reduceLanes(VectorOperators.ADD)
      while i < n do
        cv = Math.fma(vec(i) - μThis, thatVector(i) - μThat, cv)
        i += 1
      end while

      cv / (n - 1)
    end covariance

    /** Returns the index of the maximum element in the array using SIMD operations for performance.
      *
      * This method processes the array in blocks to maximize instruction-level parallelism (ILP) and minimize
      * synchronization overhead.
      *
      * https://en.algorithmica.org/hpc/algorithms/argmin/
      *
      * For small arrays, perhaps 2x slower. For larger arrays (e.g. 1000 elements, at least 2x faster)
      *
      * @return
      *   The index of the maximum element, or -1 if the array is empty.
      */
    def argmax: Int =
      val n = vec.length
      if n == 0 then return -1
      end if
      if n == 1 then return 0
      end if

      // Algorithmica.org approach: block-based with infrequent updates
      val blockSize = spd.length() * 4 // Process many elements per block for optimal ILP
      var globalMax = Double.MinValue
      var blockWithMax = 0

      var i = 0
      val loopBound = n - (n % blockSize)

      // Broadcast current max for SIMD comparison
      var maxVec = DoubleVector.broadcast(spd, globalMax)

      // Main SIMD loop processing 32 elements per iteration
      while i < loopBound do
        // Load 4 SIMD vectors (32 elements total)
        val v1 = DoubleVector.fromArray(spd, vec, i)
        val v2 = DoubleVector.fromArray(spd, vec, i + spdl)
        val v3 = DoubleVector.fromArray(spd, vec, i + 2 * spdl)
        val v4 = DoubleVector.fromArray(spd, vec, i + 3 * spdl)

        // Find block maximum using tree reduction
        val max12 = v1.max(v2)
        val max34 = v3.max(v4)
        val blockMax = max12.max(max34)

        // Check if any element in this block is greater than global max
        val mask = blockMax.compare(VectorOperators.GT, maxVec)

        if mask.anyTrue() then // Check if any element is greater - rarely executed
          // Update global maximum within this block
          var j = i
          while j < i + blockSize do
            if vec(j) > globalMax then globalMax = vec(j)
            end if
            j += 1
          end while
          blockWithMax = i
          maxVec = DoubleVector.broadcast(spd, globalMax)
        end if

        i += blockSize
      end while

      // Handle remaining elements
      while i < n do
        if vec(i) > globalMax then
          globalMax = vec(i)
          blockWithMax = (i / blockSize) * blockSize // Start of block containing this element
        end if
        i += 1
      end while

      // Find exact index within the block containing the maximum
      var exactIdx = blockWithMax
      val searchEnd = Math.min(blockWithMax + blockSize, n)
      var j = blockWithMax
      while j < searchEnd do
        if vec(j) == globalMax then
          exactIdx = j
          return exactIdx // Return first occurrence
        end if
        j += 1
      end while

      exactIdx
    end argmax

    /** Returns the index of the minimum element in the array using SIMD operations for performance.
      *
      * This method processes the array in blocks to maximize instruction-level parallelism (ILP) and minimize
      * synchronization overhead.
      *
      * For small arrays, perhaps 2x slower. For larger arrays (e.g. 1000 elements, at least 2x faster)
      *
      * @return
      *   The index of the minimum element, or -1 if the array is empty.
      */
    def argmin: Int =
      val n = vec.length
      if n == 0 then return -1
      end if
      if n == 1 then return 0
      end if

      val blockSize = spd.length() * 4
      var globalMin = Double.MaxValue
      var blockWithMin = 0

      var i = 0
      val loopBound = n - (n % blockSize)
      var minVec = DoubleVector.broadcast(spd, globalMin)

      while i < loopBound do
        // Load 4 SIMD vectors (32 elements total)
        val v1 = DoubleVector.fromArray(spd, vec, i)
        val v2 = DoubleVector.fromArray(spd, vec, i + spdl)
        val v3 = DoubleVector.fromArray(spd, vec, i + 2 * spdl)
        val v4 = DoubleVector.fromArray(spd, vec, i + 3 * spdl)

        // Find block maximum using tree reduction
        val max12 = v1.max(v2)
        val max34 = v3.max(v4)
        val blockMax = max12.max(max34)

        // Check if any element in this block is greater than global max
        val mask = blockMax.compare(VectorOperators.LT, minVec)

        if mask.anyTrue() then // Check if any element is greater - rarely executed
          // Update global maximum within this block
          var j = i
          while j < i + blockSize do
            if vec(j) < globalMin then globalMin = vec(j)
            end if
            j += 1
          end while
          blockWithMin = i
          minVec = DoubleVector.broadcast(spd, globalMin)
        end if

        i += blockSize
      end while

      while i < n do
        if vec(i) < globalMin then
          globalMin = vec(i)
          blockWithMin = (i / blockSize) * blockSize
        end if

        i += 1
      end while

      var exactIdx = blockWithMin
      val searchEnd = Math.min(blockWithMin + blockSize, n)
      var j = blockWithMin
      while j < searchEnd do
        if vec(j) == globalMin then
          exactIdx = j
          return exactIdx
        end if
        j += 1
      end while

      exactIdx
    end argmin

    // def max: Double =
    //   vec(blas.idamax(vec.length, vec, 1)) // No JS version

    inline def `zeroWhere!`(
        other: Array[Double],
        threshold: Double,
        inline op: ComparisonOp
    ): Unit =
      assert(vec.length == other.length)
      val zero = DoubleVector.zero(spd)
      val thresh = DoubleVector.broadcast(spd, threshold)
      var i = 0

      while i < spd.loopBound(vec.length) do
        val values = DoubleVector.fromArray(spd, vec, i)
        val cmp = DoubleVector.fromArray(spd, other, i)
        val mask = inline op match
          case ComparisonOp.LE => cmp.compare(VectorOperators.LE, thresh)
          case ComparisonOp.LT => cmp.compare(VectorOperators.LT, thresh)
          case ComparisonOp.GE => cmp.compare(VectorOperators.GE, thresh)
          case ComparisonOp.GT => cmp.compare(VectorOperators.GT, thresh)
          case ComparisonOp.EQ => cmp.compare(VectorOperators.EQ, thresh)
          case ComparisonOp.NE => cmp.compare(VectorOperators.NE, thresh)
        values.blend(zero, mask).intoArray(vec, i)
        i += spdl
      end while

      while i < vec.length do
        val hit = inline op match
          case ComparisonOp.LE => other(i) <= threshold
          case ComparisonOp.LT => other(i) < threshold
          case ComparisonOp.GE => other(i) >= threshold
          case ComparisonOp.GT => other(i) > threshold
          case ComparisonOp.EQ => other(i) == threshold
          case ComparisonOp.NE => other(i) != threshold
        if hit then vec(i) = 0.0
        end if
        i += 1
      end while
    end `zeroWhere!`

    inline def zeroWhere(
        other: Array[Double],
        threshold: Double,
        inline op: ComparisonOp
    ): Array[Double] =
      val out = vec.clone()
      out.`zeroWhere!`(other, threshold, op)
      out
    end zeroWhere

  end extension

end doublearrays
