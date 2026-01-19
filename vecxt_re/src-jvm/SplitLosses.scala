package vecxt_re

import java.util.concurrent.Executors

import jdk.incubator.vector.{DoubleVector, VectorOperators, VectorSpecies}
import vecxt.BoundsCheck.BoundsCheck

object SplitLosses:
  extension (tower: Tower)
    /** High-performance SIMD optimized version for small number of layers (1 -5) and large number of claims.
      *
      * You may assume that year groups already sorted and are in order.
      *
      * @param years
      * @param losses
      * @param bc
      * @return
      */
    inline def splitAmntFast(years: Array[Int], losses: Array[Double])(using
        inline bc: BoundsCheck
    ): (ceded: Array[Double], retained: Array[Double], splits: IndexedSeq[(Layer, Array[Double])]) =
      inline if bc then assert(years.length == losses.length)
      end if
      if losses.isEmpty then (Array.empty[Double], Array.empty[Double], tower.layers.map(_ -> Array.empty[Double]))
      else

        val layers = tower.layers
        val numLosses = losses.length
        val numLayers = layers.length

        // Per-layer splits (column major) and totals
        val cededSplits = IndexedSeq.fill(numLayers)(losses.clone())
        val retained = losses.clone()
        val ceded = new Array[Double](numLosses)

        // SIMD species for vectorization
        val species = DoubleVector.SPECIES_PREFERRED
        val vectorSize = species.length()

        def processLayer(layerIdx: Int): Unit =
          val layer = layers(layerIdx)
          val col = cededSplits(layerIdx)

          // Occurrence layer
          layer.occType match
            case DeductibleType.Retention =>
              applyRetentionSIMD(col, 0, numLosses, layer.occLimit, layer.occRetention, species)
            case DeductibleType.Franchise =>
              applyFranchiseSIMD(col, 0, numLosses, layer.occLimit, layer.occRetention, species)
            case DeductibleType.ReverseFranchise =>
              applyReverseFranchise(col, 0, numLosses, layer.occLimit, layer.occRetention)
          end match

          // Cumulative sum within each year group using a single pass
          var i = 0
          var prevYear = -1
          var cumSum = 0.0
          while i < numLosses do
            if years(i) != prevYear then
              prevYear = years(i)
              cumSum = col(i)
            else cumSum += col(i)
            end if
            col(i) = cumSum
            i += 1
          end while

          // Aggregate layer with share
          layer.aggType match
            case DeductibleType.Retention =>
              applyRetentionSIMD(col, 0, numLosses, layer.aggLimit, layer.aggRetention, species)
              if layer.share != 1.0 then applyShareSIMD(col, 0, numLosses, layer.share, species)
              end if
            case DeductibleType.Franchise =>
              applyFranchiseSIMD(col, 0, numLosses, layer.aggLimit, layer.aggRetention, species)
              if layer.share != 1.0 then applyShareSIMD(col, 0, numLosses, layer.share, species)
              end if
            case DeductibleType.ReverseFranchise =>
              applyReverseFranchise(col, 0, numLosses, layer.aggLimit, layer.aggRetention)
              if layer.share != 1.0 then applyShareSIMD(col, 0, numLosses, layer.share, species)
              end if
          end match

          // Convert cumulative back to per-loss (diff) within year groups in one pass
          i = 0
          var prevVal = 0.0
          prevYear = -1
          var firstInGroup = true
          while i < numLosses do
            if years(i) != prevYear then
              prevYear = years(i)
              prevVal = col(i)
              firstInGroup = false
            else
              val current = col(i)
              col(i) = current - prevVal
              prevVal = current
            end if
            i += 1
          end while
        end processLayer

        /** Benchmarking 15.01.2026
          *
          * If there is only one layer, no point paying the setup cost of parralism.
          *
          * if there is only a small number of losses to process, then benchmarking suggests that the L1 cache keeps
          * everything in memory and crushes the setup and teardown costs of parrallism
          *
          * So parrallism only helps, in the case where we have a reasonably large number of losses and more than one
          * layer...
          */
        (numLosses, numLayers) match
          case (_, 1)                => processLayer(0)
          case (nl, _) if nl < 50000 =>
            var i = 0
            while i < numLayers do
              processLayer(i)
              i += 1
            end while
          case _ =>
            val threads = math.min(numLayers, Runtime.getRuntime.availableProcessors())
            val executor = Executors.newFixedThreadPool(threads)
            try
              val futures: IndexedSeq[java.util.concurrent.Future[?]] = (0 until numLayers).map { idx =>
                executor.submit(
                  new Runnable:
                    override def run(): Unit = processLayer(idx)
                )
              }
              futures.foreach(_.get())
            finally executor.shutdown()
            end try
        end match

        // Step 5: total ceded per loss and retained (vectorized)
        val loopBound = species.loopBound(numLosses)
        var i = 0
        var layerIdx = 0
        while i < loopBound do
          var sumVector = DoubleVector.zero(species)
          layerIdx = 0
          while layerIdx < numLayers do
            val col = cededSplits(layerIdx)
            val v = DoubleVector.fromArray(species, col, i)
            sumVector = sumVector.add(v)
            layerIdx += 1
          end while

          sumVector.intoArray(ceded, i)
          val originalVector = DoubleVector.fromArray(species, losses, i)
          val retainedVector = originalVector.sub(sumVector)
          retainedVector.intoArray(retained, i)
          i += vectorSize
        end while

        // Tail loop
        while i < numLosses do
          var sum = 0.0
          layerIdx = 0
          while layerIdx < numLayers do
            sum += cededSplits(layerIdx)(i)
            layerIdx += 1
          end while
          ceded(i) = sum
          retained(i) = losses(i) - sum
          i += 1
        end while

        (ceded, retained, layers.zip(cededSplits))
      end if
  end extension

  private inline def applyRetentionSIMD(
      data: Array[Double],
      offset: Int,
      length: Int,
      limit: Option[Double],
      retention: Option[Double],
      species: VectorSpecies[java.lang.Double]
  ): Unit =
    val vectorSize = species.length()
    val loopBound = species.loopBound(length)

    (retention, limit) match
      case (Some(ret), Some(lim)) =>
        val retVector = DoubleVector.broadcast(species, ret)
        val limVector = DoubleVector.broadcast(species, lim)

        var i = 0
        while i < loopBound do
          val vector = DoubleVector.fromArray(species, data, offset + i)
          val afterRet = vector.sub(retVector).max(0.0)
          val result = afterRet.min(limVector)
          result.intoArray(data, offset + i)
          i += vectorSize
        end while

        // Handle remaining elements
        while i < length do
          val value = data(offset + i)
          data(offset + i) = math.min(math.max(value - ret, 0.0), lim)
          i += 1
        end while

      case (Some(ret), None) =>
        val retVector = DoubleVector.broadcast(species, ret)

        var i = 0
        while i < loopBound do
          val vector = DoubleVector.fromArray(species, data, offset + i)
          val result = vector.sub(retVector).max(0.0)
          result.intoArray(data, offset + i)
          i += vectorSize
        end while

        while i < length do
          val value = data(offset + i)
          data(offset + i) = math.max(value - ret, 0.0)
          i += 1
        end while

      case (None, Some(lim)) =>
        val limVector = DoubleVector.broadcast(species, lim)

        var i = 0
        while i < loopBound do
          val vector = DoubleVector.fromArray(species, data, offset + i)
          val result = vector.min(limVector)
          result.intoArray(data, offset + i)
          i += vectorSize
        end while

        while i < length do
          data(offset + i) = math.min(data(offset + i), lim)
          i += 1
        end while

      case (None, None) =>
      // No changes needed
    end match
  end applyRetentionSIMD

  private inline def applyFranchiseSIMD(
      data: Array[Double],
      offset: Int,
      length: Int,
      limit: Option[Double],
      retention: Option[Double],
      species: VectorSpecies[java.lang.Double]
  ): Unit =
    val vectorSize = species.length()
    val loopBound = species.loopBound(length)

    (retention, limit) match
      case (Some(ret), Some(lim)) =>
        val retVector = DoubleVector.broadcast(species, ret)
        val limVector = DoubleVector.broadcast(species, lim)
        val zeroVector = DoubleVector.zero(species)

        var i = 0
        while i < loopBound do
          val vector = DoubleVector.fromArray(species, data, offset + i)
          val mask = vector.compare(VectorOperators.GT, retVector)
          val result = vector.min(limVector).blend(zeroVector, mask.not())
          result.intoArray(data, offset + i)
          i += vectorSize
        end while

        while i < length do
          val value = data(offset + i)
          data(offset + i) = if value > ret then math.min(value, lim) else 0.0
          i += 1
        end while

      case (Some(ret), None) =>
        val retVector = DoubleVector.broadcast(species, ret)
        val zeroVector = DoubleVector.zero(species)

        var i = 0
        while i < loopBound do
          val vector = DoubleVector.fromArray(species, data, offset + i)
          val mask = vector.compare(VectorOperators.GT, retVector)
          val result = vector.blend(zeroVector, mask.not())
          result.intoArray(data, offset + i)
          i += vectorSize
        end while

        while i < length do
          val value = data(offset + i)
          data(offset + i) = if value > ret then value else 0.0
          i += 1
        end while

      case (None, Some(lim)) =>
        val limVector = DoubleVector.broadcast(species, lim)

        var i = 0
        while i < loopBound do
          val vector = DoubleVector.fromArray(species, data, offset + i)
          val result = vector.min(limVector)
          result.intoArray(data, offset + i)
          i += vectorSize
        end while

        while i < length do
          data(offset + i) = math.min(data(offset + i), lim)
          i += 1
        end while

      case (None, None) =>
      // No changes needed
    end match
  end applyFranchiseSIMD

  private inline def applyReverseFranchise(
      data: Array[Double],
      offset: Int,
      length: Int,
      limit: Option[Double],
      retention: Option[Double]
  ): Unit =
    var i = 0
    while i < length do
      val value = data(offset + i)
      val afterRetention = retention match
        case None      => value
        case Some(ret) => if value <= ret then value else 0.0

      data(offset + i) = limit match
        case None      => afterRetention
        case Some(lim) => math.min(afterRetention, lim)

      i += 1
    end while
  end applyReverseFranchise

  private inline def applyShareSIMD(
      data: Array[Double],
      offset: Int,
      length: Int,
      share: Double,
      species: VectorSpecies[java.lang.Double]
  ): Unit =
    val vectorSize = species.length()
    val loopBound = species.loopBound(length)
    val shareVector = DoubleVector.broadcast(species, share)

    var i = 0
    while i < loopBound do
      val vector = DoubleVector.fromArray(species, data, offset + i)
      val result = vector.mul(shareVector)
      result.intoArray(data, offset + i)
      i += vectorSize
    end while

    while i < length do
      data(offset + i) *= share
      i += 1
    end while
  end applyShareSIMD
end SplitLosses
