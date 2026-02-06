package vecxt_re

import vecxt.BoundsCheck.BoundsCheck

object SplitLosses:
  extension (tower: Tower)
    /** Optimsie for small number of layers. Large numbers of claims.
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
    ): (
        ceded: Array[Double],
        retained: Array[Double],
        splits: IndexedSeq[(layer: Layer, cededToLayer: Array[Double])]
    ) =
      inline if bc then assert(years.length == losses.length)
      end if
      if losses.isEmpty then (Array.empty[Double], Array.empty[Double], tower.layers.map(_ -> Array.empty[Double]))
      else

        val layers = tower.layers
        val numLosses = losses.length
        val numLayers = layers.length

        // Per-layer splits (column major) and totals
        val cededSplits = IndexedSeq.fill(numLayers)(losses.clone())
        val retained = new Array[Double](numLosses)
        val ceded = new Array[Double](numLosses)

        var layerIdx = 0
        while layerIdx < numLayers do
          val layer = layers(layerIdx)
          val col = cededSplits(layerIdx)

          layer.occType match
            case DeductibleType.Retention =>
              applyRetention(col, 0, numLosses, layer.occLimit, layer.occRetention)
            case DeductibleType.Franchise =>
              applyFranchise(col, 0, numLosses, layer.occLimit, layer.occRetention)
            case DeductibleType.ReverseFranchise =>
              applyReverseFranchise(col, 0, numLosses, layer.occLimit, layer.occRetention)
          end match

          // Group cumulative sum on the fly by detecting year boundaries
          var i = 0
          while i < numLosses do
            val year = years(i)
            var cumSum = 0.0
            while i < numLosses && years(i) == year do
              cumSum += col(i)
              col(i) = cumSum
              i += 1
            end while
          end while

          layer.aggType match
            case DeductibleType.Retention =>
              applyRetention(col, 0, numLosses, layer.aggLimit, layer.aggRetention)
              if layer.share != 1.0 then applyShare(col, 0, numLosses, layer.share)
              end if
            case DeductibleType.Franchise =>
              applyFranchise(col, 0, numLosses, layer.aggLimit, layer.aggRetention)
              if layer.share != 1.0 then applyShare(col, 0, numLosses, layer.share)
              end if
            case DeductibleType.ReverseFranchise =>
              applyReverseFranchise(col, 0, numLosses, layer.aggLimit, layer.aggRetention)
              if layer.share != 1.0 then applyShare(col, 0, numLosses, layer.share)
              end if
          end match

          // Group diff on the fly by detecting year boundaries
          i = 0
          while i < numLosses do
            val year = years(i)
            var prevValue = 0.0
            var isFirst = true
            while i < numLosses && years(i) == year do
              val current = col(i)
              if isFirst then isFirst = false
              else col(i) = current - prevValue
              end if
              prevValue = current
              i += 1
            end while
          end while

          layerIdx += 1
        end while

        var i = 0
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

  private inline def applyRetention(
      data: Array[Double],
      offset: Int,
      length: Int,
      limit: Option[Double],
      retention: Option[Double]
  ): Unit =
    (retention, limit) match
      case (Some(ret), Some(lim)) =>
        var i = 0
        while i < length do
          val value = data(offset + i)
          data(offset + i) = math.min(math.max(value - ret, 0.0), lim)
          i += 1
        end while
      case (Some(ret), None) =>
        var i = 0
        while i < length do
          val value = data(offset + i)
          data(offset + i) = math.max(value - ret, 0.0)
          i += 1
        end while
      case (None, Some(lim)) =>
        var i = 0
        while i < length do
          data(offset + i) = math.min(data(offset + i), lim)
          i += 1
        end while
      case (None, None) =>
      // No changes needed
    end match
  end applyRetention

  private inline def applyFranchise(
      data: Array[Double],
      offset: Int,
      length: Int,
      limit: Option[Double],
      retention: Option[Double]
  ): Unit =
    (retention, limit) match
      case (Some(ret), Some(lim)) =>
        var i = 0
        while i < length do
          val value = data(offset + i)
          data(offset + i) = if value > ret then math.min(value, lim) else 0.0
          i += 1
        end while
      case (Some(ret), None) =>
        var i = 0
        while i < length do
          val value = data(offset + i)
          data(offset + i) = if value > ret then value else 0.0
          i += 1
        end while
      case (None, Some(lim)) =>
        var i = 0
        while i < length do
          data(offset + i) = math.min(data(offset + i), lim)
          i += 1
        end while
      case (None, None) =>
      // No changes needed
    end match
  end applyFranchise

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

  private inline def applyShare(
      data: Array[Double],
      offset: Int,
      length: Int,
      share: Double
  ): Unit =
    var i = 0
    while i < length do
      data(offset + i) *= share
      i += 1
    end while
  end applyShare
end SplitLosses
