package vecxt.reinsurance

import vecxt.BoundsCheck.BoundsCheck
import narr.*

object SplitLosses:
  extension (tower: Tower)
    inline def splitAmntFast(years: NArray[Int], losses: NArray[Double])(using
        inline bc: BoundsCheck
    ): (ceded: NArray[Double], retained: NArray[Double], splits: IndexedSeq[(Layer, NArray[Double])]) =
      inline if bc then assert(years.length == losses.length)
      end if
      if losses.isEmpty then (NArray.empty[Double], NArray.empty[Double], tower.layers.map(_ -> NArray.empty[Double]))
      else

        val layers = tower.layers
        val numLosses = losses.length
        val numLayers = layers.length

        val cededSplits = IndexedSeq.fill(numLayers)(narr.copy[Double](losses))
        val retained = new NArray[Double](numLosses)
        val ceded = new NArray[Double](numLosses)        

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

  private inline def applyRetention(
      data: NArray[Double],
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
      data: NArray[Double],
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
      data: NArray[Double],
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
      data: NArray[Double],
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
