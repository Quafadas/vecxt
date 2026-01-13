package vecxt.reinsurance

import java.util.UUID
import vecxtensions.{groupCumSum, groupDiff}
import vecxt.reinsurance.Limits.Limit
import vecxt.reinsurance.Retentions.Retention
import vecxt.reinsurance.rpt.*
import vecxt.all.*
import narr.*
import jdk.incubator.vector.*

object Tower:
  inline def fromRetention(ret: Double, limits: IndexedSeq[Double]): Tower =
    val retentions = (ret +: limits.dropRight(1)).toArray.cumsum

    val layers = retentions.zip(limits).map((retention, limit) => Layer(limit, retention))
    Tower(layers)
  end fromRetention

  inline def singleShot(ret: Double, limits: IndexedSeq[Double]) =
    val retentions = (ret +: limits.dropRight(1)).toArray.cumsum

    val layers = retentions.zip(limits).map { (retention, limit) =>
      Layer(
        aggLimit = Some(limit),
        occRetention = Some(retention)
      )
    }
    Tower(layers)
  end singleShot

  inline def oneAt100(ret: Double, limits: IndexedSeq[Double]): Tower =

    val retentions = (ret +: limits.dropRight(1)).toArray.cumsum

    val layers = retentions
      .zip(limits)
      .map((retention, limit) =>
        Layer(
          occLimit = Some(limit),
          occRetention = Some(retention),
          aggLimit = Some(limit * 2),
          reinstatement = Some(Array(1.0))
        )
      )
    Tower(layers)
  end oneAt100

end Tower

case class Tower(
    layers: IndexedSeq[Layer],
    id: UUID = UUID.randomUUID(),
    name: Option[String] = None,
    subjPremium: Option[Double] = None
):
  def applyScale(scale: Double): Tower =
    Tower(
      layers = layers.map(_.applyScale(scale)),
      id = UUID.randomUUID(),
      name = name,
      subjPremium = subjPremium.map(_ * scale)
    )
  end applyScale

  /** A human friendly printout of this reinsurance tower. Skips any property which is "None" across all layers. Prints
    * a console friendly table, with consistent spacing per column.
    */
  def show: String =
    if layers.isEmpty then return s"${name.getOrElse("Tower")}: no layers"
    end if

    inline def formatDouble(value: Double): String =
      BigDecimal(value).bigDecimal.stripTrailingZeros().toPlainString

    inline def optionalColumn(label: String, f: Layer => Option[String]): Option[(String, IndexedSeq[String])] =
      val values = layers.map(f)
      if values.exists(_.isDefined) then Some(label -> values.map(_.getOrElse("-"))) else None
      end if
    end optionalColumn

    inline def requiredColumn(label: String, f: Layer => String): (String, IndexedSeq[String]) =
      label -> layers.map(f)

    val indexColumn = "Layer" -> layers.indices.map(i => (i + 1).toString)

    val columns = scala.collection.immutable
      .Vector(
        Some(indexColumn),
        optionalColumn("Name", _.layerName),
        optionalColumn("Occ Ret", l => l.occRetention.map(formatDouble)),
        optionalColumn("Occ Lim", l => l.occLimit.map(formatDouble)),
        Some(requiredColumn("Occ Type", _.occType.toString)),
        optionalColumn("Agg Ret", l => l.aggRetention.map(formatDouble)),
        optionalColumn("Agg Lim", l => l.aggLimit.map(formatDouble)),
        Some(requiredColumn("Agg Type", _.aggType.toString)),
        Some(requiredColumn("Share", l => formatDouble(l.share))),
        optionalColumn("Reinst", l => l.reinstatement.map(_.map(formatDouble).mkString("[", ", ", "]"))),
        optionalColumn("Currency", _.currency),
        optionalColumn("Premium", l => l.basePremiumAmount.map(formatDouble)),
        optionalColumn("Prem/Unit", l => l.basePremiumUnit.map(formatDouble)),
        optionalColumn("Prem Desc", _.basePremiumDescription),
        optionalColumn("Comm", l => l.commissionAmount.map(formatDouble)),
        optionalColumn("Comm/Unit", l => l.commissionUnit.map(formatDouble)),
        optionalColumn("Comm Desc", _.commissionDescription),
        optionalColumn("Broker", l => l.brokerageAmount.map(formatDouble)),
        optionalColumn("Broker/Unit", l => l.brokerageUnit.map(formatDouble)),
        optionalColumn("Broker Desc", _.brokerageDescription),
        optionalColumn("Tax", l => l.taxAmount.map(formatDouble)),
        optionalColumn("Tax/Unit", l => l.taxUnit.map(formatDouble)),
        optionalColumn("Tax Desc", _.taxDescription),
        optionalColumn("Fee", l => l.feeAmount.map(formatDouble)),
        optionalColumn("Fee/Unit", l => l.feeUnit.map(formatDouble)),
        optionalColumn("Fee Desc", _.feeDescription)
      )
      .flatten

    val widths = columns.map { case (label, rows) =>
      math.max(label.length, rows.map(_.length).maxOption.getOrElse(0))
    }

    inline def pad(value: String, width: Int): String =
      val padding = width - value.length
      if padding <= 0 then value else value + (" " * padding)
      end if
    end pad

    val header = columns.zip(widths).map { case ((label, _), w) => pad(label, w) }.mkString(" | ")
    val separator = widths.map(w => "-" * w).mkString("-+-")
    val rows = layers.indices.map { rowIdx =>
      columns.zip(widths).map { case ((_, vals), w) => pad(vals(rowIdx), w) }.mkString(" | ")
    }

    val meta = Seq(
      Some(s"${name.getOrElse("Tower")}: ${layers.length} layer(s)"),
      subjPremium.map(v => s"Subject premium: ${formatDouble(v)}"),
      Some(s"Id: $id")
    ).flatten

    (meta ++ Seq(header, separator) ++ rows).mkString(System.lineSeparator)
  end show

  // Optimized helper method to apply all occurrence layers directly using matrix rpt functions
  private def applyOccurrenceLayers(losses: NArray[Double]): Matrix[Double] =
    // Create result matrix with losses repeated for each layer
    val layerResults = layers.map { layer =>
      val layerLosses = losses.clone()

      // Convert to opaque types for rpt functions
      val limit = layer.occLimit.map(Limit.apply)
      val retention = layer.occRetention.map(Retention.apply)

      // Apply the appropriate function based on deductible type
      layer.occType match
        case DeductibleType.Retention =>
          layerLosses.reinsuranceFunction(limit, retention)
        case DeductibleType.Franchise =>
          layerLosses.franchiseFunction(limit, retention)
        case DeductibleType.ReverseFranchise =>
          // Apply reverse franchise manually
          var k = 0
          while k < layerLosses.length do
            val value = layerLosses(k)
            val afterRetention = layer.occRetention match
              case None      => value
              case Some(ret) => if value <= ret then value else 0.0

            layerLosses(k) = layer.occLimit match
              case None      => afterRetention
              case Some(lim) => math.min(afterRetention, lim)

            k += 1
          end while
      end match

      layerLosses
    }

    // Convert to matrix using fromColumns (each layer is a column)
    Matrix.fromColumns(layerResults*)(using vecxt.BoundsCheck.DoBoundsCheck.no)
  end applyOccurrenceLayers

  // Optimized helper method to apply aggregate layers with shares using matrix rpt functions
  private def applyAggregateLayers(values: Matrix[Double]): Matrix[Double] =
    inline given bc: vecxt.BoundsCheck.BoundsCheck = vecxt.BoundsCheck.DoBoundsCheck.no
    val layerResults = layers.zipWithIndex.map { case (layer, j) =>
      // Extract column j values
      val layerValues = new Array[Double](values.rows)
      var i = 0
      while i < values.rows do
        layerValues(i) = values(i, j)
        i += 1
      end while

      // Convert to opaque types for rpt functions
      val limit = layer.aggLimit.map(Limit.apply)
      val retention = layer.aggRetention.map(Retention.apply)
      val share = layer.share

      // Apply function with share in one step for maximum efficiency
      layer.aggType match
        case DeductibleType.Retention =>
          layerValues.reinsuranceFunction(limit, retention, share)
        case DeductibleType.Franchise =>
          layerValues.franchiseFunction(limit, retention)
          if share != 1.0 then
            var k = 0
            while k < layerValues.length do
              layerValues(k) = layerValues(k) * share
              k += 1
            end while
          end if
        case DeductibleType.ReverseFranchise =>
          // Apply reverse franchise manually
          var k = 0
          while k < layerValues.length do
            val value = layerValues(k)
            val afterRetention = layer.aggRetention match
              case None      => value
              case Some(ret) => if value <= ret then value else 0.0

            val afterLimit = layer.aggLimit match
              case None      => afterRetention
              case Some(lim) => math.min(afterRetention, lim)

            layerValues(k) = afterLimit * share
            k += 1
          end while
      end match

      NArray(layerValues*)
    }

    // Convert to matrix using fromColumns (each layer is a column)
    Matrix.fromColumns(layerResults*)(using vecxt.BoundsCheck.DoBoundsCheck.no)
  end applyAggregateLayers

  def splitAmnt(
      years: Array[Int],
      days: Array[Int],
      losses: Array[Double]
  ): (ceded: Matrix[Double], retained: Array[Double]) =
    inline given bc: vecxt.BoundsCheck.BoundsCheck = vecxt.BoundsCheck.DoBoundsCheck.no
    if losses.isEmpty then return (Matrix.zeros[Double]((0, layers.length)), Array.empty[Double])
    end if

    // Step 1: Apply occurrence layers - L = obj.occLay.applyto(osc.getAmnts)
    val occApplied = applyOccurrenceLayers(losses)

    // Step 2: Group cumulative sum by years - tmp = grpcumsum(osc.getItrs,L)
    val cumSums = Matrix.zeros[Double]((occApplied.rows, layers.length))
    for layerIdx <- layers.indices do
      val layerValues = new Array[Double](occApplied.rows)
      var i = 0
      while i < occApplied.rows do
        layerValues(i) = occApplied(i, layerIdx)
        i += 1
      end while

      val cumSummed = groupCumSum(years, layerValues)
      i = 0
      while i < cumSummed.length do
        cumSums(i, layerIdx) = cumSummed(i)
        i += 1
      end while
    end for

    // Step 3: Apply aggregate layers with shares in one step - tmp = obj.aggLay.applyto(tmp) and apply shares
    val aggApplied = applyAggregateLayers(cumSums)

    // Step 4: Group diff - covered = grpdiff(osc.getItrs,tmp) - shares already applied
    val ceded = Matrix.zeros[Double]((aggApplied.rows, layers.length))
    for layerIdx <- layers.indices do
      val layerValues = new Array[Double](aggApplied.rows)
      var i = 0
      while i < aggApplied.rows do
        layerValues(i) = aggApplied(i, layerIdx)
        i += 1
      end while

      val diffed = groupDiff(years, layerValues)
      i = 0
      while i < diffed.length do
        // Shares already applied in aggregate layer step
        ceded(i, layerIdx) = diffed(i)
        i += 1
      end while
    end for

    // Step 5: Calculate retained - retained = osc.getAmnts - sum(covered,2)
    val retained = new Array[Double](losses.length)
    var i = 0
    while i < losses.length do
      var sum = 0.0
      var j = 0
      while j < layers.length do
        sum += ceded(i, j)
        j += 1
      end while
      retained(i) = losses(i) - sum
      i += 1
    end while

    (ceded, retained)
  end splitAmnt

  /** High-performance implementation of splitAmnt optimized for SIMD and minimal allocations. Uses pre-allocated
    * matrices and in-place operations for maximum efficiency.
    */
  inline def splitAmntFast(years: Array[Int], losses: Array[Double])(using
      inline bc: vecxt.BoundsCheck.BoundsCheck
  ): (ceded: Array[Double], retained: Array[Double], splits: IndexedSeq[(Layer, Array[Double])]) =
    inline if bc then assert(years.length == losses.length)
    end if
    if losses.isEmpty then (Array.empty[Double], Array.empty[Double], layers.map(_ -> Array.empty[Double]))
    else

      val numLosses = losses.length
      val numLayers = layers.length

      // Per-layer splits (column major) and totals
      val cededSplits = IndexedSeq.fill(numLayers)(
        losses.clone()
      )
      val retained = new Array[Double](numLosses)
      val ceded = new Array[Double](numLosses)

      // SIMD species for vectorization
      val species = DoubleVector.SPECIES_PREFERRED
      val vectorSize = species.length()

      // Step 1-4: process each layer column in-place (occurrence -> cumsum -> aggregate -> diff)
      var layerIdx = 0
      while layerIdx < numLayers do
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

        // Group cumulative sum by year (scalar due to group boundaries)
        var i = 0
        while i < numLosses do
          val g = years(i)
          var cumSum = 0.0
          while i < numLosses && years(i) == g do
            cumSum += col(i)
            col(i) = cumSum
            i += 1
          end while
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

        // Group diff by year (scalar due to dependency on previous row)
        i = 0
        while i < numLosses do
          val g = years(i)
          var prevValue = 0.0
          var isFirst = true
          while i < numLosses && years(i) == g do
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

      // Step 5: total ceded per loss and retained (vectorized)
      val loopBound = species.loopBound(numLosses)
      var i = 0
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
      //tail loop for ceded loss.
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
  end splitAmntFast

  /** Apply occurrence layers directly to pre-allocated matrix storage with SIMD optimization */
  private inline def applyOccurrenceLayersFast(
      losses: Array[Double],
      cededRaw: Array[Double],
      numLosses: Int,
      numLayers: Int,
      species: VectorSpecies[java.lang.Double]
  ): Unit =
    val vectorSize = species.length()

    // Process each layer
    for layerIdx <- 0 until numLayers do
      val layer = layers(layerIdx)
      // Column-major: each column starts at layerIdx * numLosses
      val offset = layerIdx * numLosses

      // Copy losses to this layer's column first
      System.arraycopy(losses, 0, cededRaw, offset, numLosses)

      // Apply layer logic based on type
      layer.occType match
        case DeductibleType.Retention =>
          applyRetentionSIMD(cededRaw, offset, numLosses, layer.occLimit, layer.occRetention, species)
        case DeductibleType.Franchise =>
          applyFranchiseSIMD(cededRaw, offset, numLosses, layer.occLimit, layer.occRetention, species)
        case DeductibleType.ReverseFranchise =>
          applyReverseFranchise(cededRaw, offset, numLosses, layer.occLimit, layer.occRetention)
      end match
    end for
  end applyOccurrenceLayersFast

  /** SIMD-optimized retention application */
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

  /** SIMD-optimized franchise application */
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

  /** Non-vectorized reverse franchise (complex branching logic) */
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

  /** In-place group cumulative sum with SIMD optimization where possible */
  private inline def applyGroupCumSumFast(
      years: Array[Int],
      cededRaw: Array[Double],
      numLosses: Int,
      numLayers: Int,
      species: VectorSpecies[java.lang.Double]
  ): Unit =
    // Process each layer column directly in the matrix storage
    for layerIdx <- 0 until numLayers do
      // Column-major: layer layerIdx starts at layerIdx * numLosses
      val columnOffset = layerIdx * numLosses

      // Apply group cumulative sum directly on matrix storage
      var i = 0
      while i < numLosses do
        val g = years(i)
        var cumSum = 0.0

        // Process block of same group, computing cumulative sum in-place
        while i < numLosses && years(i) == g do
          cumSum += cededRaw(columnOffset + i)
          cededRaw(columnOffset + i) = cumSum
          i += 1
        end while
      end while
    end for
  end applyGroupCumSumFast

  /** Apply aggregate layers with shares directly to matrix storage */
  private inline def applyAggregateLayersFast(
      cededRaw: Array[Double],
      numLosses: Int,
      numLayers: Int,
      species: VectorSpecies[java.lang.Double]
  ): Unit =
    val vectorSize = species.length()

    // Process each layer
    for layerIdx <- 0 until numLayers do
      val layer = layers(layerIdx)
      // Column-major: layer layerIdx starts at layerIdx * numLosses
      val columnOffset = layerIdx * numLosses
      val loopBound = species.loopBound(numLosses)

      // Convert to opaque types
      val limit = layer.aggLimit.map(Limit.apply)
      val retention = layer.aggRetention.map(Retention.apply)
      val share = layer.share

      // Apply aggregate layer logic
      layer.aggType match
        case DeductibleType.Retention =>
          applyRetentionSIMD(cededRaw, columnOffset, numLosses, layer.aggLimit, layer.aggRetention, species)
          if share != 1.0 then applyShareSIMD(cededRaw, columnOffset, numLosses, share, species)
          end if
        case DeductibleType.Franchise =>
          applyFranchiseSIMD(cededRaw, columnOffset, numLosses, layer.aggLimit, layer.aggRetention, species)
          if share != 1.0 then applyShareSIMD(cededRaw, columnOffset, numLosses, share, species)
          end if
        case DeductibleType.ReverseFranchise =>
          applyReverseFranchise(cededRaw, columnOffset, numLosses, layer.aggLimit, layer.aggRetention)
          if share != 1.0 then applyShareSIMD(cededRaw, columnOffset, numLosses, share, species)
          end if
      end match
    end for
  end applyAggregateLayersFast

  /** SIMD-optimized share application */
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

  /** In-place group diff with SIMD optimization where possible */
  private inline def applyGroupDiffFast(
      years: Array[Int],
      cededRaw: Array[Double],
      numLosses: Int,
      numLayers: Int,
      species: VectorSpecies[java.lang.Double]
  ): Unit =
    // Process each layer column directly in the matrix storage
    for layerIdx <- 0 until numLayers do
      // Column-major: layer layerIdx starts at layerIdx * numLosses
      val columnOffset = layerIdx * numLosses

      // Apply group diff directly on matrix storage
      var i = 0
      while i < numLosses do
        val g = years(i)
        var prevValue = 0.0
        var isFirstInGroup = true

        // Process block of same group, computing differences in-place
        while i < numLosses && years(i) == g do
          val currentValue = cededRaw(columnOffset + i)
          if isFirstInGroup then
            // First element in group gets its own value - no change needed
            isFirstInGroup = false
          else cededRaw(columnOffset + i) = currentValue - prevValue
          end if
          prevValue = currentValue
          i += 1
        end while
      end while
    end for
  end applyGroupDiffFast

  /** SIMD-optimized retained calculation */
  private inline def calculateRetainedFast(
      originalLosses: Array[Double],
      cededRaw: Array[Double],
      retained: Array[Double],
      numLosses: Int,
      numLayers: Int,
      species: VectorSpecies[java.lang.Double]
  ): Unit =
    val vectorSize = species.length()
    val loopBound = species.loopBound(numLosses)

    var i = 0
    while i < loopBound do
      var sumVector = DoubleVector.zero(species)

      // Vectorized sum across all layers for rows i to i+vectorSize-1
      for layerIdx <- 0 until numLayers do
        // Column-major: layer layerIdx starts at layerIdx * numLosses
        val columnOffset = layerIdx * numLosses
        val layerVector = DoubleVector.fromArray(species, cededRaw, columnOffset + i)
        sumVector = sumVector.add(layerVector)
      end for

      // Calculate retained = original - sum(ceded)
      val originalVector = DoubleVector.fromArray(species, originalLosses, i)
      val retainedVector = originalVector.sub(sumVector)
      retainedVector.intoArray(retained, i)

      i += vectorSize
    end while

    // Handle remaining elements
    while i < numLosses do
      var sum = 0.0
      for layerIdx <- 0 until numLayers do
        // Column-major: layer layerIdx starts at layerIdx * numLosses
        val columnOffset = layerIdx * numLosses
        sum += cededRaw(columnOffset + i)
      end for
      retained(i) = originalLosses(i) - sum
      i += 1
    end while
  end calculateRetainedFast
end Tower
