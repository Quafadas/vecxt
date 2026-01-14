package vecxt.reinsurance

import java.util.UUID
import vecxtensions.{groupCumSum, groupDiff}
import vecxt.reinsurance.Limits.Limit
import vecxt.reinsurance.Retentions.Retention
import vecxt.reinsurance.rpt.*
import vecxt.all.*
import vecxt.all.given

object Tower:
  inline def fromRetention(ret: Double, limits: IndexedSeq[Double]): Tower =
    val retentions = Array((ret +: limits.dropRight(1))*).cumsum.toArray

    val layers = retentions.zip(limits).map((retention, limit) => Layer(limit, retention))
    Tower(layers)
  end fromRetention

  inline def singleShot(ret: Double, limits: IndexedSeq[Double]) =
    val retentions = Array((ret +: limits.dropRight(1))*).cumsum.toArray

    val layers = retentions.zip(limits).map { (retention, limit) =>
      Layer(
        aggLimit = Some(limit),
        occRetention = Some(retention)
      )
    }
    Tower(layers)
  end singleShot

  inline def oneAt100(ret: Double, limits: IndexedSeq[Double]): Tower =

    val retentions = Array((ret +: limits.dropRight(1))*).cumsum.toArray

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
    id: Long = scala.util.Random.nextLong(),
    name: Option[String] = None,
    subjPremium: Option[Double] = None
):
  def applyScale(scale: Double): Tower =
    Tower(
      layers = layers.map(_.applyScale(scale)),
      id = scala.util.Random.nextLong(),
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
  private def applyOccurrenceLayers(losses: Array[Double]): Matrix[Double] =
    // Create result matrix with losses repeated for each layer
    val layerResults: IndexedSeq[Array[Double]] = layers.map { layer =>
      val layerLosses = Array(losses*)

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
    Matrix.fromColumns[Double](layerResults.toSeq*)(using vecxt.BoundsCheck.DoBoundsCheck.no)
  end applyOccurrenceLayers

  // Optimized helper method to apply aggregate layers with shares using matrix rpt functions
  private def applyAggregateLayers(values: Matrix[Double]): Matrix[Double] =
    inline given bc: vecxt.BoundsCheck.BoundsCheck = vecxt.BoundsCheck.DoBoundsCheck.no
    val layerResults: IndexedSeq[Array[Double]] = layers.zipWithIndex.map { case (layer, j) =>
      // Extract column j values
      val layerValuesArray = new Array[Double](values.rows)
      var i = 0
      while i < values.rows do
        layerValuesArray(i) = values(i, j)
        i += 1
      end while

      val layerValues = Array(layerValuesArray*)

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

      layerValues
    }

    // Convert to matrix using fromColumns (each layer is a column)
    Matrix.fromColumns[Double](layerResults.toSeq*)(using vecxt.BoundsCheck.DoBoundsCheck.no)
  end applyAggregateLayers

  def splitAmnt(
      years: Array[Int],
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
end Tower
