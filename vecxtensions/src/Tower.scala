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

end Tower
