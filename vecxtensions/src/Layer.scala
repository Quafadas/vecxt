package vecxt.reinsurance

object Layer:
  inline def apply(limit: Double, ret: Double): Layer =
    Layer(
      occLimit = Some(limit),
      occRetention = Some(ret)
    )

  /** reinstaements is the % to reinstate. e.g. Array(0) is one free
    */
  inline def apply(occLimit: Double, occRet: Double, aggLimit: Double, reinstatements: Array[Double]): Layer =
    Layer(
      occLimit = Some(occLimit),
      occRetention = Some(occRet),
      aggLimit = Some(aggLimit),
      reinstatement = Some(reinstatements)
    )
end Layer

case class Layer(
    layerId: Long = scala.util.Random.nextLong(),
    layerName: Option[String] = None,
    aggLimit: Option[Double] = None,
    aggRetention: Option[Double] = None,
    aggType: DeductibleType = DeductibleType.Retention,
    occLimit: Option[Double] = None,
    occRetention: Option[Double] = None,
    occType: DeductibleType = DeductibleType.Retention,
    share: Double = 1,
    basePremiumAmount: Option[Double] = None,
    basePremiumUnit: Option[Double] = None,
    basePremiumDescription: Option[String] = None,
    commissionAmount: Option[Double] = None,
    commissionUnit: Option[Double] = None,
    commissionDescription: Option[String] = None,
    brokerageAmount: Option[Double] = None,
    brokerageUnit: Option[Double] = None,
    brokerageDescription: Option[String] = None,
    taxAmount: Option[Double] = None,
    taxUnit: Option[Double] = None,
    taxDescription: Option[String] = None,
    feeAmount: Option[Double] = None,
    feeUnit: Option[Double] = None,
    feeDescription: Option[String] = None,
    reinstatement: Option[Array[Double]] = None,
    currency: Option[String] = None
):
  lazy val aggLimitString = aggLimit.map(_.toString)
  lazy val aggDeductibleString = aggRetention.map(_.toString)
  lazy val occLimitString = occLimit.map(_.toString)
  lazy val occDeductibleString = occRetention.map(_.toString)
  lazy val premimuAmountString = basePremiumAmount.map(_.toString)
  lazy val premiumUnitString = basePremiumUnit.map(_.toString)
  lazy val brokerageAmountString = brokerageAmount.map(_.toString)
  lazy val brokerageUnitString = brokerageUnit.map(_.toString)
  lazy val occLayer = Sublayer(occLimit, occRetention, LossCalc.Occ, occType)
  lazy val aggLayer = Sublayer(aggLimit, aggRetention, LossCalc.Agg, aggType)

  /** The smallest claim which exhausts the first limit of this layer */
  lazy val cap = occLimit match
    case Some(occLimit) =>
      occType match
        case DeductibleType.Retention => occLimit + occRetention.getOrElse(0.0)
        case DeductibleType.Franchise => occLimit
        // A cap is not a meaningful concept for a reverse franchise. The behaviour is non monotonic.
        // We prefer NaN to an exception here to indicate that the concept does not make sense.
        case DeductibleType.ReverseFranchise => Double.NaN //

    case None => Double.PositiveInfinity

  inline def applyScale(scale: Double): Layer =
    Layer(
      layerId = layerId,
      layerName = layerName,
      aggLimit.map(_ * scale),
      aggRetention = aggRetention.map(_ * scale),
      aggType,
      occLimit.map(_ * scale),
      occRetention.map(_ * scale),
      occType,
      share,
      basePremiumAmount,
      basePremiumUnit.map(_ * scale),
      basePremiumDescription.map(d => s"$d at $basePremiumUnit * $scale"),
      commissionAmount,
      commissionUnit.map(_ * scale),
      commissionDescription.map(d => s"$d at $commissionUnit * $scale"),
      brokerageAmount,
      brokerageUnit.map(_ * scale),
      brokerageDescription.map(d => s"$d at $brokerageUnit * $scale"),
      taxAmount,
      taxUnit.map(_ * scale),
      taxDescription.map(d => s"$d at $taxUnit * $scale"),
      feeAmount,
      feeUnit.map(_ * scale),
      feeDescription.map(d => s"$d at $feeUnit * $scale"),
      reinstatement,
      currency
    )
  end applyScale
end Layer

case class Sublayer(limit: Option[Double], retention: Option[Double], layerType: LossCalc, aggOrOcc: DeductibleType)
