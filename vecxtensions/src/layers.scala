/*
 * Copyright 2023 quafadas
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package vecxt.reinsurance

import java.util.UUID
import vecxt.arrays.*

case class Layer(
    layerId: UUID = UUID.randomUUID(),
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
    reinstatement: Option[List[Double]] = None,
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

case class Tower(id: UUID, name: Option[String], layers: Seq[Layer], subjPremium: Option[Double] = None):
  def applyScale(scale: Double): Tower =
    Tower(
      id = UUID.randomUUID(),
      name = name,
      layers = layers.map(_.applyScale(scale)),
      subjPremium = subjPremium.map(_ * scale)
    )
  end applyScale
end Tower
