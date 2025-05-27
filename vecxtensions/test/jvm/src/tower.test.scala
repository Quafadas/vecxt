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

class TowerSuite extends munit.FunSuite:

  val sampleLayer = Layer(
    layerId = UUID.randomUUID(),
    layerName = Some("Primary Layer"),
    aggLimit = Some(1000000.0),
    aggRetention = Some(100000.0),
    occLimit = Some(500000.0),
    occRetention = Some(50000.0),
    basePremiumUnit = Some(100000.0),
    basePremiumDescription = Some("Base premium"),
    commissionUnit = Some(15000.0),
    commissionDescription = Some("Commission"),
    currency = Some("USD")
  )

  val sampleTower = Tower(
    id = UUID.randomUUID(),
    name = Some("Test Tower"),
    layers = Seq(sampleLayer),
    subjPremium = Some(2000000.0)
  )

  test("applyScale should scale all monetary values correctly"):
    val scale = 2.5
    val scaledTower = sampleTower.applyScale(scale)
    
    // Check that subjPremium is scaled
    assertEquals(scaledTower.subjPremium, Some(5000000.0))
    
    // Check that layer monetary values are scaled
    val scaledLayer = scaledTower.layers.head
    assertEquals(scaledLayer.aggLimit, Some(2500000.0))
    assertEquals(scaledLayer.aggRetention, Some(250000.0))
    assertEquals(scaledLayer.occLimit, Some(1250000.0))
    assertEquals(scaledLayer.occRetention, Some(125000.0))
    assertEquals(scaledLayer.basePremiumUnit, Some(250000.0))
    assertEquals(scaledLayer.commissionUnit, Some(37500.0))

  test("applyScale should preserve non-monetary fields"):
    val scale = 1.5
    val scaledTower = sampleTower.applyScale(scale)
    
    // Name should be preserved
    assertEquals(scaledTower.name, sampleTower.name)
    
    // Layer properties should be preserved
    val originalLayer = sampleTower.layers.head
    val scaledLayer = scaledTower.layers.head
    
    assertEquals(scaledLayer.layerName, originalLayer.layerName)
    assertEquals(scaledLayer.aggType, originalLayer.aggType)
    assertEquals(scaledLayer.occType, originalLayer.occType)
    assertEquals(scaledLayer.share, originalLayer.share)
    assertEquals(scaledLayer.currency, originalLayer.currency)

  test("applyScale should generate new UUID for tower"):
    val scaledTower = sampleTower.applyScale(2.0)
    
    assertNotEquals(scaledTower.id, sampleTower.id)

  test("applyScale should handle None values gracefully"):
    val towerWithNones = Tower(
      id = UUID.randomUUID(),
      name = None,
      layers = Seq(Layer(currency = Some("EUR"))),
      subjPremium = None
    )
    
    val scaledTower = towerWithNones.applyScale(3.0)
    
    assertEquals(scaledTower.name, None)
    assertEquals(scaledTower.subjPremium, None)
    assertEquals(scaledTower.layers.size, 1)

  test("applyScale should handle multiple layers"):
    val layer2 = Layer(
      layerName = Some("Excess Layer"),
      aggLimit = Some(5000000.0),
      occLimit = Some(2000000.0),
      currency = Some("USD")
    )
    
    val multiLayerTower = sampleTower.copy(layers = Seq(sampleLayer, layer2))
    val scaledTower = multiLayerTower.applyScale(0.5)
    
    assertEquals(scaledTower.layers.size, 2)
    assertEquals(scaledTower.layers(1).aggLimit, Some(2500000.0))
    assertEquals(scaledTower.layers(1).occLimit, Some(1000000.0))

  test("applyScale with scale of 1.0 should preserve monetary values"):
    val scaledTower = sampleTower.applyScale(1.0)
    
    assertEquals(scaledTower.subjPremium, sampleTower.subjPremium)
    
    val originalLayer = sampleTower.layers.head
    val scaledLayer = scaledTower.layers.head
    
    assertEquals(scaledLayer.aggLimit, originalLayer.aggLimit)
    assertEquals(scaledLayer.aggRetention, originalLayer.aggRetention)
    assertEquals(scaledLayer.occLimit, originalLayer.occLimit)
    assertEquals(scaledLayer.occRetention, originalLayer.occRetention)


end TowerSuite