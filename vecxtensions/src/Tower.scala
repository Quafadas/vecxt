package vecxt.reinsurance

import java.util.UUID
import vecxtensions.{groupCumSum, groupDiff}
import vecxt.reinsurance.Limits.Limit
import vecxt.reinsurance.Retentions.Retention
import vecxt.reinsurance.rpt.*
import vecxt.all.*
import vecxt.all.given
import narr.*

case class Tower( layers: Seq[Layer],  id: UUID = UUID.randomUUID(), name: Option[String] = None ,subjPremium: Option[Double] = None):
  def applyScale(scale: Double): Tower =
    Tower(
      layers = layers.map(_.applyScale(scale)),
      id = UUID.randomUUID(),
      name = name,      
      subjPremium = subjPremium.map(_ * scale)
    )
  end applyScale

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
              case None => value
              case Some(ret) => if value <= ret then value else 0.0
            
            layerLosses(k) = layer.occLimit match
              case None => afterRetention  
              case Some(lim) => math.min(afterRetention, lim)
            
            k += 1
          end while
      
      layerLosses
    }
    
    // Convert to matrix using fromColumns (each layer is a column)
    Matrix.fromColumns(layerResults*)
  end applyOccurrenceLayers

  // Optimized helper method to apply aggregate layers with shares using matrix rpt functions  
  private def applyAggregateLayers(values: Matrix[Double]): Matrix[Double] =
    // Process each layer separately  
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
              case None => value
              case Some(ret) => if value <= ret then value else 0.0
            
            val afterLimit = layer.aggLimit match
              case None => afterRetention  
              case Some(lim) => math.min(afterRetention, lim)
            
            layerValues(k) = afterLimit * share
            k += 1
          end while
      
      NArray(layerValues*)
    }
    
    // Convert to matrix using fromColumns (each layer is a column)
    Matrix.fromColumns(layerResults*)
  end applyAggregateLayers

  def splitAmnt(years: Array[Int], days: Array[Int], losses: Array[Double]): (ceded: Matrix[Double], retained: Array[Double]) =
    if losses.isEmpty then
      return (Matrix.zeros[Double]((0, layers.length)), Array.empty[Double])
    
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
