package vecxt.reinsurance

import java.util.UUID
import vecxtensions.{groupCumSum, groupDiff}
import vecxt.reinsurance.Limits.Limit
import vecxt.reinsurance.Retentions.Retention
import vecxt.reinsurance.rpt.*
import vecxt.all.*
import vecxt.all.given
import narr.*
import jdk.incubator.vector.*

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

  /** 
   * High-performance implementation of splitAmnt optimized for SIMD and minimal allocations.
   * Uses pre-allocated matrices and in-place operations for maximum efficiency.
   */
  def splitAmntFast(years: Array[Int], days: Array[Int], losses: Array[Double]): (ceded: Matrix[Double], retained: Array[Double]) =
    if losses.isEmpty then
      return (Matrix.zeros[Double]((0, layers.length)), Array.empty[Double])
    
    val numLosses = losses.length
    val numLayers = layers.length
    
    // Pre-allocate result matrix - single allocation for entire computation
    val cededMatrix = Matrix.zeros[Double]((numLosses, numLayers))
    val cededRaw = cededMatrix.raw // Direct access to underlying array
    val retained = new Array[Double](numLosses)
    
    // SIMD species for vectorization
    val species = DoubleVector.SPECIES_PREFERRED
    val vectorSize = species.length()
    
    // Step 1: Apply occurrence layers directly to matrix storage with SIMD
    applyOccurrenceLayersFast(losses, cededRaw, numLosses, numLayers, species)
    
    // Step 2: In-place group cumulative sum using SIMD where possible  
    applyGroupCumSumFast(years, cededRaw, numLosses, numLayers, species)
    
    // Step 3: Apply aggregate layers with shares in-place
    applyAggregateLayersFast(cededRaw, numLosses, numLayers, species)
    
    // Step 4: In-place group diff 
    applyGroupDiffFast(years, cededRaw, numLosses, numLayers, species)
    
    // Step 5: Calculate retained with SIMD - vectorized sum and subtract
    calculateRetainedFast(losses, cededRaw, retained, numLosses, numLayers, species)
    
    (cededMatrix, retained)
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
        case None => value
        case Some(ret) => if value <= ret then value else 0.0
      
      data(offset + i) = limit match
        case None => afterRetention  
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
          if share != 1.0 then
            applyShareSIMD(cededRaw, columnOffset, numLosses, share, species)
        case DeductibleType.Franchise =>
          applyFranchiseSIMD(cededRaw, columnOffset, numLosses, layer.aggLimit, layer.aggRetention, species)
          if share != 1.0 then
            applyShareSIMD(cededRaw, columnOffset, numLosses, share, species)
        case DeductibleType.ReverseFranchise =>
          applyReverseFranchise(cededRaw, columnOffset, numLosses, layer.aggLimit, layer.aggRetention)
          if share != 1.0 then
            applyShareSIMD(cededRaw, columnOffset, numLosses, share, species)
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
          else 
            cededRaw(columnOffset + i) = currentValue - prevValue
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
  
  /**
   * High-performance implementation with single-loop optimization.
   * Inlines all logic into a single outer loop that processes each loss event once,
   * applying all transformations for all layers in a single pass.
   * 
   * @param years Immutable array of year identifiers (must be sorted)
   * @param days Immutable array of day identifiers 
   * @param losses Immutable array of loss amounts
   * @return Tuple of (ceded: flat array, retained: array, splits: sequence of (layer, layer-ceded-array))
   */
  def splitAmnt3(years: IArray[Int], days: IArray[Int], losses: IArray[Double]): (ceded: Array[Double], retained: Array[Double], splits: Seq[(Layer, Array[Double])]) =
    if losses.isEmpty then
      return (Array.empty[Double], Array.empty[Double], layers.map((_, Array.empty[Double])))
    
    val numLosses = losses.length
    val numLayers = layers.length
    
    // Pre-allocate all memory upfront
    val cededFlat = new Array[Double](numLosses * numLayers)
    val retained = new Array[Double](numLosses)
    val perLayerSplits = new Array[Array[Double]](numLayers)
    
    // Initialize per-layer result arrays
    for layerIdx <- 0 until numLayers do
      perLayerSplits(layerIdx) = new Array[Double](numLosses)
    
    // Convert IArray to Array for internal processing
    val yearsArr = years.asInstanceOf[Array[Int]]
    val lossesArr = losses.asInstanceOf[Array[Double]]
    
    // Per-layer state tracking for group operations
    val layerCumSum = new Array[Double](numLayers)  // Current cumulative sum per layer
    val layerPrevCumSum = new Array[Double](numLayers)  // Previous cumsum in group for diff
    val layerIsFirstInGroup = new Array[Boolean](numLayers)  // Track first element in group
    
    // Initialize state
    for layerIdx <- 0 until numLayers do
      layerCumSum(layerIdx) = 0.0
      layerPrevCumSum(layerIdx) = 0.0
      layerIsFirstInGroup(layerIdx) = true
    
    var prevYear = if numLosses > 0 then yearsArr(0) else 0
    
    // Single outer loop processing all losses once
    var i = 0
    while i < numLosses do
      val loss = lossesArr(i)
      val year = yearsArr(i)
      
      // Detect group boundary
      val isNewGroup = (year != prevYear)
      if isNewGroup then
        // Reset state for new group
        for layerIdx <- 0 until numLayers do
          layerCumSum(layerIdx) = 0.0
          layerPrevCumSum(layerIdx) = 0.0
          layerIsFirstInGroup(layerIdx) = true
        prevYear = year
      
      var totalCeded = 0.0
      
      // Process all layers for this loss
      for layerIdx <- 0 until numLayers do
        val layer = layers(layerIdx)
        val flatIdx = layerIdx * numLosses + i
        
        // Step 1: Apply occurrence layer
        var ceded = loss
        
        layer.occType match
          case DeductibleType.Retention =>
            ceded = layer.occRetention match
              case Some(ret) => math.max(ceded - ret, 0.0)
              case None => ceded
            ceded = layer.occLimit match
              case Some(lim) => math.min(ceded, lim)
              case None => ceded
              
          case DeductibleType.Franchise =>
            val passesThreshold = layer.occRetention match
              case Some(ret) => loss > ret
              case None => true
            if passesThreshold then
              ceded = layer.occLimit match
                case Some(lim) => math.min(loss, lim)
                case None => loss
            else
              ceded = 0.0
              
          case DeductibleType.ReverseFranchise =>
            val belowThreshold = layer.occRetention match
              case Some(ret) => loss <= ret
              case None => true
            if belowThreshold then
              ceded = layer.occLimit match
                case Some(lim) => math.min(loss, lim)
                case None => loss
            else
              ceded = 0.0
        
        // Step 2: Group cumulative sum (accumulate within group)
        layerCumSum(layerIdx) += ceded
        val cumSumValue = layerCumSum(layerIdx)
        
        // Step 3: Apply aggregate layer
        var aggCeded = cumSumValue
        
        layer.aggType match
          case DeductibleType.Retention =>
            aggCeded = layer.aggRetention match
              case Some(ret) => math.max(aggCeded - ret, 0.0)
              case None => aggCeded
            aggCeded = layer.aggLimit match
              case Some(lim) => math.min(aggCeded, lim)
              case None => aggCeded
              
          case DeductibleType.Franchise =>
            val passesAggThreshold = layer.aggRetention match
              case Some(ret) => cumSumValue > ret
              case None => true
            if passesAggThreshold then
              aggCeded = layer.aggLimit match
                case Some(lim) => math.min(cumSumValue, lim)
                case None => cumSumValue
            else
              aggCeded = 0.0
              
          case DeductibleType.ReverseFranchise =>
            val belowAggThreshold = layer.aggRetention match
              case Some(ret) => cumSumValue <= ret
              case None => true
            if belowAggThreshold then
              aggCeded = layer.aggLimit match
                case Some(lim) => math.min(cumSumValue, lim)
                case None => cumSumValue
            else
              aggCeded = 0.0
        
        // Apply share
        aggCeded = aggCeded * layer.share
        
        // Step 4: Group diff (compute difference from previous cumsum in group)
        val finalCeded = if layerIsFirstInGroup(layerIdx) then
          layerIsFirstInGroup(layerIdx) = false
          aggCeded
        else
          aggCeded - layerPrevCumSum(layerIdx)
        
        layerPrevCumSum(layerIdx) = aggCeded
        
        // Store results
        cededFlat(flatIdx) = finalCeded
        perLayerSplits(layerIdx)(i) = finalCeded
        totalCeded += finalCeded
      end for
      
      // Step 5: Calculate retained
      retained(i) = loss - totalCeded
      
      i += 1
    end while
    
    // Build result with per-layer data
    val splits = layers.zip(perLayerSplits).toSeq
    
    (cededFlat, retained, splits)
  end splitAmnt3
  
  /**
   * High-performance implementation with alternative API that returns flat arrays and per-layer splits.
   * Uses IArray inputs for immutability and returns separate arrays for ceded, retained, and per-layer data.
   * 
   * @param years Immutable array of year identifiers (must be sorted)
   * @param days Immutable array of day identifiers 
   * @param losses Immutable array of loss amounts
   * @return Tuple of (ceded: flat array, retained: array, splits: sequence of (layer, layer-ceded-array))
   */
  def splitAmnt2(years: IArray[Int], days: IArray[Int], losses: IArray[Double]): (ceded: Array[Double], retained: Array[Double], splits: Seq[(Layer, Array[Double])]) =
    if losses.isEmpty then
      return (Array.empty[Double], Array.empty[Double], layers.map((_, Array.empty[Double])))
    
    val numLosses = losses.length
    val numLayers = layers.length
    
    // Pre-allocate all memory upfront for maximum performance
    val cededFlat = new Array[Double](numLosses * numLayers)  // Column-major storage: layer0, layer1, ...
    val retained = new Array[Double](numLosses)
    val perLayerSplits = new Array[Array[Double]](numLayers)
    
    // Convert IArray to Array for internal processing (zero-copy view in Scala 3)
    val yearsArr = years.asInstanceOf[Array[Int]]
    val lossesArr = losses.asInstanceOf[Array[Double]]
    
    // SIMD species for vectorization
    val species = DoubleVector.SPECIES_PREFERRED
    val vectorSize = species.length()
    
    // Step 1: Apply occurrence layers directly to flat storage with SIMD
    applyOccurrenceLayersSIMD2(lossesArr, cededFlat, numLosses, numLayers, species)
    
    // Step 2: In-place group cumulative sum with accumulator
    applyGroupCumSumInPlace2(yearsArr, cededFlat, numLosses, numLayers)
    
    // Step 3: Apply aggregate layers with shares in-place using SIMD
    applyAggregateLayersSIMD2(cededFlat, numLosses, numLayers, species)
    
    // Step 4: In-place group diff with accumulator
    applyGroupDiffInPlace2(yearsArr, cededFlat, numLosses, numLayers)
    
    // Step 5: Calculate retained with SIMD and extract per-layer splits
    calculateRetainedAndSplits2(lossesArr, cededFlat, retained, perLayerSplits, numLosses, numLayers, species)
    
    // Build result with per-layer data
    val splits = layers.zip(perLayerSplits).toSeq
    
    (cededFlat, retained, splits)
  end splitAmnt2
  
  /** Step 1: Apply occurrence layers with SIMD optimization */
  private inline def applyOccurrenceLayersSIMD2(
    losses: Array[Double],
    cededFlat: Array[Double],
    numLosses: Int,
    numLayers: Int,
    species: VectorSpecies[java.lang.Double]
  ): Unit =
    // Process each layer in column-major order
    for layerIdx <- 0 until numLayers do
      val layer = layers(layerIdx)
      val offset = layerIdx * numLosses
      
      // Copy losses to this layer's column
      System.arraycopy(losses, 0, cededFlat, offset, numLosses)
      
      // Apply occurrence terms based on type
      layer.occType match
        case DeductibleType.Retention =>
          applyRetentionSIMD(cededFlat, offset, numLosses, layer.occLimit, layer.occRetention, species)
        case DeductibleType.Franchise =>
          applyFranchiseSIMD(cededFlat, offset, numLosses, layer.occLimit, layer.occRetention, species)
        case DeductibleType.ReverseFranchise =>
          applyReverseFranchise(cededFlat, offset, numLosses, layer.occLimit, layer.occRetention)
    end for
  end applyOccurrenceLayersSIMD2
  
  /** Step 2: In-place group cumulative sum - sequential with accumulator */
  private inline def applyGroupCumSumInPlace2(
    years: Array[Int],
    cededFlat: Array[Double],
    numLosses: Int,
    numLayers: Int
  ): Unit =
    // Process each layer column
    for layerIdx <- 0 until numLayers do
      val offset = layerIdx * numLosses
      var i = 0
      
      while i < numLosses do
        val g = years(i)
        var cumSum = 0.0  // Accumulator for current group
        
        // Process all elements in this group
        while i < numLosses && years(i) == g do
          cumSum += cededFlat(offset + i)
          cededFlat(offset + i) = cumSum
          i += 1
        end while
      end while
    end for
  end applyGroupCumSumInPlace2
  
  /** Step 3: Apply aggregate layers with SIMD optimization */
  private inline def applyAggregateLayersSIMD2(
    cededFlat: Array[Double],
    numLosses: Int,
    numLayers: Int,
    species: VectorSpecies[java.lang.Double]
  ): Unit =
    for layerIdx <- 0 until numLayers do
      val layer = layers(layerIdx)
      val offset = layerIdx * numLosses
      
      // Apply aggregate terms
      layer.aggType match
        case DeductibleType.Retention =>
          applyRetentionSIMD(cededFlat, offset, numLosses, layer.aggLimit, layer.aggRetention, species)
          if layer.share != 1.0 then
            applyShareSIMD(cededFlat, offset, numLosses, layer.share, species)
        case DeductibleType.Franchise =>
          applyFranchiseSIMD(cededFlat, offset, numLosses, layer.aggLimit, layer.aggRetention, species)
          if layer.share != 1.0 then
            applyShareSIMD(cededFlat, offset, numLosses, layer.share, species)
        case DeductibleType.ReverseFranchise =>
          applyReverseFranchise(cededFlat, offset, numLosses, layer.aggLimit, layer.aggRetention)
          if layer.share != 1.0 then
            applyShareSIMD(cededFlat, offset, numLosses, layer.share, species)
    end for
  end applyAggregateLayersSIMD2
  
  /** Step 4: In-place group diff - sequential with accumulator */
  private inline def applyGroupDiffInPlace2(
    years: Array[Int],
    cededFlat: Array[Double],
    numLosses: Int,
    numLayers: Int
  ): Unit =
    // Process each layer column
    for layerIdx <- 0 until numLayers do
      val offset = layerIdx * numLosses
      var i = 0
      
      while i < numLosses do
        val g = years(i)
        var prevValue = 0.0  // Accumulator for previous value in group
        var isFirstInGroup = true
        
        // Process all elements in this group
        while i < numLosses && years(i) == g do
          val currentValue = cededFlat(offset + i)
          if isFirstInGroup then
            // First element keeps its value
            isFirstInGroup = false
          else
            cededFlat(offset + i) = currentValue - prevValue
          end if
          prevValue = currentValue
          i += 1
        end while
      end while
    end for
  end applyGroupDiffInPlace2
  
  /** Step 5: Calculate retained and extract per-layer splits with SIMD */
  private inline def calculateRetainedAndSplits2(
    originalLosses: Array[Double],
    cededFlat: Array[Double],
    retained: Array[Double],
    perLayerSplits: Array[Array[Double]],
    numLosses: Int,
    numLayers: Int,
    species: VectorSpecies[java.lang.Double]
  ): Unit =
    val vectorSize = species.length()
    val loopBound = species.loopBound(numLosses)
    
    // First, copy per-layer data from flat array
    for layerIdx <- 0 until numLayers do
      val offset = layerIdx * numLosses
      val layerData = new Array[Double](numLosses)
      System.arraycopy(cededFlat, offset, layerData, 0, numLosses)
      perLayerSplits(layerIdx) = layerData
    end for
    
    // Calculate retained with SIMD: retained = original - sum(all layers)
    var i = 0
    while i < loopBound do
      var sumVector = DoubleVector.zero(species)
      
      // Vectorized sum across all layers
      for layerIdx <- 0 until numLayers do
        val offset = layerIdx * numLosses
        val layerVector = DoubleVector.fromArray(species, cededFlat, offset + i)
        sumVector = sumVector.add(layerVector)
      end for
      
      // Calculate retained = original - sum(ceded)
      val originalVector = DoubleVector.fromArray(species, originalLosses, i)
      val retainedVector = originalVector.sub(sumVector)
      retainedVector.intoArray(retained, i)
      
      i += vectorSize
    end while
    
    // Handle remaining elements (scalar tail)
    while i < numLosses do
      var sum = 0.0
      for layerIdx <- 0 until numLayers do
        val offset = layerIdx * numLosses
        sum += cededFlat(offset + i)
      end for
      retained(i) = originalLosses(i) - sum
      i += 1
    end while
  end calculateRetainedAndSplits2
  
end Tower
