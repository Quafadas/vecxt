package vecxt.reinsurance


extension (thisVector: Array[Double])

  /** Calculate tail dependence between two distributions. Tail dependence measures the proportion of observations that
    * appear in both tails at a given confidence level. This is useful for understanding the co-movement of extreme
    * values in two related datasets.
    *
    * @param alpha
    *   Confidence level (e.g., 0.95 for 95% confidence)
    * @param thatVector
    *   The second array to compare against
    * @return
    *   The proportion of shared tail observations (0.0 to 1.0)
    */
  def qdep(alpha: Double, thatVector: Array[Double]): Double =
    val numYears = thisVector.length
    val nte = numYears * (1.0 - alpha);

    val fte = Math.floor(nte).toInt;

    val order = Ordering.Double.TotalOrdering.reverse

    val (_, originalPositionThis) =
      thisVector.toVector.zipWithIndex.sortBy(_._1)(using order).unzip
    val (_, originalPositionThat) =
      thatVector.toVector.zipWithIndex.sortBy(_._1)(using order).unzip

    val tailIdxThis = originalPositionThis.slice(0, fte).toSet
    val tailIdxThat = originalPositionThat.slice(0, fte).toSet

    val sharedTail = tailIdxThis.intersect(tailIdxThat).size
    sharedTail.toDouble / nte
  end qdep

  /** Calculate Tail Value at Risk (TVaR), also known as Conditional Value at Risk (CVaR) or Expected Shortfall. TVaR is
    * the expected value of losses in the tail beyond the VaR threshold. It represents the average of all values in the
    * worst (1-alpha) portion of the distribution.
    *
    * @param alpha
    *   Confidence level (e.g., 0.95 for 95% confidence)
    * @return
    *   The average value in the tail (lower values)
    */
  def tVar(alpha: Double): Double =
    val numYears = thisVector.length
    val nte = numYears * (1.0 - alpha);
    val fte = Math.floor(nte + 0.00000000001).toInt; // numberic precision is a pain.
    val sorted = thisVector.sorted
    var i = 0
    var tailSum = 0.0;
    while i < fte do
      tailSum += sorted(i);
      i += 1;
    end while
    tailSum / fte.toDouble;
  end tVar

  /** Calculate Value at Risk (VaR) at the given confidence level alpha. VaR is the threshold value at the (1-alpha)
    * quantile of the distribution.
    *
    * @param alpha
    *   Confidence level (e.g., 0.95 for 95% confidence)
    * @return
    *   The VaR value at the specified confidence level
    */
  def VaR(alpha: Double): Double =
    val numYears = thisVector.length
    val nte = numYears * (1.0 - alpha);
    val fte = Math.floor(nte + 0.00000000001).toInt;
    val sorted = thisVector.sorted
    if fte > 0 then sorted(fte - 1) else sorted(0)
    end if
  end VaR

  /** Calculate both TVaR and VaR at the given confidence level alpha.
    *
    * @param alpha
    *   Confidence level (e.g., 0.95 for 95% confidence)
    * @return
    *   A named tuple with (cl, VaR, TVaR) where cl = 1-alpha
    */
  def tVarWithVaR(alpha: Double): (cl: Double, VaR: Double, TVaR: Double) =
    val numYears = thisVector.length
    val nte = numYears * (1.0 - alpha);
    val fte = Math.floor(nte + 0.00000000001).toInt;
    val sorted = thisVector.sorted

    var i = 0
    var tailSum = 0.0;
    while i < fte do
      tailSum += sorted(i);
      i += 1;
    end while
    val tvar = tailSum / fte.toDouble
    val var_value = if fte > 0 then sorted(fte - 1) else sorted(0)
    val confidence_level = 1.0 - alpha

    (cl = confidence_level, VaR = var_value, TVaR = tvar)
  end tVarWithVaR

  /** Calculate multiple (TVaR, VaR) pairs for different confidence levels. This is more efficient than calling
    * tVarWithVaR multiple times as it only sorts once.
    *
    * @param alphas
    *   Array of confidence levels
    * @return
    *   Array of named tuples containing (cl, VaR, TVaR) for each confidence level where cl = 1-alpha
    */
  def tVarWithVaRBatch(alphas: Array[Double]): Array[(cl: Double, VaR: Double, TVaR: Double)] =
    val sorted = thisVector.sorted
    val numYears = thisVector.length
    val result = Array.ofDim[(cl: Double, VaR: Double, TVaR: Double)](alphas.length)

    var alphaIdx = 0
    while alphaIdx < alphas.length do
      val alpha = alphas(alphaIdx)
      val nte = numYears * (1.0 - alpha)
      val fte = Math.floor(nte + 0.00000000001).toInt

      var i = 0
      var tailSum = 0.0
      while i < fte do
        tailSum += sorted(i)
        i += 1
      end while

      val tvar = tailSum / fte.toDouble
      val var_value = if fte > 0 then sorted(fte - 1) else sorted(0)
      val confidence_level = 1.0 - alpha

      result(alphaIdx) = (cl = confidence_level, VaR = var_value, TVaR = tvar)
      alphaIdx += 1
    end while

    result
  end tVarWithVaRBatch

  /** Return a boolean mask indicating which elements are in the tail (used in TVaR calculation). This is useful for
    * identifying which observations contribute to the tail risk.
    *
    * @param alpha
    *   Confidence level (e.g., 0.95 for 95% confidence)
    * @return
    *   A boolean array where true indicates the element is in the tail
    */
  def tVarIdx(alpha: Double): Array[Boolean] =
    val numYears = thisVector.length
    val nte = numYears * (1.0 - alpha);
    val fte = Math.floor(nte + 0.00000000001).toInt;
    val sorted = thisVector.toVector.zipWithIndex.sortBy(_._1).map(_._2)
    val idx = Array.fill[Boolean](numYears)(false)
    var i = 0
    while i < fte do
      idx(sorted(i)) = true;
      i = i + 1
    end while
    idx

  end tVarIdx
end extension
