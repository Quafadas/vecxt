package vecxt.reinsurance

import narr.*
// import narr.native.Extensions.sort

extension [N <: Int](thisVector: NArray[Double])

  def qdep(alpha: Double, thatVector: NArray[Double]): Double =
    val numYears = thisVector.length
    val nte = numYears * (1.0 - alpha);

    val fte = Math.floor(nte).toInt;

    val order = Ordering.Double.TotalOrdering.reverse

    val (_, originalPositionThis) =
      thisVector.asInstanceOf[NArray[Double]].toVector.zipWithIndex.sortBy(_._1)(using order).unzip
    val (_, originalPositionThat) =
      thatVector.asInstanceOf[NArray[Double]].toVector.zipWithIndex.sortBy(_._1)(using order).unzip

    val tailIdxThis = originalPositionThis.slice(0, fte).toSet
    val tailIdxThat = originalPositionThat.slice(0, fte).toSet

    val sharedTail = tailIdxThis.intersect(tailIdxThat).size
    sharedTail.toDouble / nte
  end qdep

  def tVar(alpha: Double): Double =
    val numYears = thisVector.length
    val nte = numYears * (1.0 - alpha);
    val fte = Math.floor(nte + 0.00000000001).toInt; // numberic precision is a pain.
    val sorted = thisVector.sort()
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
    val sorted = thisVector.sort()
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
    val sorted = thisVector.sort()

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
  def tVarWithVaRBatch(alphas: NArray[Double]): NArray[(cl: Double, VaR: Double, TVaR: Double)] =
    val sorted = thisVector.sort()
    val numYears = thisVector.length
    val result = NArray.ofSize[(cl: Double, VaR: Double, TVaR: Double)](alphas.length)

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

  def tVarIdx(alpha: Double): NArray[Boolean] =
    val numYears = thisVector.length
    val nte = numYears * (1.0 - alpha);
    val fte = Math.floor(nte + 0.00000000001).toInt;
    val sorted = thisVector.toVector.zipWithIndex.sortBy(_._1).map(_._2)
    val idx = NArray.fill[Boolean](numYears)(false)
    var i = 0
    while i < fte do
      idx(sorted(i)) = true;
      i = i + 1
    end while
    idx

  end tVarIdx
end extension
