package vecxt.rpt

import narr.NArray
import narr.native.Extensions.sort


extension [N <: Int](thisVector: NArray[Double])

    def qdep(alpha: Double, thatVector: NArray[Double]): Double =
      val numYears = thisVector.length
      val nte = numYears * (1.0 - alpha);

      val fte = Math.floor(nte).toInt;

      val order = Ordering.Double.TotalOrdering.reverse

      val (_, originalPositionThis) = thisVector.asInstanceOf[NArray[Double]].toVector.zipWithIndex.sortBy(_._1)(using order).unzip
      val (_, originalPositionThat) = thatVector.asInstanceOf[NArray[Double]].toVector.zipWithIndex.sortBy(_._1)(using order).unzip

      val tailIdxThis = originalPositionThis.slice(0, fte).toSet
      val tailIdxThat = originalPositionThat.slice(0, fte).toSet

      val sharedTail = tailIdxThis.intersect(tailIdxThat).size
      sharedTail.toDouble / nte
    end qdep

    def tVar(alpha: Double): Double =
      val numYears = thisVector.length
      val nte = numYears * (1.0 - alpha);
      val fte = Math.floor(nte).toInt;
      val sorted = thisVector.sort()
      var i = 0
      var tailSum = 0.0;
      while i < fte do
        tailSum += sorted(i);
        i += 1;
      end while
      tailSum / fte.toDouble;
    end tVar

    def tVarIdx(alpha: Double): NArray[Boolean] =
      val numYears = thisVector.length
      val nte = numYears * (1.0 - alpha);
      val fte = Math.floor(nte).toInt;
      val sorted = thisVector.zipWithIndex.sortBy(_._1).map(_._2)
      val idx = NArray.fill[Boolean](numYears)(false)
      var i = 0
      while i < fte do
        idx(sorted(i)) = true;        
        i = i + 1
      end while
      idx
      
    end tVarIdx