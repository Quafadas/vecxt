// package vecxt_re

// import vecxt.all.*

// extension (scenario: Scenario)
//   def imposeClustering(newCoeff: Double): Scenario = {
//     // expectation and variance of new scenario
//     val numItrs = scenario.numberIterations
//     val frequency = scenario.freq
//     val e         = frequency.mean
//     val v         = newCoeff * Math.pow(e, 2) + e

//     // in (r,p) form
//     val p = e / v
//     val r = e * p / (1 - p)

//     val newDist: DiscreteDistr[Int] with Product = if (newCoeff > 0) {
//       breeze.stats.distributions.NegativeBinomial(r, 1 - p) // different parameterisation to matlab
//     } else {
//       breeze.stats.distributions.Poisson(e)
//     }

//     var newFreq: IndexedSeq[Int] = newDist.sample(numberIterations)
//     val maxSteps                 = 10
//     val sumEvents                = scenario.events.length
//     var step                     = 0

//     def matchMean(
//         inFreq: IndexedSeq[Int],
//         sumEvents: Int,
//         newCoeff: Double
//     ): IndexedSeq[Int] = {
//       val delta                      = sumEvents - inFreq.sum;
//       val anz                        = math.min(numItrs, Math.abs(delta))
//       val asVector: Array[Int] = Array(inFreq: _*) // for slicing...
//       val asVectorDouble             = convert(asVector, Double)

//       delta match {
//         case n if (n < 0) => {

//           val d                         = breeze.numerics.abs(asVectorDouble - Math.max(Math.ceil(mean(asVectorDouble)), 1))
//           val temp: Matrix[Double] = Matrix(d.toArray.toScalaVector.zipWithIndex.map { case (x, y) => (x, y.toDouble) }: _*)
//           val sorted                    = sortrows(temp, Vector(0))
//           sorted(::, 0)
//           val idx         = convert(sorted(::, 1), Int)
//           val changeThese = idx(0 until anz)
//           asVector(changeThese.toScalaVector) -= 1
//           val check = (asVector <:< 0).activeKeysIterator.toVector
//           asVector(check) += 1
//           asVector.toScalaVector
//         }
//         case n if (n > 0) => {
//           val d                         = breeze.numerics.abs(asVectorDouble - Math.floor(mean(asVectorDouble)))
//           val temp: Matrix[Double] = Matrix(d.toArray.toScalaVector.zipWithIndex.map { case (x, y) => (x, y.toDouble) }: _*)
//           val sorted                    = sortrows(temp, Vector(0))
//           sorted(::, 0)
//           val idx         = convert(sorted(::, 1), Int)
//           val changeThese = idx(0 until anz)
//           asVector(changeThese.toScalaVector) += 1
//           asVector.toScalaVector
//         }
//       }
//     }
//     while (newFreq.sum != sumEvents && step <= maxSteps) {
//       newFreq = matchMean(newFreq, sumEvents, newCoeff)
//       step = step + 1
//     }
//     val frequencyC = convert(Array(newFreq: _*), Double)
//     val meanFreqC  = mean(frequencyC)
//     (variance(frequencyC) - meanFreqC) / Math.pow(meanFreqC, 2)

//     if (step == maxSteps) {
//       throw new Exception("Max steps reached, this probably didn't work")
//     }
//     val builder = Vector.newBuilder[Int]
//     for ((numEvents, itr) <- newFreq.zipWithIndex) {
//       // decumcount
//       val etend = for (_ <- 1 to numEvents if numEvents > 0) yield (itr + 1)
//       builder ++= etend
//     }
//     val decumcount = builder.result()

//     val zipTogether = decumcount.zip(events)
//     val permute     = zipTogether.map { case (itr, event) => event.copy(iteration = itr) }

//     scenario.copy(events = permute)

//   }
