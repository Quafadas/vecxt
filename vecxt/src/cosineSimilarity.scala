package vecxt

import vecxt.array.norm
import vecxt.array.dot
import narr.NArray
import vecxt.BoundsCheck.BoundsCheck

/** Compute the cosine similarity between two vectors
  *
  * @param v1
  *   the first vector
  * @param v2
  *   the second vector
  */
object cosineSimilarity:

  inline def apply(v1: NArray[Double], v2: NArray[Double])(using inline boundsCheck: BoundsCheck): Double =
    dimCheck(v1, v2)
    v1.dot(v2) / (v1.norm * v2.norm)
  end apply

end cosineSimilarity
