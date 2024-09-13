package vecxt

import narr.*

object Tensors:

  type TupleOfInts[T <: Tuple] <: Boolean = T match
    case EmptyTuple  => true // Base case: Empty tuple is valid
    case Int *: tail => TupleOfInts[tail] // Recursive case: Head is Int, check the tail
    case _           => false // If any element is not an Int, return false

  opaque type Tensor = (NArray[Double], Tuple)
  object Tensor:
    def apply[T <: Tuple](a: NArray[Double], b: T)(using ev: TupleOfInts[T] =:= true): Tensor = (a, b)
  end Tensor

  opaque type Vector1 = (NArray[Double], Tuple1[Int])
  type Vector = Vector1 & Tensor

  object Vector:
    def apply(a: NArray[Double]): Vector = (a, Tuple1(a.size))
  end Vector

  opaque type Matrix1 = (NArray[Double], Tuple2[Int, Int])

  type Matrix = Matrix1 & Tensor

  object Matrix:
    inline def apply[T <: Tuple2[Int, Int]](a: NArray[Double], b: T)(using inline boundsCheck: BoundsCheck)(using
        ev: TupleOfInts[T] =:= true
    ): Matrix =
      (a, b)
    inline def apply[T <: Tuple2[Int, Int]](b: T, a: NArray[Double])(using inline boundsCheck: BoundsCheck)(using
        ev: TupleOfInts[T] =:= true
    ): Matrix =
      (a, b)

    inline def fromRows(a: NArray[NArray[Double]])(using inline boundsCheck: BoundsCheck): Matrix =
      val rows = a.size
      val cols = a(0).size

      assert(a.forall(_.size == cols))

      val newArr = NArray.ofSize[Double](rows * cols)
      var idx = 0
      var i = 0
      while i < cols do
        var j = 0
        while j < rows do
          newArr(idx) = a(j)(i)
          // println(s"row: $i || col: $j || ${a(j)(i)} entered at index ${idx}")
          j += 1
          idx += 1
        end while
        i += 1
      end while
      Matrix(newArr, (rows, cols))
    end fromRows

    inline def fromColumns(a: NArray[NArray[Double]])(using inline boundsCheck: BoundsCheck): Matrix =
      val cols = a.size
      val rows = a(0).size
      assert(a.forall(_.size == rows))
      val newArr = NArray.ofSize[Double](rows * cols)
      var idx = 0
      var i = 0
      while i < cols do
        var j = 0
        while j < rows do
          // val idx = i * cols + j
          newArr(idx) = a(i)(j)
          // println(s"i: $i || j: $j || ${a(i)(j)} entered at index ${idx}")
          j += 1
          idx += 1
        end while
        i += 1
      end while
      Matrix(newArr, (rows, cols))
    end fromColumns

  end Matrix

  opaque type StrictMatrix1[M <: Int, N <: Int] = (NArray[Double], Tuple2[M, N]) & Matrix

  type StrictMatrix[M <: Int, N <: Int] = StrictMatrix1[M, N] & Tensor
  object StrictMatrix:
    def apply[M <: Int, N <: Int](
        a: NArray[Double]
    )(using ev: TupleOfInts[Tuple2[M, N]] =:= true, m: ValueOf[M], n: ValueOf[N]): StrictMatrix[M, N] =
      val tup = (m.value, n.value)
      (a, tup)
    end apply
  end StrictMatrix

  extension (t: Tensor)
    def raw: NArray[Double] = t._1

    /** Zero indexed element retrieval
      */
    def elementAt[T <: Tuple](b: T)(using ev: TupleOfInts[T] =:= true) =
      val indexes = b.toList.asInstanceOf[List[Int]]
      val dimensions = t._2.toList.asInstanceOf[List[Int]]

      assert(indexes.length == dimensions.length)

      val linearIndex = indexes
        .zip(dimensions.scanRight(1)(_ * _).tail)
        .map { case (index, stride) =>
          index * stride
        }
        .sum

      t._1(linearIndex)
    end elementAt
  end extension

  extension (d: Array[Double]) def arrPrint: String = d.mkString("[", ",", "],")

  extension (m: Matrix)
    inline def :@(b: Matrix)(using inline boundsCheck: BoundsCheck): Matrix = m.matmul(b)

    inline def scale(d: Double): Unit = m._1 *= d

    inline def rows: Int = m._2._1

    inline def cols: Int = m._2._2

    inline def row(i: Int): NArray[Double] =
      val result = new NArray[Double](m.cols)
      val cols = m.cols
      var j = 0
      var k = 0
      while j < m.cols do
        result(k) = m._1(i + j * m.rows)
        j += 1
        k += 1
      end while
      result
    end row

    inline def print: String =
      val arrArr = for i <- 0 until m.rows yield m.row(i).mkString(" ")
      arrArr.mkString("\n")
    end print

    inline def col(i: Int): NArray[Double] =
      val result = new NArray[Double](m.rows)
      val cols = m.cols
      var j = 0
      var k = 0
      while j < m.rows do
        result(k) = m._1(i * m.cols + j)
        j += 1
        k += 1
      end while
      result
    end col

    inline def transpose: Matrix =
      val newArr = NArray.ofSize[Double](m._1.length)
      var i = 0
      while i < m.cols do
        var j = 0
        while j < m.rows do
          newArr(i * m.rows + j) = m._1(j * m.cols + i)
          j += 1
        end while
        i += 1
      end while
      Matrix(newArr, (m.cols, m.rows))(using
        BoundsCheck.DoBoundsCheck.no
      ) // we already have a valid matrix if we are transposing it, so this check is redundant if this method works as intended.
    end transpose
  end extension

end Tensors
