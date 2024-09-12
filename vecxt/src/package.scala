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
    def apply[T <: Tuple2[Int, Int]](a: NArray[Double], b: T)(using ev: TupleOfInts[T] =:= true): Matrix = (a, b)

    // extension (t: Tensor)
    //     @targetName("martixRaw")
    //     def raw: Array[Double] = t.raw
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

  extension (m: Matrix)
    inline def rows: Int = m._2._1

    inline def cols: Int = m._2._2

    inline def row(i: Int): NArray[Double] =
      val start = i * m.cols
      val end = (i + 1) * m.cols
      val result = new NArray[Double](m.cols)
      var j = start
      var k = 0
      while j < end do
        result(k) = m._1(j)
        j += 1
        k += 1
      end while
      result
    end row

    inline def print: String =
      val arrArr = for i <- 0 until m.rows yield m.row(i).mkString(" ")
      arrArr.mkString("\n")
    end print

    inline def col(i: Int): NArray[Double] = NArray.tabulate(m.rows)(j => m._1(j * m.cols + i))

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
      Matrix(newArr, (m.cols, m.rows))
    end transpose
  end extension

end Tensors
