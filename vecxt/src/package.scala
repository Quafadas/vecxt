package vecxt

import narr.*
import jdk.incubator.vector.DoubleVector

object Matrix:

  import vecxt.extensions.*

  type TupleOfInts[T <: Tuple] <: Boolean = T match
    case EmptyTuple  => true // Base case: Empty tuple is valid
    case Int *: tail => TupleOfInts[tail] // Recursive case: Head is Int, check the tail
    case _           => false // If any element is not an Int, return false

  // opaque type Tensor = (NArray[Double], Tuple)
  // object Tensor:
  //   def apply[T <: Tuple](a: NArray[Double], b: T)(using ev: TupleOfInts[T] =:= true): Tensor = (a, b)
  // end Tensor

  // opaque type Vector = (NArray[Double], Tuple1[Int])
  // type Vector = Vector1 & Tensor

  // object Vector:
  //   def apply(a: NArray[Double]): Vector = (a, Tuple1(a.size))
  // end Vector

  /** This is a matrix
    *
    * ._1 is the matrix values, stored as a single contiguous array ._2 is the dimensions ._2._1 is the number of rows
    * ._2._2 is the number of columns
    */
  opaque type Matrix = (NArray[Double], Tuple2[Int, Int])

  type RangeExtender = Range | Int | NArray[Int] | ::.type

  // type Matrix = Matrix1 & Tensor

  object Matrix:

    inline def doubleSpecies = DoubleVector.SPECIES_PREFERRED

    inline def apply[T <: Tuple2[Int, Int]](raw: NArray[Double], dim: T)(using inline boundsCheck: BoundsCheck)(using
        ev: TupleOfInts[T] =:= true
    ): Matrix =
      dimMatInstantiateCheck(raw, dim)
      (raw, dim)
    end apply
    inline def apply[T <: Tuple2[Int, Int]](dim: T, raw: NArray[Double])(using inline boundsCheck: BoundsCheck)(using
        ev: TupleOfInts[T] =:= true
    ): Matrix =
      dimMatInstantiateCheck(raw, dim)
      (raw, dim)
    end apply

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

    inline def ones(dim: Tuple2[Int, Int]): Matrix =
      val (rows, cols) = dim
      val newArr = NArray.fill[Double](rows * cols)(1.0)
      Matrix(newArr, dim)(using BoundsCheck.DoBoundsCheck.no)
    end ones

    inline def zeros(dim: Tuple2[Int, Int]): Matrix =
      val (rows, cols) = dim
      val newArr = NArray.ofSize[Double](rows * cols)
      Matrix(newArr, dim)(using BoundsCheck.DoBoundsCheck.no)
    end zeros

    inline def eye(dim: Int): Matrix =
      val newArr = NArray.ofSize[Double](dim * dim)
      var i = 0
      while i < dim do
        newArr(i * dim + i) = 1.0
        i += 1
      end while
      Matrix(newArr, (dim, dim))(using BoundsCheck.DoBoundsCheck.no)
    end eye

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

  // opaque type StrictMatrix1[M <: Int, N <: Int] = (NArray[Double], Tuple2[M, N]) & Matrix

  // type StrictMatrix[M <: Int, N <: Int] = StrictMatrix1[M, N]
  // object StrictMatrix:
  //   def apply[M <: Int, N <: Int](
  //       a: NArray[Double]
  //   )(using ev: TupleOfInts[Tuple2[M, N]] =:= true, m: ValueOf[M], n: ValueOf[N]): StrictMatrix[M, N] =
  //     val tup = (m.value, n.value)
  //     (a, tup)
  //   end apply
  // end StrictMatrix

  extension [A](d: Array[A]) def print: String = d.mkString("[", ",", "],")

  extension (m: Matrix)
    inline def update(loc: Tuple2[Int, Int], value: Double)(using inline boundsCheck: BoundsCheck) =
      indexCheckMat(m, loc)
      val idx = loc._1 * m._2._2 + loc._2
      m._1(idx) = value
    end update

    private inline def range(r: RangeExtender, max: Int): NArray[Int] = r match
      case _: ::.type     => NArray.from((0 until max).toArray)
      case r: Range       => NArray.from(r.toArray)
      case l: NArray[Int] => l
      case i: Int         => NArray(i)

    def apply(rowRange: RangeExtender, colRange: RangeExtender): Matrix =
      val newRows = range(rowRange, m.rows)
      val newCols = range(colRange, m.cols)
      val newArr = NArray.ofSize[Double](newCols.size * newRows.size)

      var idx = 0

      var i = 0
      while i < newCols.length do
        val oldCol = newCols(i)
        val stride = oldCol * m.cols
        var j = 0
        while j < newRows.length do
          val oldRow = newRows(j)
          newArr(idx) = m._1(stride + oldRow)
          idx += 1
          j += 1
        end while
        i += 1
      end while

      Matrix(newArr, (newRows.size, newCols.size))(using BoundsCheck.DoBoundsCheck.no)

    end apply

    inline def raw: NArray[Double] = m._1

    /** Zero indexed element retrieval
      */
    inline def apply(b: Tuple2[Int, Int]) =
      val indexes = b.toList.asInstanceOf[List[Int]]
      val dimensions = m._2.toList.asInstanceOf[List[Int]]

      assert(indexes.length == dimensions.length)

      val linearIndex = indexes
        .zip(dimensions.scanRight(1)(_ * _).tail)
        .map { case (index, stride) =>
          index * stride
        }
        .sum

      m._1(linearIndex)
    end apply

    inline def @@(b: Matrix)(using inline boundsCheck: BoundsCheck): Matrix = m.matmul(b)

    inline def *=(d: Double): Unit = m._1.multInPlace(d)

    inline def +(m2: Matrix)(using inline boundsCheck: BoundsCheck): Matrix =
      sameDimMatCheck(m, m2)
      val newArr = m._1.add(m2._1)
      Matrix(newArr, m._2)(using BoundsCheck.DoBoundsCheck.no)
    end +

    inline def rows: Int = m._2._1

    inline def cols: Int = m._2._2

    inline def shape: String = s"${m.rows} x ${m.cols}"

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

end Matrix
