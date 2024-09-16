package vecxt

import narr.*
import javax.print.attribute.standard.MediaSize.NA
import scala.reflect.ClassTag
import scala.compiletime.erasedValue

import vecxt.extensions.*

object MatrixStuff:

  // type TupleOfInts[T <: Tuple] <: Boolean = T match
  //   case EmptyTuple  => true // Base case: Empty tuple is valid
  //   case Int *: tail => TupleOfInts[tail] // Recursive case: Head is Int, check the tail
  //   case _           => false // If any element is not an Int, return false

  // opaque type Tensor = (NArray[Double], Tuple)
  // object Tensor:
  //   def apply[T <: Tuple](a: NArray[Double], b: T)(using ev: TupleOfInts[T] =:= true): Tensor = (a, b)
  // end Tensor

  // opaque type Vector = (NArray[Double], Tuple1[Int])
  // type Vector = Vector1 & Tensor

  // object Vector:
  //   def apply(a: NArray[Double]): Vector = (a, Tuple1(a.size))
  // end Vector

  type RangeExtender = Range | Int | NArray[Int] | ::.type

  // type MatTypCheck[A <: MatTyp] = A match
  //   case Boolean => true
  //   case Double  => true
  //   case _       => false

  // type MatTyp = Double | Boolean

  transparent inline def zero[A] =
    inline erasedValue[A] match
      case _: Double  => 0.0.asInstanceOf[A]
      case _: Boolean => false.asInstanceOf[A]
      case _: A       => ???
    end match
  end zero

  transparent inline def one[A] =
    inline erasedValue[A] match
      case _: Double  => 1.0.asInstanceOf[A]
      case _: Boolean => true.asInstanceOf[A]
      case _: A       => ???
    end match
  end one

  // Sparta : Double, Int, Float, Long, Boolean
  transparent inline def supportedType[A] =
    inline erasedValue[A] match
      case _: Double  => true
      case _: Boolean => true
      case _: A       => false
    end match
  end supportedType

  opaque type Matrix[A] = (NArray[A], Tuple2[Int, Int])

  // type Matrix = Matrix1 & Tensor

  object Matrix:
    inline def apply[T <: Tuple2[Int, Int], @specialized(Double, Boolean) A](raw: NArray[A], dim: T)(using
        inline boundsCheck: BoundsCheck
    ): Matrix[A] =
      dimMatInstantiateCheck(raw, dim)
      (raw, dim)
    end apply

    inline def apply[T <: Tuple2[Int, Int], @specialized(Double, Boolean) A](dim: T, raw: NArray[A])(using
        inline boundsCheck: BoundsCheck
    ): Matrix[A] =
      dimMatInstantiateCheck(raw, dim)
      (raw, dim)
    end apply

    inline def fromRows[@specialized(Double, Boolean) A](a: NArray[NArray[A]])(using
        inline boundsCheck: BoundsCheck,
        classTag: ClassTag[A]
    ): Matrix[A] =
      val rows = a.size
      val cols = a(0).size

      assert(a.forall(_.size == cols))

      val newArr = NArray.ofSize[A](rows * cols)
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

    inline def ones[@specialized(Double, Boolean) A](dim: Tuple2[Int, Int])(using
        classTag: ClassTag[A]
    ): Matrix[A] =
      val (rows, cols) = dim
      val newArr = NArray.fill[A](rows * cols)(one[A])
      Matrix(newArr, dim)(using BoundsCheck.DoBoundsCheck.no)
    end ones

    inline def zeros[@specialized(Double, Boolean) A](dim: Tuple2[Int, Int])(using
        classTag: ClassTag[A]
    ): Matrix[A] =
      val (rows, cols) = dim
      val newArr = NArray.fill[A](rows * cols)(zero[A])
      Matrix(newArr, dim)(using BoundsCheck.DoBoundsCheck.no)
    end zeros

    inline def eye[@specialized(Double, Boolean) A](dim: Int)(using classTag: ClassTag[A]): Matrix[A] =
      val newArr = NArray.fill[A](dim * dim)(zero[A])
      val oneVal = one[A]
      var i = 0
      while i < dim do
        newArr(i * dim + i) = oneVal
        i += 1
      end while
      Matrix(newArr, (dim, dim))(using BoundsCheck.DoBoundsCheck.no)
    end eye

    inline def fromColumns[@specialized(Double, Boolean) A](a: NArray[NArray[A]])(using
        inline boundsCheck: BoundsCheck,
        ct: ClassTag[A]
    ): Matrix[A] =
      val cols = a.size
      val rows = a(0).size
      assert(a.forall(_.size == rows))
      val newArr = NArray.ofSize[A](rows * cols)
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

    // end Matrix[MatTyp]
  end Matrix

  // opaque type StrictMatrix1[M <: Int, N <: Int] = (NArray[Double], Tuple2[M, N]) & Matrix[MatTyp]

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

  extension [A](m: Matrix[A])

    inline def rows: Int = m._2._1

    inline def cols: Int = m._2._2

    inline def raw: NArray[A] = m._1

    inline def shape: String = s"${m.rows} x ${m.cols}"
  end extension

  extension (m: Matrix[Double])

    inline def +(m2: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] =
      sameDimMatCheck(m, m2)
      val newArr = m._1.add(m2._1)
      Matrix[(Int, Int), Double](newArr, m._2)(using BoundsCheck.DoBoundsCheck.no)
    end +

    inline def *=(d: Double): Unit = m._1.multInPlace(d)

    inline def @@(b: Matrix[Double])(using inline boundsCheck: BoundsCheck): Matrix[Double] = m.matmul(b)

    inline def >=(d: Double): Matrix[Boolean] =
      Matrix[(Int, Int), Boolean](m.raw.gte(d), m._2)(using BoundsCheck.DoBoundsCheck.no)

    inline def >(d: Double): Matrix[Boolean] =
      Matrix[(Int, Int), Boolean](m.raw.gt(d), m._2)(using BoundsCheck.DoBoundsCheck.no)

    inline def <=(d: Double): Matrix[Boolean] =
      Matrix[(Int, Int), Boolean](m.raw.lte(d), m._2)(using BoundsCheck.DoBoundsCheck.no)

    inline def <(d: Double): Matrix[Boolean] =
      Matrix[(Int, Int), Boolean](m.raw.lt(d), m._2)(using BoundsCheck.DoBoundsCheck.no)

  end extension

  extension [@specialized(Double, Boolean) A](m: Matrix[A])

    private inline def range(r: RangeExtender, max: Int)(using ClassTag[A]): NArray[Int] = r match
      case _: ::.type     => NArray.from((0 until max).toArray)
      case r: Range       => NArray.from(r.toArray)
      case l: NArray[Int] => l
      case i: Int         => NArray(i)

    def apply(rowRange: RangeExtender, colRange: RangeExtender)(using ClassTag[A]): Matrix[A] =
      val newRows = range(rowRange, m.rows)
      val newCols = range(colRange, m.cols)
      val newArr = NArray.ofSize[A](newCols.size * newRows.size)

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

    /** Zero indexed element retrieval
      */
    inline def elementAt[T <: Tuple](b: T) =
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
    end elementAt

    inline def row(i: Int)(using ClassTag[A]): NArray[A] =
      val result = NArray.ofSize[A](m.cols)
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

    inline def print(using ClassTag[A]): String =
      val arrArr = for i <- 0 until m.rows yield m.row(i).toString()
      arrArr.mkString("\n")
    end print

    inline def col(i: Int)(using ClassTag[A]): NArray[A] =
      val result = NArray.ofSize[A](m.rows)
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

    inline def transpose(using ClassTag[A]): Matrix[A] =
      val newArr = NArray.ofSize[A](m._1.length)
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

end MatrixStuff
