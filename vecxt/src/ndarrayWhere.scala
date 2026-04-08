package vecxt

import scala.reflect.ClassTag

import vecxt.broadcast.*
import vecxt.ndarray.*

object NDArrayWhere:

  /** Element-wise conditional: where(condition, x, y) → result where result(i) = x(i) if condition(i) else y(i). All
    * three must have the same shape.
    */
  inline def where[A: ClassTag](
      condition: NDArray[Boolean],
      x: NDArray[A],
      y: NDArray[A]
  ): NDArray[A] =
    if !sameShape(condition.shape, x.shape) || !sameShape(condition.shape, y.shape) then
      throw ShapeMismatchException(
        s"where requires all three operands to have the same shape: " +
          s"condition=[${condition.shape.mkString(",")}], x=[${x.shape.mkString(",")}], y=[${y.shape.mkString(",")}]."
      )
    end if
    val n = condition.numel
    val out = new Array[A](n)
    if condition.isColMajor && x.isColMajor && y.isColMajor then
      var i = 0
      while i < n do
        out(i) = if condition.data(i) then x.data(i) else y.data(i)
        i += 1
      end while
    else
      val ndim = condition.ndim
      val cumProd = colMajorStrides(condition.shape)
      var j = 0
      while j < n do
        var posCond = condition.offset
        var posX = x.offset
        var posY = y.offset
        var k = 0
        while k < ndim do
          val coord = (j / cumProd(k)) % condition.shape(k)
          posCond += coord * condition.strides(k)
          posX += coord * x.strides(k)
          posY += coord * y.strides(k)
          k += 1
        end while
        out(j) = if condition.data(posCond) then x.data(posX) else y.data(posY)
        j += 1
      end while
    end if
    mkNDArray(out, condition.shape.clone(), colMajorStrides(condition.shape), 0)
  end where

  /** Scalar x variant: where(condition, x, y) with scalar x. */
  inline def where[A: ClassTag](
      condition: NDArray[Boolean],
      x: A,
      y: NDArray[A]
  ): NDArray[A] =
    if !sameShape(condition.shape, y.shape) then
      throw ShapeMismatchException(
        s"where requires condition and y to have the same shape: " +
          s"condition=[${condition.shape.mkString(",")}], y=[${y.shape.mkString(",")}]."
      )
    end if
    val n = condition.numel
    val out = new Array[A](n)
    if condition.isColMajor && y.isColMajor then
      var i = 0
      while i < n do
        out(i) = if condition.data(i) then x else y.data(i)
        i += 1
      end while
    else
      val ndim = condition.ndim
      val cumProd = colMajorStrides(condition.shape)
      var j = 0
      while j < n do
        var posCond = condition.offset
        var posY = y.offset
        var k = 0
        while k < ndim do
          val coord = (j / cumProd(k)) % condition.shape(k)
          posCond += coord * condition.strides(k)
          posY += coord * y.strides(k)
          k += 1
        end while
        out(j) = if condition.data(posCond) then x else y.data(posY)
        j += 1
      end while
    end if
    mkNDArray(out, condition.shape.clone(), colMajorStrides(condition.shape), 0)
  end where

  /** Scalar y variant: where(condition, x, y) with scalar y. */
  inline def where[A: ClassTag](
      condition: NDArray[Boolean],
      x: NDArray[A],
      y: A
  ): NDArray[A] =
    if !sameShape(condition.shape, x.shape) then
      throw ShapeMismatchException(
        s"where requires condition and x to have the same shape: " +
          s"condition=[${condition.shape.mkString(",")}], x=[${x.shape.mkString(",")}]."
      )
    end if
    val n = condition.numel
    val out = new Array[A](n)
    if condition.isColMajor && x.isColMajor then
      var i = 0
      while i < n do
        out(i) = if condition.data(i) then x.data(i) else y
        i += 1
      end while
    else
      val ndim = condition.ndim
      val cumProd = colMajorStrides(condition.shape)
      var j = 0
      while j < n do
        var posCond = condition.offset
        var posX = x.offset
        var k = 0
        while k < ndim do
          val coord = (j / cumProd(k)) % condition.shape(k)
          posCond += coord * condition.strides(k)
          posX += coord * x.strides(k)
          k += 1
        end while
        out(j) = if condition.data(posCond) then x.data(posX) else y
        j += 1
      end while
    end if
    mkNDArray(out, condition.shape.clone(), colMajorStrides(condition.shape), 0)
  end where

  /** Scalar x and y variant: where(condition, x, y) with scalar x and y. */
  inline def where[A: ClassTag](
      condition: NDArray[Boolean],
      x: A,
      y: A
  ): NDArray[A] =
    val n = condition.numel
    val out = new Array[A](n)
    if condition.isColMajor then
      var i = 0
      while i < n do
        out(i) = if condition.data(i) then x else y
        i += 1
      end while
    else
      val ndim = condition.ndim
      val cumProd = colMajorStrides(condition.shape)
      var j = 0
      while j < n do
        var posCond = condition.offset
        var k = 0
        while k < ndim do
          val coord = (j / cumProd(k)) % condition.shape(k)
          posCond += coord * condition.strides(k)
          k += 1
        end while
        out(j) = if condition.data(posCond) then x else y
        j += 1
      end while
    end if
    mkNDArray(out, condition.shape.clone(), colMajorStrides(condition.shape), 0)
  end where

end NDArrayWhere
