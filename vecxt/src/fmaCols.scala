package vecxt

import vecxt.matrix.*

/** Shared pieces of `fmaCols`: `out(i, j) = m(i, j) * multiply(j) + add(j)`.
  *
  * Each platform exposes the public `fmaCols` extension (`JvmDoubleMatrix`, `JsDoubleMatrix`, `NativeDoubleMatrix`).
  * JS and Native delegate straight to [[loop]]; the JVM uses a SIMD path when rows are contiguous and falls back to
  * [[loop]] otherwise. Deliberately not exported from `vecxt.all` - it is the kernel, not the API.
  */
private[vecxt] object FmaCols:

  /** Throws unless both vectors have one entry per column of `m`. Message built only on the failing path. */
  inline def check(m: Matrix[Double], multiply: Array[Double], add: Array[Double]): Unit =
    if multiply.length != m.cols then
      throw new IllegalArgumentException(s"multiply length ${multiply.length} != expected ${m.cols}")
    end if
    if add.length != m.cols then throw new IllegalArgumentException(s"add length ${add.length} != expected ${m.cols}")
    end if
  end check

  /** Layout-agnostic kernel: reads through the strides, writes a fresh dense column-major matrix.
    *
    * Columns outer, rows inner, so the write is always sequential. Uses a plain `x * mul + ad` rather than
    * `Math.fma`, which is emulated (slowly) on Scala.js; results may therefore differ from the JVM SIMD path in the
    * last bit.
    */
  def loop(m: Matrix[Double], multiply: Array[Double], add: Array[Double]): Matrix[Double] =
    check(m, multiply, add)
    val rows = m.rows
    val out = new Array[Double](m.numel)
    var j = 0
    while j < m.cols do
      val src = m.offset + j * m.colStride
      val dst = j * rows
      val mul = multiply(j)
      val ad = add(j)
      var i = 0
      while i < rows do
        out(dst + i) = m.raw(src + i * m.rowStride) * mul + ad
        i += 1
      end while
      j += 1
    end while
    Matrix[Double](out, m.rows, m.cols)
  end loop

end FmaCols
