package experiments

import vecxt.all.*
import vecxt.ndarray.NDArray
import vecxt.ndarray.NDArray.*
import vecxt.ndarray.mkNDArray
import vecxt.ndarray.colMajorStrides
import vecxt.BoundsCheck.BoundsCheck

//////////////////////////////////////////////
// Three Pass Regression Filter – NDArray  //
/////////////////////////////////////////////
// NDArray-based translation of ThreePrf.
//
// Mapping from Matrix[Double]:
//   Matrix[Double] (r x c)        -> NDArray[Double] with shape Array(r, c)
//   Matrix.zeros[Double]((r,c))   -> NDArray.zeros[Double](Array(r,c))
//   Matrix.ones[Double]((r,c))    -> NDArray.ones[Double](Array(r,c))
//   Matrix.eye[Double](n)         -> ndEye(n)
//   Matrix.fill[Double](v,(r,c))  -> NDArray.fill(Array(r,c), v)
//   m.rows / m.cols               -> m.shape(0) / m.shape(1)
//   m.numel                       -> m.numel
//   m.raw                         -> m.data  (only if col-major)
//   m @@ b                        -> a @@ b
//   m.T                           -> m.T
//   m * scalar                    -> m * scalar
//   m.inv  (JVM only)             -> ndInv(m)        (bridges through Matrix)
//   solve(A, b)  (JVM only)       -> ndSolve(A, b)   (bridges through Matrix)
//   m.deepCopy                    -> ndCopy(m)

object ThreePrfNd:

  private inline given bc: BoundsCheck = vecxt.BoundsCheck.DoBoundsCheck.no

  private val rng = new scala.util.Random(42)

  var seed: Int    = 42
  var n_proxy: Int = 1

  // ─── NDArray ↔ Matrix bridges ──────────────────────────────────────────────

  /** Wrap a 2-D NDArray as a Matrix without copying (zero-copy when col-major). */
  private def ndToMat(a: NDArray[Double]): Matrix[Double] =
    Matrix(a.data, a.shape(0), a.shape(1), a.strides(0), a.strides(1), a.offset)

  /** Wrap a Matrix as a col-major NDArray without copying. */
  private def matToNd(m: Matrix[Double]): NDArray[Double] =
    val shape   = Array(m.rows, m.cols)
    val strides = Array(m.rowStride, m.colStride)
    mkNDArray(m.raw, shape, strides, m.offset)

  /** Identity matrix as NDArray (col-major, n x n). */
  private def ndEye(n: Int): NDArray[Double] =
    val data = new Array[Double](n * n)
    var i = 0
    while i < n do
      data(i + i * n) = 1.0
      i += 1
    end while
    NDArray[Double](data, Array(n, n))

  /** Deep copy of a 2-D NDArray. */
  private def ndCopy(a: NDArray[Double]): NDArray[Double] =
    // Materialise as col-major if needed, then clone data
    if a.isColMajor then
      NDArray[Double](a.data.clone(), a.shape.clone())
    else
      val n    = a.numel
      val out  = new Array[Double](n)
      for i <- 0 until a.shape(0); j <- 0 until a.shape(1) do
        out(j * a.shape(0) + i) = a(i, j)
      NDArray[Double](out, a.shape.clone())

  /** Matrix inverse via LAPACK (JVM only). */
  private def ndInv(a: NDArray[Double]): NDArray[Double] =
    matToNd(ndToMat(a).inv)

  /** Solve A x = b via LAPACK (JVM only). */
  private def ndSolve(A: NDArray[Double], b: NDArray[Double]): NDArray[Double] =
    matToNd(solve(ndToMat(A), ndToMat(b)))

  // ─── main entry point ──────────────────────────────────────────────────────

  def TPRF(
    _X: NDArray[Double],
    y: NDArray[Double],           // column vector, shape (T, 1)
    _Z: NDArray[Double],
    L: Int = 0,
    pls: Boolean = false,
    centerData: Boolean = false,
    scaleData: Boolean = true,
    closed_form: Boolean = false,
    fitalg: Int = 2
  ): T3prf =
    if y.shape(1) != 1 then sys.error("y should be univariate")

    val (centerFinal, scaleFinal) =
      if pls then (true, true) else (centerData, scaleData)

    // L == 0 means "use the provided Z as-is". Only auto-generate when L > 0.
    val n_proxyLocal = if L == 0 then 0 else if closed_form then 1 else L
    ThreePrfNd.n_proxy = n_proxyLocal

    if n_proxyLocal == 0 && (_Z.shape(0) == 0 || _Z.shape(1) == 0) then
      sys.error("please either provide proxies or choose a number of automatic proxies to build")

    val X = scaleMatrix(_X, centerFinal, scaleFinal)

    val Z =
      if n_proxyLocal == 0 then _Z
      else autoProxies(n_proxyLocal, X, y, pls, closed_form, fitalg)

    val fit = tprf_fit(X, y, Z, pls = pls, closed_form = closed_form, fitalg = fitalg)
    T3prf(fit, pls, n_proxyLocal, closed_form, centerFinal, scaleFinal)

  // ─── data classes ──────────────────────────────────────────────────────────

  case class T3prf(
    fit: PredReg,
    pls: Boolean,
    n_proxy: Int,
    closed_form: Boolean,
    center: Boolean,
    scale: Boolean
  ):
    def alpha_hat = fit.alpha_hat
    def y_hat     = fit.y_hat
    def residuals = fit.residuals
    def loadings  = fit.loadings
    def factors   = fit.factors

  case class PredReg(
    alpha_hat: NDArray[Double],
    y_hat: NDArray[Double],
    residuals: NDArray[Double],
    loadings: NDArray[Double],
    factors: NDArray[Double]
  )

  // ─── automatic proxies ─────────────────────────────────────────────────────

  def autoProxies(
    n_proxy: Int,
    X: NDArray[Double],
    y: NDArray[Double],
    pls: Boolean = false,
    closed_form: Boolean = true,
    fitalg: Int = 2
  ): NDArray[Double] =
    val T = y.shape(0)
    val r = NDArray.zeros[Double](Array(T, n_proxy))
    // r[:, 0] = y
    for i <- 0 until T do r(i, 0) = y(i, 0)
    for k <- 1 until n_proxy do
      val slice = colSlice(r, k - 1)
      val fitR  = tprf_fit(X, y, slice, pls, closed_form, fitalg)
      setCol(r, k, fitR.residuals)
    r

  // ─── dispatch ──────────────────────────────────────────────────────────────

  def tprf_fit(
    X: NDArray[Double],
    y: NDArray[Double],
    Z: NDArray[Double],
    pls: Boolean = false,
    closed_form: Boolean = true,
    fitalg: Int = 2
  ): PredReg =
    if closed_form then tprf_fit_closed(X, y, Z, pls = pls)
    else tprf_fit_iter(X, y, Z, pls = pls, fitalg = fitalg)

  // ─── closed form ───────────────────────────────────────────────────────────

  /** J_T = I_T - (1/T) * 1_T * 1_T'  (demeaning projection matrix) */
  def J(T: Int): NDArray[Double] =
    val It  = ndEye(T)
    val ιt  = NDArray.ones[Double](Array(T, 1))
    It - ((ιt @@ ιt.T) * (1.0 / T))

  def tprf_fit_closed(
    X: NDArray[Double],
    y: NDArray[Double],
    Z: NDArray[Double],
    pls: Boolean = false
  ): PredReg =
    val T  = X.shape(0)
    val N  = X.shape(1)
    val Jn = J(N)
    val Jt = J(T)

    val (y_hat, alpha_hat) =
      if pls then
        val XX     = (X @@ X.T)
        val part1  = (XX.T @@ y)
        val part2  = 1.0 / (y.T @@ XX @@ XX @@ y)(0, 0)
        val part3  = ((y.T @@ XX) @@ y)(0, 0)
        val yh     = (part1 * (part2 * part3)) + colMeanVal(y)
        (yh, NDArray.zeros[Double](Array(yh.shape(0), 1)))
      else
        val wxzRite = (Jt @@ Z)
        val wxzLeft = (Jn @@ X.T)
        val W_XZ    = (wxzLeft @@ wxzRite)
        val Xt      = X.T
        val S_XX    = ((Xt @@ Jt) @@ X)
        val S_Xy    = ((Xt @@ Jt) @@ y)
        val inner   = ndInv((W_XZ.T @@ S_XX @@ W_XZ))
        val alpha   = (W_XZ @@ inner @@ W_XZ.T @@ S_Xy)
        val yh      = ((Jt @@ X) @@ alpha) + colMeanVal(y)
        (yh, alpha)

    val residuals = y - y_hat
    PredReg(alpha_hat, y_hat, residuals, ndNull, ndNull)

  // ─── iterative (pass I / II / III) ─────────────────────────────────────────

  def tprf_fit_iter(
    X: NDArray[Double],
    y: NDArray[Double],
    Z: NDArray[Double],
    pls: Boolean = false,
    fitalg: Int = 2
  ): PredReg =
    val T = X.shape(0)
    val N = X.shape(1)
    val plsNum = if pls then 0 else 1

    // Pass I: time-series regression – loadings (N x (K_z + plsNum))
    val loadings = ndFill(N, Z.shape(1) + plsNum, Double.NaN)

    if pls then
      for j <- 0 until N do
        val Xj   = colSlice(X, j)
        val coef = leastSquares(Z, Xj)
        setRow(loadings, j, coef.T)
    else
      for i <- 0 until N do loadings(i, 0) = 1.0
      for j <- 1 until N do
        val Xj  = colSlice(X, j)
        val Zm  = prependOnesColumn(Z)
        val coef = leastSquares(Zm, Xj)
        setRow(loadings, j, coef.T)

    // Pass II: cross-section regression – factors (T x loadings.cols)
    val factors = ndFill(T, loadings.shape(1), Double.NaN)

    if pls then
      for i <- 0 until T do
        val Xi   = rowSlice(X, i).T
        val coef = leastSquares(loadings, Xi)
        setRow(factors, i, coef.T)
    else
      for i <- 1 until T do
        val Xi   = rowSlice(X, i).T
        val coef = leastSquares(loadings, Xi)
        setRow(factors, i, coef.T)

    // Pass III: predictive regression
    val factors_reg =
      if pls then ndCopy(factors)
      else submatrixCols(factors, 1, factors.shape(1) - 1)

    val factors_wi = prependOnesColumn(factors_reg)
    val result     = leastSquares(factors_wi, y)

    // NOTE: tprf_fit_iter is still a work-in-progress – doesn't yet return
    // a proper y_hat / loadings / factors consistent with tprf_fit_closed.
    // The alpha_hat here is the predictive coefficients, not the structural loadings.
    PredReg(result, ndNull, ndNull, loadings, factors)

  // ─── helpers ───────────────────────────────────────────────────────────────

  /** Least squares solve for overdetermined systems using normal equations.
    *  x = (A'A)^{-1} A'b
    */
  def leastSquares(A: NDArray[Double], b: NDArray[Double]): NDArray[Double] =
    val AtA = (A.T @@ A)
    val Atb = (A.T @@ b)
    ndSolve(AtA, Atb)

  def prependOnesColumn(m: NDArray[Double]): NDArray[Double] =
    val T    = m.shape(0)
    val ones = NDArray.ones[Double](Array(T, 1))
    hstack(ones, m)

  def hstack(left: NDArray[Double], right: NDArray[Double]): NDArray[Double] =
    assert(left.shape(0) == right.shape(0))
    val rows = left.shape(0)
    val cols = left.shape(1) + right.shape(1)
    val out  = NDArray.zeros[Double](Array(rows, cols))
    for j <- 0 until left.shape(1); i <- 0 until rows do
      out(i, j) = left(i, j)
    for j <- 0 until right.shape(1); i <- 0 until rows do
      out(i, left.shape(1) + j) = right(i, j)
    out

  /** Extract column j as a (rows x 1) NDArray. */
  def colSlice(m: NDArray[Double], j: Int): NDArray[Double] = m(::, Array(j))

  /** Extract row i as a (1 x cols) NDArray. */
  def rowSlice(m: NDArray[Double], i: Int): NDArray[Double] = m(Array(i), ::)

  /** Write src (1 x k or k x 1) into row i of dest. */
  def setRow(dest: NDArray[Double], i: Int, src: NDArray[Double]): Unit =
    val k = if src.shape(0) == 1 then src.shape(1) else src.shape(0)
    for j <- 0 until k do
      dest(i, j) = if src.shape(0) == 1 then src(0, j) else src(j, 0)

  /** Write src (rows x 1 or 1 x rows) into column j of dest. */
  def setCol(dest: NDArray[Double], j: Int, src: NDArray[Double]): Unit =
    val k = if src.shape(1) == 1 then src.shape(0) else src.shape(1)
    for i <- 0 until k do
      dest(i, j) = if src.shape(1) == 1 then src(i, 0) else src(0, i)

  /** Extract columns [from, to] inclusive. */
  def submatrixCols(m: NDArray[Double], from: Int, to: Int): NDArray[Double] =
    val ncols = to - from + 1
    val rows  = m.shape(0)
    val out   = NDArray.zeros[Double](Array(rows, ncols))
    for j <- 0 until ncols; i <- 0 until rows do
      out(i, j) = m(i, from + j)
    out

  /** Allocate (rows x cols) filled with `v`. */
  private def ndFill(rows: Int, cols: Int, v: Double): NDArray[Double] =
    NDArray.fill(Array(rows, cols), v)

  /** Column mean of a (T x 1) NDArray. */
  def colMeanVal(m: NDArray[Double]): Double =
    assert(m.shape(1) == 1)
    m.data.sum / m.shape(0)

  /** Centre and optionally scale each column of X. */
  def scaleMatrix(X: NDArray[Double], centerData: Boolean, scaleData: Boolean): NDArray[Double] =
    if !centerData && !scaleData then return ndCopy(X)
    val rows = X.shape(0)
    val cols = X.shape(1)
    val out  = ndCopy(X)
    for j <- 0 until cols do
      val col = Array.tabulate(rows)(i => X(i, j))
      val mu  = col.sum / rows
      val sd  =
        if scaleData then
          val v = col.map(x => (x - mu) * (x - mu)).sum / scala.math.max(rows - 1, 1)
          scala.math.sqrt(v)
        else 1.0
      for i <- 0 until rows do
        val centred = if centerData then col(i) - mu else col(i)
        out(i, j) = if scaleData && sd > 0 then centred / sd else centred
    out

  lazy val ndNull: NDArray[Double] = NDArray.zeros[Double](Array(0, 0))

end ThreePrfNd
