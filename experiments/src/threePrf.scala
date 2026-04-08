package experiments

import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.no

/////////////////////////////////////////////
// Three Pass Regression Filter Estimators //
/////////////////////////////////////////////
// Translated from a scala-cli / uni/breeze version into vecxt Matrix[Double] API.
//
// Mapping from uni to vecxt:
//   MatD                  -> Matrix[Double]
//   VecD (column vector)  -> Matrix[Double] (rows x 1)
//   RVecD (row vector)    -> Matrix[Double] (1 x cols)
//   *@                    -> @@
//   .T                    -> .T
//   .inverse              -> .inv  (JVM only)
//   MatD.zeros(r, c)      -> Matrix.zeros((r, c))
//   MatD.ones(n)          -> Matrix.ones((n, 1))
//   MatD.eye(n)           -> Matrix.eye(n)
//   MatD.full(r, c, v)    -> Matrix.fill(v, (r, c))
//   leastSquares(X, y)    -> leastSquaresVecxt(X, y)  (via QR / normal equations)

object ThreePrf:

  private val rng = new scala.util.Random(42)

  var seed: Int  = 42
  var n_proxy: Int = 1
  var center: Boolean = false
  var scale: Boolean  = true
  // sigma_g row vector (1 x 4)
  val sigma_g: Matrix[Double] = Matrix(Array(1.25, 1.75, 2.25, 2.75), 1, 4)
  val sigma_y: Int = 1

  // @main def runThreePrf(): Unit =
    // rng.setSeed(seed)
    // val (_T, _N, _K_f, _L, _center, _scale, pls, closed_form, _fitalg) =
    //   (200, 200, 1, 1, true, true, true, false, 2)

    // val sim = sim_problem(_T, _N, _K_f, sigma_g, _L, sigma_y)
    // val allfactors = cbind(sim.factors)
    // val y_inf = allfactors @@ sim.beta + sim.beta_0

    // val Z   = autoProxies(n_proxy, sim.X, sim.y, pls, closed_form, _fitalg)
    // val fit = TPRF(sim.X, sim.y, Z, L = 0, pls, _center, _scale, closed_form, _fitalg)
    // val cc: Matrix[Double] = fit.loadings
    // dump4x4("loadings", cc)

  // ─── main entry point ──────────────────────────────────────────────────────

  def TPRF(
    _X: Matrix[Double],
    y: Matrix[Double],           // was VecD – keep as column matrix (T x 1)
    _Z: Matrix[Double],
    L: Int = 0,
    pls: Boolean = false,
    centerData: Boolean = false,
    scaleData: Boolean = true,
    closed_form: Boolean = false,
    fitalg: Int = 2
  ): T3prf =
    if y.cols != 1 then sys.error("y should be univariate")

    val (centerFinal, scaleFinal) =
      if pls then (true, true) else (centerData, scaleData)

    // L == 0 means "use the provided Z as-is". Only auto-generate proxies when L > 0.
    val n_proxyLocal = if L == 0 then 0 else if closed_form then 1 else L
    ThreePrf.n_proxy  = n_proxyLocal

    if n_proxyLocal == 0 && (_Z.rows == 0 || _Z.cols == 0) then
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
    def alpha_hat  = fit.alpha_hat
    def y_hat      = fit.y_hat
    def residuals  = fit.residuals
    def loadings   = fit.loadings
    def factors    = fit.factors

  case class PredReg(
    alpha_hat: Matrix[Double],
    y_hat: Matrix[Double],
    residuals: Matrix[Double],
    loadings: Matrix[Double],
    factors: Matrix[Double]
  )

  // ─── automatic proxies ─────────────────────────────────────────────────────

  def autoProxies(
    n_proxy: Int,
    X: Matrix[Double],
    y: Matrix[Double],
    pls: Boolean = false,
    closed_form: Boolean = true,
    fitalg: Int = 2
  ): Matrix[Double] =
    val r = Matrix.zeros[Double]((y.rows, n_proxy))
    // r[:, 0] = y
    for i <- 0 until y.rows do r(i, 0) = y(i, 0)

    printf("auprx:X             %5d x %5d\n", X.rows, X.cols)
    printf("auprx:y             %5d x %5d\n", y.rows, y.cols)
    printf("auprx:r             %5d x %5d\n", r.rows, r.cols)

    for k <- 1 until n_proxy do
      val slice = r(::, Array(k - 1))           // (T x 1) column k-1
      val fitR = tprf_fit(X, y, slice, pls, closed_form, fitalg)
      setCol(r, k, fitR.residuals)
    r

  // ─── dispatch ──────────────────────────────────────────────────────────────

  def tprf_fit(
    X: Matrix[Double],
    y: Matrix[Double],
    Z: Matrix[Double],
    pls: Boolean = false,
    closed_form: Boolean = true,
    fitalg: Int = 2
  ): PredReg =
    if closed_form then tprf_fit_closed(X, y, Z, pls = pls)
    else tprf_fit_iter(X, y, Z, pls = pls, fitalg = fitalg)

  // ─── iterative (pass I / II / III) ─────────────────────────────────────────

  def tprf_fit_iter(
    X: Matrix[Double],
    y: Matrix[Double],
    Z: Matrix[Double],
    pls: Boolean = false,
    closed_form: Boolean = true,
    fitalg: Int = 2
  ): PredReg =
    printf("fit_iter:X          %5d x %5d\n", X.rows, X.cols)
    printf("fit_iter:y          %5d x %5d\n", y.rows, y.cols)
    printf("fit_iter:Z          %5d x %5d\n", Z.rows, Z.cols)

    val plsNum = if pls then 0 else 1  // no intercept if pls

    // Pass I: time-series regression  – loadings (N x (K_z + plsNum))
    val loadings = matrixNaN(X.cols, Z.cols + plsNum)
    printf("fit_iter:loadings:  %5d x %5d\n", loadings.rows, loadings.cols)

    if pls then
      // X[j] ~ Z - 1
      for j <- 0 until loadings.rows do
        val Xj   = colAsMatrix(X, j)                      // (T x 1)
        val coef = leastSquaresVecxt(Z, Xj)               // (K_z x 1)
        if j == 0 then printf("fit_iter:coeffs:    %5d x %5d\n", coef.rows, coef.cols)
        setRow(loadings, j, coef.T)
    else
      // first column of loadings = ones
      for i <- 0 until loadings.rows do loadings(i, 0) = 1.0
      for j <- 1 until loadings.rows do
        val Xj  = colAsMatrix(X, j)
        val Zm  = prependOnesColumn(Z)
        val coef = leastSquaresVecxt(Zm, Xj)
        if j == 0 then printf("fit_iter:coeffs:    %5d x %5d\n", coef.rows, coef.cols)
        setRow(loadings, j, coef.T)

    // Pass II: cross-section regression – factors (T x loadings.cols)
    val factors = matrixNaN(X.rows, loadings.cols)

    if pls then
      for i <- 0 until factors.rows do
        val Xi  = rowAsMatrix(X, i).T                     // (N x 1)
        val coef = leastSquaresVecxt(loadings, Xi)
        setRow(factors, i, coef.T)
    else
      for i <- 1 until factors.rows do
        val Xi  = rowAsMatrix(X, i).T                     // (N x 1)
        val coef = leastSquaresVecxt(loadings, Xi)
        if i == 1 then printf("fit_iter:L1:        %5d x %5d\n", loadings.rows, loadings.cols)
        if i == 1 then printf("fit_iter:coefs:     %5d x %5d\n", coef.rows, coef.cols)
        setRow(factors, i, coef.T)

    // Pass III: predictive regression
    val factors_reg: Matrix[Double] =
      if pls then factors.deepCopy
      else submatrixCols(factors, 1, factors.cols - 1)   // drop first column

    // NOTE: the original prependOnesColumn / intercept approach differs slightly
    // between the R original and this Scala translation. Treating it as:
    //   y ~ 1 + factors_reg  (intercept included via prependOnesColumn)
    val factors_with_intercept = prependOnesColumn(factors_reg)
    //  NOTE: leastSquaresVecxt below is a stub placeholder; QR-based solve is JVM-only.
    //  val result = leastSquaresVecxt(factors_with_intercept, y)

    PredReg(NullMat, NullMat, NullMat, loadings, factors)

  // ─── closed form ───────────────────────────────────────────────────────────

  // J_T = I_T - (1/T) * 1_T * 1_T'
  def J(T: Double): Matrix[Double] =
    val n   = T.toInt
    val It  = Matrix.eye[Double](n)
    val ιt  = Matrix.ones[Double]((n, 1))
    // It - (1/T) * (ιt @@ ιt.T)
    It - ((ιt @@ ιt.T) * (1.0 / T))

  def tprf_fit_closed(
    Xorig: Matrix[Double],
    y: Matrix[Double],
    Z: Matrix[Double],
    pls: Boolean = false
  ): PredReg =
    val X = Xorig.deepCopy
    printf("fit_closed:X:       %5d x %5d\n", X.rows, X.cols)
    printf("fit_closed:y:       %5d x %5d\n", y.rows, y.cols)
    printf("fit_closed:Z:       %5d x %5d\n", Z.rows, Z.cols)

    val (y_hat, alpha_hat) =
      if pls then
        val XX     = X @@ X.T
        val part1  = XX.T @@ y
        val part2  = 1.0 / (y.T @@ XX @@ XX @@ y)(0, 0)
        val part3  = (y.T @@ XX @@ y)(0, 0)
        val yh     = part1 * (part2 * part3) + colMean(y)
        (yh, Matrix.zeros[Double]((yh.rows, 1)))
      else
        val T  = X.rows
        val N  = X.cols
        val Jn = J(N.toDouble)
        val Jt = J(T.toDouble)
        printf("fit_closed:Jn:      %5d x %5d\n", Jn.rows, Jn.cols)
        printf("fit_closed:Jt:      %5d x %5d\n", Jt.rows, Jt.cols)

        val wxzRite = Jt @@ Z
        val wxzLeft = Jn @@ X.T
        val W_XZ    = wxzLeft @@ wxzRite
        val Xt      = X.T
        val S_XX    = Xt @@ Jt @@ X
        val S_Xy    = Xt @@ Jt @@ y
        // NOTE: .inv is JVM-only (via adjugate/determinant). May be slow for large matrices –
        // consider using Solve.solve instead for production code.
        val alpha_hat = W_XZ @@ (W_XZ.T @@ S_XX @@ W_XZ).inv @@ W_XZ.T @@ S_Xy
        val yh        = Jt @@ X @@ alpha_hat + colMean(y)
        (yh, alpha_hat)

    val residuals: Matrix[Double] = y - y_hat
    PredReg(alpha_hat, y_hat, residuals, NullMat, NullMat)

  // ─── simulation ────────────────────────────────────────────────────────────

  def sim_factors(
    T: Int,
    K_f: Int = 1,
    rho_f: Double = 0.0,
    rho_g: Double = 0.0,
    sigma_g: Matrix[Double] = sigma_g
  ): Seq[Matrix[Double]] =

    // Relevant factors
    val u_f = rnorm(T * K_f, T, K_f)   // (T x K_f)
    val f   = Matrix.zeros[Double]((T, K_f))
    setRow(f, 0, rowAsMatrix(u_f, 0))
    for i <- 1 until f.rows do
      val prevRow = rowAsMatrix(f, i - 1).T * rho_f  // (K_f x 1)
      val prevU   = rowAsMatrix(u_f, i - 1).T        // (K_f x 1)
      val newRow  = prevRow + prevU
      setRow(f, i, newRow.T)

    // Irrelevant factors
    val K_g = sigma_g.numel
    val col0 = colAsMatrix(f, 0)  // (T x 1)
    val col0var = sampleVariance(col0)
    // col0variance = col0var * sigma_g  (element-wise, sigma_g is 1 x K_g)
    val col0varVec = sigma_g * col0var   // (1 x K_g)
    val col0varDiag = Matrix.createDiagonal( col0varVec.diag )  // (K_g x K_g) diagonal matrix

    printf("sim_f:col0varDiag   %5d x %5d\n", col0varDiag.rows, col0varDiag.cols)

    var g: List[Matrix[Double]] =
      if K_g > 0 then
        val sigma_g_sqrt = col0varDiag.sqrt
        val matRnorm  = rnorm(T * K_g, T, K_g)
        printf("sim_f:matRnorm      %5d x %5d\n", matRnorm.rows, matRnorm.cols)
        printf("sim_f:sigma_g_sqrt  %5d x %5d\n", sigma_g_sqrt.rows, sigma_g_sqrt.cols)
        val u_g = matRnorm @@ sigma_g_sqrt
        printf("sim_f:u_g           %5d x %5d\n", u_g.rows, u_g.cols)

        val gMat = Matrix.zeros[Double]((T, K_g))
        setRow(gMat, 0, rowAsMatrix(u_g, 0))
        for i <- 1 until gMat.rows do
          try
            val gprevRow = rowAsMatrix(gMat, i - 1)
            val u_giRow  = rowAsMatrix(u_g, i)
            val gnuRow   = gprevRow * rho_g + u_giRow
            setRow(gMat, i, gnuRow)
          catch
            case t: Throwable =>
              printf("last loop before possible exception, i: %d\n", i)
              throw t
        List(gMat)
      else Nil

    f :: g

  //##' @export
  def sim_target(
    factors: Seq[Matrix[Double]],
    beta_0: Double,
    beta: Matrix[Double],
    sigma_y: Int = 1
  ): Matrix[Double] =
    printf("sim_target(factors, beta_0:%s, beta, sigma_y:%s)\n", beta_0, sigma_y)
    val F   = mergeFactorColumns(factors)
    val T   = F.rows
    val u_y = rnorm(T, T, 1)
    val Fxbeta = F @@ beta
    printf("sim_t:F             %5d x %5d\n", F.rows, F.cols)
    printf("sim_t:beta          %5d x %5d\n", beta.rows, beta.cols)
    printf("sim_t:Fxbeta        %5d x %5d\n", Fxbeta.rows, Fxbeta.cols)
    printf("sim_t:u_y           %5d x %5d\n", u_y.rows, u_y.cols)
    val y = Fxbeta + beta_0 + u_y * sigma_y.toDouble
    printf("sim_t:y             %5d x %5d\n", y.rows, y.cols)
    assert(y.cols == 1, s"too many columns: ${y.cols}")
    y   // already (T x 1)

  //##' @export
  def sim_observations(
    N: Int,
    factors: Seq[Matrix[Double]],
    phi_0: Double,
    phi: Matrix[Double]
  ): Matrix[Double] =
    printf("sim_o:factors$f     %5d x %5d\n", factors(0).rows, factors(0).cols)
    if factors.size > 1 then printf("sim_o:factors$g     %5d x %5d\n", factors(1).rows, factors(1).cols)
    val F = mergeFactorColumns(factors)
    val T = F.rows
    val epsilon = rnorm(T * N, T, N)
    printf("sim_o:rnorm(T*N)    %5d x %5d\n", T * N, 1)
    printf("sim_o:epsilon       %5d x %5d\n", epsilon.rows, epsilon.cols)
    printf("sim_o:F             %5d x %5d\n", F.rows, F.cols)
    printf("sim_o:phi           %5d x %5d\n", phi.rows, phi.cols)
    val FxphiT = F @@ phi.T
    FxphiT + epsilon + phi_0

  def sim_proxies(
    L: Int,
    factors: Seq[Matrix[Double]],
    lambda_0: Double,
    lambda: Matrix[Double]
  ): Matrix[Double] =
    sim_observations(L, factors, lambda_0, lambda)

  def mergeFactorColumns(factors: Seq[Matrix[Double]]): Matrix[Double] =
    factors match
      case Seq(f)    => f
      case Seq(f, g) => hstack(f, g)
      case _         => sys.error(s"bad factors: $factors")

  //##' @export
  def sim_problem(T: Int, N: Int, K_f: Int, sigma_g: Matrix[Double], L: Int = 2, sigma_y: Int = 1): Simulation =
    assert(L > 0, s"error: L <= 0, but at least one proxy is required")

    val factors: Seq[Matrix[Double]] = sim_factors(T, K_f, sigma_g = sigma_g)
    for (fact, i) <- factors.zipWithIndex do
      printf("sim_p:factors(%d)    %5d x %5d\n", i, fact.rows, fact.cols)

    val F = mergeFactorColumns(factors)
    val K = F.cols
    printf("sim_p:K:                    %5d\n", K)

    val phi_0   = runifDbl(-1.0, 1.0)
    printf("sim_p:N x K:        %5d x %5d\n", N, K)
    val phi     = runif(N * K, -1.0, 1.0, N, K)
    printf("sim_p:phi.T         %5d x %5d\n", phi.T.rows, phi.T.cols)
    val X       = sim_observations(N, factors, phi_0, phi)

    val lambda_0 = runifDbl(-1.0, 1.0)
    val lambda   = runif(L * K, -1.0, 1.0, L, K)
    val Z        = sim_proxies(L, factors, lambda_0, lambda)

    val beta_0 = runifDbl(-1.0, 1.0)
    val K_g    = K - K_f
    // beta: first K_f entries uniform(-1,1), remaining zeros
    val betaArr =
      Array.fill(K_f)(rng.nextDouble() * 2.0 - 1.0) ++ Array.fill(K - K_f)(0.0)
    val beta = Matrix(betaArr, K, 1)
    val y    = sim_target(factors, beta_0, beta)

    Simulation(factors, phi_0, phi, X, K, K_f, K_g, sigma_g, lambda_0, lambda, Z, beta_0, beta, y, sigma_y.toDouble)

  case class Simulation(
    factors: Seq[Matrix[Double]],
    phi_0: Double,
    phi: Matrix[Double],
    X: Matrix[Double],
    K: Int,
    K_f: Int,
    K_g: Int,
    sigma_g: Matrix[Double],
    lambda_0: Double,
    lambda: Matrix[Double],
    Z: Matrix[Double],
    beta_0: Double,
    beta: Matrix[Double],
    y: Matrix[Double],
    sigma_y: Double
  )

  // ─── helpers ───────────────────────────────────────────────────────────────

  /** Least squares solve for overdetermined systems (M × N, M >= N).
    *  Uses QR decomposition: x = (A'A)^{-1} A'b
    *
    * NOTE: For large matrices prefer QR via LAPACK (vecxt.QR.qr).
    * The simple normal-equations approach below may be numerically unstable.
    * A proper QR-based implementation would be:
    *   val (q, r) = qr(A)
    *   solve(r, q.T @@ b)
    * But vecxt.QR.qr currently only gives Q and R, not a solve method.
    * TODO: replace with a proper LAPACK dgels / dgels-backed call once available.
    */
  def leastSquaresVecxt(A: Matrix[Double], b: Matrix[Double]): Matrix[Double] =
    // Normal equations: (A'A) x = A'b
    val AtA = A.T @@ A
    val Atb = A.T @@ b
    // solve is square system now
    solve(AtA, Atb)

  def prependOnesColumn(m: Matrix[Double]): Matrix[Double] =
    val ones = Matrix.ones[Double]((m.rows, 1))
    hstack(ones, m)

  /** Stack two matrices side by side (column-wise). */
  def hstack(left: Matrix[Double], right: Matrix[Double]): Matrix[Double] = left.vertcat(right)


  def cbind(mats: Seq[Matrix[Double]]): Matrix[Double] =
    mats.reduce(hstack)

  /** Extract column j as a (rows x 1) matrix. */
  def colAsMatrix(m: Matrix[Double], j: Int): Matrix[Double] = m(::, Array(j))

  /** Extract row i as a (1 x cols) matrix. */
  def rowAsMatrix(m: Matrix[Double], i: Int): Matrix[Double] = m(Array(i), ::)

  /** Write src (1 x cols or cols x 1) into row i of dest. */
  def setRow(dest: Matrix[Double], i: Int, src: Matrix[Double]): Unit =
    val cols = if src.rows == 1 then src.cols else src.rows
    for j <- 0 until cols do
      dest(i, j) = if src.rows == 1 then src(0, j) else src(j, 0)

  /** Write src (rows x 1 or 1 x rows matrix) into column j of dest. */
  def setCol(dest: Matrix[Double], j: Int, src: Matrix[Double]): Unit =
    val rows = if src.cols == 1 then src.rows else src.cols
    for i <- 0 until rows do
      dest(i, j) = if src.cols == 1 then src(i, 0) else src(0, i)

  /** Extract columns [from, to] inclusive as a new matrix. */
  def submatrixCols(m: Matrix[Double], from: Int, to: Int): Matrix[Double] = m(::, from to to)

  /** Build a diagonal (n x n) matrix from a row or column matrix of values. */
  // def diag(v: Matrix[Double]): Matrix[Double] = diag(v)

  def matrixNaN(rows: Int, cols: Int): Matrix[Double] =
    Matrix.NaN((rows, cols))

  lazy val NullMat: Matrix[Double] = Matrix.zeros[Double]((0, 0))

  /** Standard Normal samples (T x K matrix, column-major). */
  def rnorm(n: Int, rows: Int, cols: Int): Matrix[Double] =
    val arr = Array.fill(n)(rng.nextGaussian())
    Matrix(arr, rows, cols)

  /** Uniform samples in [min, max] shaped into (rows x cols). */
  def runif(n: Int, min: Double, max: Double, rows: Int, cols: Int): Matrix[Double] =
    val arr = Array.fill(n)(min + (max - min) * rng.nextDouble())
    Matrix(arr, rows, cols)

  def runifDbl(min: Double, max: Double): Double =
    min + (max - min) * rng.nextDouble()

  /** Sample variance (unbiased) of all elements. */
  def sampleVariance(m: Matrix[Double]): Double =
    val n  = m.numel
    val mu = m.raw.sum / n
    m.raw.map(x => (x - mu) * (x - mu)).sum / scala.math.max(n - 1, 1)

  /** Column mean of a (T x 1) matrix, returned as a scalar. */
  def colMean(m: Matrix[Double]): Double =
    assert(m.cols == 1, "colMean expects a column vector")
    m.raw.sum / m.rows

  /** Centre and optionally scale the columns of X. */
  def scaleMatrix(X: Matrix[Double], centerData: Boolean, scaleData: Boolean): Matrix[Double] =
    if !centerData && !scaleData then return X.deepCopy
    val out = X.deepCopy
    for j <- 0 until X.cols do
      val col = Array.tabulate(X.rows)(i => X(i, j))
      val mu  = col.sum / col.length
      val sd  =
        if scaleData then
          val v = col.map(x => (x - mu) * (x - mu)).sum / scala.math.max(col.length - 1, 1)
          scala.math.sqrt(v)
        else 1.0
      for i <- 0 until X.rows do
        val centred = if centerData then col(i) - mu else col(i)
        out(i, j) = if scaleData && sd > 0 then centred / sd else centred
    out

  def dump4x4(tag: String, m: Matrix[Double]): Unit =
    printf("%s: %d x %d\n", tag, m.rows, m.cols)
    for rnum <- 0 until scala.math.min(m.rows, 4) do
      printf("[%d,] ", rnum + 1)
      for j <- 0 until scala.math.min(m.cols, 4) do
        printf(" %9.7f", m(rnum, j))
      printf("\n")

end ThreePrf
