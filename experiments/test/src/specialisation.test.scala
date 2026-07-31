package vecxt

import vecxt.all.*
import vecxt_io.MatrixIO.loadMatrix

/** Runtime proof that specialization survives the API, complementing the bytecode audit in
  * `specializationFailure.test.scala` (vecxt/issues/105).
  *
  * The two checks cover genuinely different failure modes, and only together do they cover both:
  *
  *   - A generic *element access* (`arr(i)` on an `Array[A]`) is contained: it boxes inside one method and the array
  *     itself stays a real `double[]`. That is what the bytecode scan finds, and it costs one slow loop.
  *   - A generic *allocation* is not contained. `new Array[A](n)` resolves through `ClassTag[A]`, and if that ClassTag
  *     ever arrives as `Any`/`AnyRef` the result is an `Object[]` of boxed values. Every downstream consumer inherits
  *     it - including the Vector API kernels - and nothing later can undo it.
  *
  * The bytecode scan deliberately does not check `ClassTag.newArray`, because it is the ordinary and legitimate way to
  * allocate a generic array; whether a given call is a problem depends on the ClassTag that reaches it, which is not
  * visible in the bytecode. So this suite checks the observable consequence instead: for every primitive element type,
  * assert the backing store is still a primitive array after each operation.
  *
  * Everything below is deliberately written out per concrete element type rather than shared through a generic helper.
  * A `def sweep[A](...)` here would itself be generic code over `Array[A]`, in a module the C6a scan covers - the test
  * would create the very findings it exists to police.
  */
class SpecialisationSuite extends munit.FunSuite:

  private def check(label: String, expected: Class[?], observed: Seq[(String, Class[?])]): Unit =
    val bad = observed.filterNot((_, cls) => cls == expected)
    assert(
      bad.isEmpty,
      s"$label: expected every backing store to stay `${expected.getName}`, but " +
        bad.map((op, cls) => s"`$op` produced ${cls.getName}").mkString("; ") +
        s"\n(a non-primitive backing store here means the data has been boxed into an Object[], which every " +
        s"downstream consumer inherits - see this suite's header)"
    )
  end check

  // ── NDArray ───────────────────────────────────────────────────────────────

  test("NDArray[Double] stays specialised across the API") {
    val base = NDArray(Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), Array(2, 3))
    val mask = NDArray(Array(true, false, true, false, true, false), Array(2, 3))
    check(
      "NDArray[Double]",
      classOf[Double],
      Seq(
        "apply(data, shape)" -> base.elementClass,
        "reshape" -> base.reshape(Array(3, 2)).elementClass,
        "T" -> base.T.elementClass,
        "transpose(perm)" -> base.transpose(Array(1, 0)).elementClass,
        "slice" -> base.slice(1, 1, 3).elementClass,
        "squeeze" -> base.reshape(Array(1, 6)).squeeze.elementClass,
        "unsqueeze" -> base.unsqueeze(0).elementClass,
        "expandDims" -> base.expandDims(0).elementClass,
        "flatten" -> base.flatten.elementClass,
        // Contiguous selectors take the zero-copy view path ...
        "apply(selectors) view" -> base(0 until 2, ::).elementClass,
        // ... a non-monotonic index array forces the copy/gather path, which is the one that was rewritten.
        "apply(selectors) gather" -> base(Array(1, 0), ::).elementClass,
        "apply(mask)" -> base(mask).elementClass,
        "broadcastTo" -> NDArray(Array(1.0, 2.0, 3.0), Array(1, 3)).broadcastTo(Array(2, 3)).elementClass,
        "toArray" -> base.toArray.getClass.getComponentType,
        "zeros" -> NDArray.zeros[Double](Array(2, 2)).elementClass,
        "ones" -> NDArray.ones[Double](Array(2, 2)).elementClass,
        "fill" -> NDArray.fill(Array(2, 2), 1.0).elementClass,
        "fromArray" -> NDArray.fromArray(Array(1.0, 2.0)).elementClass,
        "scalar" -> NDArray.scalar(1.0).elementClass
      )
    )
  }

  test("NDArray[Float] stays specialised across the API") {
    val base = NDArray(Array(1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f), Array(2, 3))
    val mask = NDArray(Array(true, false, true, false, true, false), Array(2, 3))
    check(
      "NDArray[Float]",
      classOf[Float],
      Seq(
        "apply(data, shape)" -> base.elementClass,
        "reshape" -> base.reshape(Array(3, 2)).elementClass,
        "T" -> base.T.elementClass,
        "slice" -> base.slice(1, 1, 3).elementClass,
        "flatten" -> base.flatten.elementClass,
        "apply(selectors) view" -> base(0 until 2, ::).elementClass,
        "apply(selectors) gather" -> base(Array(1, 0), ::).elementClass,
        "apply(mask)" -> base(mask).elementClass,
        "toArray" -> base.toArray.getClass.getComponentType,
        "zeros" -> NDArray.zeros[Float](Array(2, 2)).elementClass,
        "fill" -> NDArray.fill(Array(2, 2), 1.0f).elementClass,
        "scalar" -> NDArray.scalar(1.0f).elementClass
      )
    )
  }

  test("NDArray[Int] stays specialised across the API") {
    val base = NDArray(Array(1, 2, 3, 4, 5, 6), Array(2, 3))
    val mask = NDArray(Array(true, false, true, false, true, false), Array(2, 3))
    check(
      "NDArray[Int]",
      classOf[Int],
      Seq(
        "apply(data, shape)" -> base.elementClass,
        "reshape" -> base.reshape(Array(3, 2)).elementClass,
        "T" -> base.T.elementClass,
        "slice" -> base.slice(1, 1, 3).elementClass,
        "flatten" -> base.flatten.elementClass,
        "apply(selectors) view" -> base(0 until 2, ::).elementClass,
        "apply(selectors) gather" -> base(Array(1, 0), ::).elementClass,
        "apply(mask)" -> base(mask).elementClass,
        "toArray" -> base.toArray.getClass.getComponentType,
        "zeros" -> NDArray.zeros[Int](Array(2, 2)).elementClass,
        "fill" -> NDArray.fill(Array(2, 2), 1).elementClass,
        "scalar" -> NDArray.scalar(1).elementClass
      )
    )
  }

  test("NDArray[Long] stays specialised across the API") {
    val base = NDArray(Array(1L, 2L, 3L, 4L, 5L, 6L), Array(2, 3))
    check(
      "NDArray[Long]",
      classOf[Long],
      Seq(
        "apply(data, shape)" -> base.elementClass,
        "reshape" -> base.reshape(Array(3, 2)).elementClass,
        "T" -> base.T.elementClass,
        "slice" -> base.slice(1, 1, 3).elementClass,
        "flatten" -> base.flatten.elementClass,
        "apply(selectors) view" -> base(0 until 2, ::).elementClass,
        "apply(selectors) gather" -> base(Array(1, 0), ::).elementClass,
        "toArray" -> base.toArray.getClass.getComponentType,
        "zeros" -> NDArray.zeros[Long](Array(2, 2)).elementClass,
        "fill" -> NDArray.fill(Array(2, 2), 1L).elementClass
      )
    )
  }

  test("NDArray[Boolean] stays specialised across the API") {
    val base = NDArray(Array(true, false, true, false, true, false), Array(2, 3))
    check(
      "NDArray[Boolean]",
      classOf[Boolean],
      Seq(
        "apply(data, shape)" -> base.elementClass,
        "reshape" -> base.reshape(Array(3, 2)).elementClass,
        "T" -> base.T.elementClass,
        "slice" -> base.slice(1, 1, 3).elementClass,
        "flatten" -> base.flatten.elementClass,
        "apply(selectors) view" -> base(0 until 2, ::).elementClass,
        "apply(selectors) gather" -> base(Array(1, 0), ::).elementClass,
        "toArray" -> base.toArray.getClass.getComponentType,
        "zeros" -> NDArray.zeros[Boolean](Array(2, 2)).elementClass,
        "fill" -> NDArray.fill(Array(2, 2), true).elementClass
      )
    )
  }

  // ── Matrix ────────────────────────────────────────────────────────────────

  test("Matrix[Double] stays specialised across the API") {
    val m = Matrix(Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), 2, 3)
    check(
      "Matrix[Double]",
      classOf[Double],
      Seq(
        "apply(raw, rows, cols)" -> m.elementClass,
        "transpose" -> m.transpose.elementClass,
        "T" -> m.T.elementClass,
        "deepCopy" -> m.deepCopy.elementClass,
        "deepCopy(asRowMajor)" -> m.deepCopy(asRowMajor = true).elementClass,
        "submatrix" -> m.submatrix(0 until 2, 0 until 2).elementClass,
        "apply(rowRange, colRange)" -> m(0 until 2, 0 until 2).elementClass,
        "horzcat" -> m.horzcat(m).elementClass,
        "vertcat" -> m.vertcat(m).elementClass,
        "row" -> m.row(0).getClass.getComponentType,
        "col" -> m.col(0).getClass.getComponentType,
        "diag" -> m.diag.getClass.getComponentType,
        // `fill` rather than `zeros`/`ones` only because it has a single unambiguous using clause; the
        // allocation path being covered is the same `Array.fill[A]` that zeros and ones both go through.
        "fill" -> Matrix.fill(0.0, (2, 2)).elementClass
      )
    )
  }

  test("Matrix[Int] stays specialised across the API") {
    val m = Matrix(Array(1, 2, 3, 4, 5, 6), 2, 3)
    check(
      "Matrix[Int]",
      classOf[Int],
      Seq(
        "apply(raw, rows, cols)" -> m.elementClass,
        "transpose" -> m.transpose.elementClass,
        "deepCopy" -> m.deepCopy.elementClass,
        "submatrix" -> m.submatrix(0 until 2, 0 until 2).elementClass,
        "horzcat" -> m.horzcat(m).elementClass,
        "vertcat" -> m.vertcat(m).elementClass,
        "row" -> m.row(0).getClass.getComponentType,
        "col" -> m.col(0).getClass.getComponentType,
        "diag" -> m.diag.getClass.getComponentType,
        "fill" -> Matrix.fill(0, (2, 2)).elementClass
      )
    )
  }

  test("Matrix[Boolean] stays specialised across the API") {
    val m = Matrix(Array(true, false, true, false, true, false), 2, 3)
    check(
      "Matrix[Boolean]",
      classOf[Boolean],
      Seq(
        "apply(raw, rows, cols)" -> m.elementClass,
        "transpose" -> m.transpose.elementClass,
        "deepCopy" -> m.deepCopy.elementClass,
        "submatrix" -> m.submatrix(0 until 2, 0 until 2).elementClass,
        "row" -> m.row(0).getClass.getComponentType,
        "col" -> m.col(0).getClass.getComponentType,
        "fill" -> Matrix.fill(true, (2, 2)).elementClass
      )
    )
  }

  // ── vecxt_io ──────────────────────────────────────────────────────────────

  test("vecxt_io does not hand back a boxed matrix") {
    // The reason this is worth asserting rather than assuming: `loadMatrix` is generic and allocates via
    // `new Array[A]`, so it is exactly the shape that *could* leak an Object[] downstream. It does not, because
    // the `Numeric` context bound makes `A = Any` unsatisfiable - there is no `Numeric[Any]` - so every call
    // site pins A to a concrete numeric type and the ClassTag that reaches the allocation is a real one.
    // Being slow while parsing is fine; handing back a boxed array would not be.
    val csv = os.temp("1.0,2.0\n3.0,4.0", suffix = ".csv")
    val doubles = loadMatrix[Double](csv)
    assertEquals(doubles.elementClass, classOf[Double], "loadMatrix[Double] backing store")

    val intCsv = os.temp("1,2\n3,4", suffix = ".csv")
    val ints = loadMatrix[Int](intCsv)
    assertEquals(ints.elementClass, classOf[Int], "loadMatrix[Int] backing store")
  }

  // ── the negative control ──────────────────────────────────────────────────

  test("control: a reference element type is genuinely NOT primitive") {
    // Guards the assertions above from being vacuous. If `elementClass` ever started reporting something
    // constant, or the primitive comparisons silently began passing for everything, this notices.
    val strings = NDArray(Array("a", "b", "c", "d"), Array(2, 2))
    assertEquals(strings.elementClass, classOf[String])
    assert(!strings.elementClass.isPrimitive, "Array[String] must not report a primitive element type")
    assert(classOf[Double].isPrimitive, "classOf[Double] should be the primitive double, not java.lang.Double")
  }

end SpecialisationSuite
