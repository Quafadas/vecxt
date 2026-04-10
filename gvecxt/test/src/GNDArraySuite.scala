package gvecxt

import vecxt.gpu.*
import io.computenode.cyfra.runtime.VkCyfraRuntime

class GNDArraySuite extends munit.FunSuite:

  // ── Construction ─────────────────────────────────────────

  test("GNDArray.fromArray creates 1D leaf"):
    val leaf = GNDArray.fromArray(Array(1.0f, 2.0f, 3.0f))
    assertEquals(leaf.shape.toSeq, Seq(1, 2, 3).map(_.toInt).take(0) ++ Seq(3))
    assertEquals(leaf.shape.toSeq, Seq(3))
    assertEquals(leaf.strides.toSeq, Seq(1))
    assertEquals(leaf.offset, 0)

  test("GNDArray.matrix creates 2D col-major leaf"):
    // 2×3 matrix, col-major: stride = [1, 2]
    val leaf = GNDArray.matrix(Array.ofDim[Float](6), 2, 3)
    assertEquals(leaf.shape.toSeq, Seq(2, 3))
    assertEquals(leaf.strides.toSeq, Seq(1, 2))
    assertEquals(leaf.offset, 0)

  test("GNDArray 3D col-major strides"):
    val leaf = GNDArray(Array.ofDim[Float](24), Array(2, 3, 4))
    assertEquals(leaf.shape.toSeq, Seq(2, 3, 4))
    assertEquals(leaf.strides.toSeq, Seq(1, 2, 6))

  test("GNDArray custom strides (row-major 2×3)"):
    // Row-major: stride = [3, 1]
    val leaf = GNDArray(Array.ofDim[Float](6), Array(2, 3), Array(3, 1))
    assertEquals(leaf.shape.toSeq, Seq(2, 3))
    assertEquals(leaf.strides.toSeq, Seq(3, 1))

  test("GNDArray with offset"):
    // Strided view starting at offset 2
    val data = Array.ofDim[Float](10)
    val leaf = GNDArray(data, Array(2, 3), Array(1, 2), offset = 2)
    assertEquals(leaf.offset, 2)

  test("GNDArray rejects shape/strides rank mismatch"):
    interceptMessage[IllegalArgumentException]("GNDArray: shape rank 2 != strides rank 1"):
      GNDArray(Array.ofDim[Float](6), Array(2, 3), Array(1))

  test("GNDArray rejects out-of-bounds view"):
    interceptMessage[IllegalArgumentException](
      "GNDArray: max reachable index 5 >= data.length 4 (shape=[2,3], strides=[1,2], offset=0)"
    ):
      GNDArray(Array.ofDim[Float](4), Array(2, 3), Array(1, 2))

  test("GNDArray rejects negative offset"):
    interceptMessage[IllegalArgumentException]("GNDArray: negative offset -1"):
      GNDArray(Array.ofDim[Float](6), Array(2, 3), Array(1, 2), offset = -1)

  test("GNDArray rejects negative dimensions"):
    interceptMessage[IllegalArgumentException]("GNDArray: shape has negative dimension: [2,-3]"):
      GNDArray(Array.ofDim[Float](6), Array(2, -3))

  // ── Shape analysis: unary / scalar / clamp ───────────────

  test("validateShape: unary preserves shape"):
    val a = GNDArray.matrix(Array.ofDim[Float](6), 2, 3)
    assertEquals(a.exp.validateShape.toSeq, Seq(2, 3))
    assertEquals(a.neg.sin.abs.validateShape.toSeq, Seq(2, 3))

  test("validateShape: scalar preserves shape"):
    val a = GNDArray.matrix(Array.ofDim[Float](6), 2, 3)
    assertEquals((a + 1.0f).validateShape.toSeq, Seq(2, 3))
    assertEquals((a * 2.0f).validateShape.toSeq, Seq(2, 3))

  test("validateShape: clamp preserves shape"):
    val a = GNDArray(Array.ofDim[Float](24), Array(2, 3, 4))
    assertEquals(a.clamp(0.0f, 1.0f).validateShape.toSeq, Seq(2, 3, 4))

  test("validateShape: chain preserves shape"):
    val a = GNDArray.matrix(Array.ofDim[Float](6), 2, 3)
    assertEquals(a.exp.sin.+(3.0f).clamp(0.0f, 10.0f).validateShape.toSeq, Seq(2, 3))

  // ── Shape analysis: explicit broadcasting ────────────────

  test("validateShape: same-shape binary"):
    val a = GNDArray.matrix(Array.ofDim[Float](6), 2, 3)
    val b = GNDArray.matrix(Array.ofDim[Float](6), 2, 3)
    assertEquals((a + b).validateShape.toSeq, Seq(2, 3))

  test("validateShape: broadcastTo [1,3] → [2,3] then binary"):
    val a = GNDArray.matrix(Array.ofDim[Float](6), 2, 3)
    val b = GNDArray.matrix(Array.ofDim[Float](3), 1, 3)
    assertEquals((a + b.broadcastTo(Array(2, 3))).validateShape.toSeq, Seq(2, 3))

  test("validateShape: broadcastTo both sides [2,1] + [1,3] → [2,3]"):
    val a = GNDArray.matrix(Array.ofDim[Float](2), 2, 1)
    val b = GNDArray.matrix(Array.ofDim[Float](3), 1, 3)
    val target = Array(2, 3)
    assertEquals((a.broadcastTo(target) + b.broadcastTo(target)).validateShape.toSeq, Seq(2, 3))

  test("validateShape: broadcastTo rank extension [3] → [2,3]"):
    val a = GNDArray.fromArray(Array.ofDim[Float](3))
    val b = GNDArray.matrix(Array.ofDim[Float](6), 2, 3)
    assertEquals((a.broadcastTo(Array(2, 3)) + b).validateShape.toSeq, Seq(2, 3))

  test("validateShape: broadcastTo [5,1,4] → [5,3,4]"):
    val a = GNDArray(Array.ofDim[Float](20), Array(5, 1, 4))
    assertEquals(a.broadcastTo(Array(5, 3, 4)).validateShape.toSeq, Seq(5, 3, 4))

  test("validateShape: broadcastTo [1,3,1] → [5,3,4]"):
    val b = GNDArray(Array.ofDim[Float](3), Array(1, 3, 1))
    assertEquals(b.broadcastTo(Array(5, 3, 4)).validateShape.toSeq, Seq(5, 3, 4))

  test("validateShape: broadcastTo incompatible [2,3] → [2,4] throws"):
    val a = GNDArray.matrix(Array.ofDim[Float](6), 2, 3)
    interceptMessage[IllegalArgumentException](
      "GNDExpr broadcast: cannot broadcast [2,3] to [2,4] \u2014 incompatible at dimension 1 (3 vs 4)"
    ):
      a.broadcastTo(Array(2, 4)).validateShape

  test("validateShape: broadcastTo more dims in source throws"):
    val a = GNDArray.matrix(Array.ofDim[Float](6), 2, 3)
    interceptMessage[IllegalArgumentException](
      "GNDExpr broadcast: cannot broadcast [2,3] to [6] \u2014 source has more dimensions (2) than target (1)"
    ):
      a.broadcastTo(Array(6)).validateShape

  test("validateShape: binary without broadcast rejects mismatched shapes"):
    val a = GNDArray.matrix(Array.ofDim[Float](6), 2, 3)
    val b = GNDArray.matrix(Array.ofDim[Float](8), 2, 4)
    interceptMessage[IllegalArgumentException](
      "GNDExpr binary: shape mismatch \u2014 [2,3] vs [2,4] at dimension 1 (3 vs 4). Use .broadcastTo to align shapes explicitly."
    ):
      (a + b).validateShape

  test("validateShape: binary without broadcast rejects mismatched rank"):
    val a = GNDArray.fromArray(Array.ofDim[Float](3))
    val b = GNDArray.matrix(Array.ofDim[Float](6), 2, 3)
    interceptMessage[IllegalArgumentException](
      "GNDExpr binary: shape mismatch \u2014 [3] vs [2,3] (rank 1 != 2). Use .broadcastTo to align shapes explicitly."
    ):
      (a + b).validateShape

  // ── Shape analysis: matmul ───────────────────────────────

  test("validateShape: matmul 2D×2D [2,3] @@ [3,4] → [2,4]"):
    val a = GNDArray.matrix(Array.ofDim[Float](6), 2, 3)
    val b = GNDArray.matrix(Array.ofDim[Float](12), 3, 4)
    assertEquals((a @@ b).validateShape.toSeq, Seq(2, 4))

  test("validateShape: matmul square [3,3] @@ [3,3] → [3,3]"):
    val a = GNDArray.matrix(Array.ofDim[Float](9), 3, 3)
    val b = GNDArray.matrix(Array.ofDim[Float](9), 3, 3)
    assertEquals((a @@ b).validateShape.toSeq, Seq(3, 3))

  test("validateShape: matmul vector-matrix [3] @@ [3,4] → [4]"):
    val a = GNDArray.fromArray(Array.ofDim[Float](3))
    val b = GNDArray.matrix(Array.ofDim[Float](12), 3, 4)
    assertEquals((a @@ b).validateShape.toSeq, Seq(4))

  test("validateShape: matmul matrix-vector [2,3] @@ [3] → [2]"):
    val a = GNDArray.matrix(Array.ofDim[Float](6), 2, 3)
    val b = GNDArray.fromArray(Array.ofDim[Float](3))
    assertEquals((a @@ b).validateShape.toSeq, Seq(2))

  test("validateShape: matmul dot product [3] @@ [3] → []"):
    val a = GNDArray.fromArray(Array.ofDim[Float](3))
    val b = GNDArray.fromArray(Array.ofDim[Float](3))
    assertEquals((a @@ b).validateShape.toSeq, Seq.empty)

  test("validateShape: matmul inner dim mismatch [2,3] @@ [4,5]"):
    val a = GNDArray.matrix(Array.ofDim[Float](6), 2, 3)
    val b = GNDArray.matrix(Array.ofDim[Float](20), 4, 5)
    interceptMessage[IllegalArgumentException](
      "GNDExpr matmul: inner dimensions don't match \u2014 [2,3] @@ [4,5]: 3 != 4"
    ):
      (a @@ b).validateShape

  test("validateShape: batched matmul [2,3,4] @@ [2,4,5] → [2,3,5]"):
    val a = GNDArray(Array.ofDim[Float](24), Array(2, 3, 4))
    val b = GNDArray(Array.ofDim[Float](40), Array(2, 4, 5))
    assertEquals((a @@ b).validateShape.toSeq, Seq(2, 3, 5))

  test("validateShape: batched matmul with broadcast [1,3,4] @@ [2,4,5] → [2,3,5]"):
    val a = GNDArray(Array.ofDim[Float](12), Array(1, 3, 4))
    val b = GNDArray(Array.ofDim[Float](40), Array(2, 4, 5))
    assertEquals((a @@ b).validateShape.toSeq, Seq(2, 3, 5))

  test("validateShape: batched matmul batch mismatch [3,2,4] @@ [2,4,5]"):
    val a = GNDArray(Array.ofDim[Float](24), Array(3, 2, 4))
    val b = GNDArray(Array.ofDim[Float](40), Array(2, 4, 5))
    interceptMessage[IllegalArgumentException](
      "GNDExpr broadcast: incompatible shapes [3] and [2] at dimension 0 (3 vs 2)"
    ):
      (a @@ b).validateShape

  // ── Shape analysis: reshape ──────────────────────────────

  test("validateShape: reshape [2,3] → [3,2]"):
    val a = GNDArray.matrix(Array.ofDim[Float](6), 2, 3)
    assertEquals(a.reshape(Array(3, 2)).validateShape.toSeq, Seq(3, 2))

  test("validateShape: reshape [2,3] → [6]"):
    val a = GNDArray.matrix(Array.ofDim[Float](6), 2, 3)
    assertEquals(a.reshape(Array(6)).validateShape.toSeq, Seq(6))

  test("validateShape: reshape [6] → [2,3]"):
    val a = GNDArray.fromArray(Array.ofDim[Float](6))
    assertEquals(a.reshape(Array(2, 3)).validateShape.toSeq, Seq(2, 3))

  test("validateShape: reshape numel mismatch [2,3] → [2,4]"):
    val a = GNDArray.matrix(Array.ofDim[Float](6), 2, 3)
    interceptMessage[IllegalArgumentException](
      "GNDExpr reshape: cannot reshape [2,3] (6 elements) to [2,4] (8 elements)"
    ):
      a.reshape(Array(2, 4)).validateShape

  // ── Shape analysis: transpose ────────────────────────────

  test("validateShape: transpose [2,3] → [3,2]"):
    val a = GNDArray.matrix(Array.ofDim[Float](6), 2, 3)
    assertEquals(a.T.validateShape.toSeq, Seq(3, 2))

  test("validateShape: transpose [2,3,4] → [4,3,2]"):
    val a = GNDArray(Array.ofDim[Float](24), Array(2, 3, 4))
    assertEquals(a.T.validateShape.toSeq, Seq(4, 3, 2))

  test("validateShape: transpose 1D throws"):
    val a = GNDArray.fromArray(Array.ofDim[Float](3))
    interceptMessage[IllegalArgumentException]("GNDExpr transpose requires at least 2 dimensions, got [3]"):
      a.T.validateShape

  // ── Shape analysis: composite expressions ────────────────

  test("validateShape: (A @@ B).exp + C with explicit broadcast"):
    // A: [2,3], B: [3,4] → matmul → [2,4]
    // C: [1,4] → broadcastTo [2,4] → [2,4]
    val a = GNDArray.matrix(Array.ofDim[Float](6), 2, 3)
    val b = GNDArray.matrix(Array.ofDim[Float](12), 3, 4)
    val c = GNDArray.matrix(Array.ofDim[Float](4), 1, 4)
    assertEquals(((a @@ b).exp + c.broadcastTo(Array(2, 4))).validateShape.toSeq, Seq(2, 4))

  test("validateShape: (A.T @@ B) * 2.0"):
    // A: [3,2] → T → [2,3], B: [3,4] → matmul → [2,4]
    val a = GNDArray.matrix(Array.ofDim[Float](6), 3, 2)
    val b = GNDArray.matrix(Array.ofDim[Float](12), 3, 4)
    assertEquals(((a.T @@ b) * 2.0f).validateShape.toSeq, Seq(2, 4))

  test("validateShape: A.reshape([3,2]) @@ B"):
    // A: [6], reshape → [3,2], B: [2,4] → matmul → [3,4]
    val a = GNDArray.fromArray(Array.ofDim[Float](6))
    val b = GNDArray.matrix(Array.ofDim[Float](8), 2, 4)
    assertEquals((a.reshape(Array(3, 2)) @@ b).validateShape.toSeq, Seq(3, 4))

  test("validateShape: complex chain catches inner mismatch"):
    // (A @@ B).exp + C where C is wrong shape — binary requires exact match
    // A: [2,3], B: [3,4] → [2,4]. C: [2,3] → [2,4] + [2,3] fails
    val a = GNDArray.matrix(Array.ofDim[Float](6), 2, 3)
    val b = GNDArray.matrix(Array.ofDim[Float](12), 3, 4)
    val c = GNDArray.matrix(Array.ofDim[Float](6), 2, 3)
    interceptMessage[IllegalArgumentException](
      "GNDExpr binary: shape mismatch \u2014 [2,4] vs [2,3] at dimension 1 (4 vs 3). Use .broadcastTo to align shapes explicitly."
    ):
      ((a @@ b).exp + c).validateShape

  test("validateShape: deep chain with multiple matmuls"):
    // A: [2,3], B: [3,4], C: [4,5]
    // (A @@ B) @@ C → [2,4] @@ [4,5] → [2,5]
    val a = GNDArray.matrix(Array.ofDim[Float](6), 2, 3)
    val b = GNDArray.matrix(Array.ofDim[Float](12), 3, 4)
    val c = GNDArray.matrix(Array.ofDim[Float](20), 4, 5)
    assertEquals(((a @@ b) @@ c).validateShape.toSeq, Seq(2, 5))

  // ── GPU dispatch: elementwise run ────────────────────────

  test("run: 1D unary neg"):
    VkCyfraRuntime.using:
      val a = GNDArray.fromArray(Array(1.0f, -2.0f, 3.0f))
      val result = a.neg.run
      assertEquals(result.data.toSeq, Seq(-1.0f, 2.0f, -3.0f))
      assertEquals(result.shape.toSeq, Seq(3))

  test("run: 2D scalar add"):
    VkCyfraRuntime.using:
      val a = GNDArray.matrix(Array(1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f), 2, 3)
      val result = (a + 10.0f).run
      assertEquals(result.data.toSeq, Seq(11.0f, 12.0f, 13.0f, 14.0f, 15.0f, 16.0f))
      assertEquals(result.shape.toSeq, Seq(2, 3))
      assertEquals(result.strides.toSeq, Seq(1, 2)) // col-major

  test("run: 2D binary add same shape"):
    VkCyfraRuntime.using:
      val a = GNDArray.matrix(Array(1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f), 2, 3)
      val b = GNDArray.matrix(Array(10.0f, 20.0f, 30.0f, 40.0f, 50.0f, 60.0f), 2, 3)
      val result = (a + b).run
      assertEquals(result.data.toSeq, Seq(11.0f, 22.0f, 33.0f, 44.0f, 55.0f, 66.0f))
      assertEquals(result.shape.toSeq, Seq(2, 3))

  test("run: 2D binary sub"):
    VkCyfraRuntime.using:
      val a = GNDArray.matrix(Array(10.0f, 20.0f, 30.0f, 40.0f), 2, 2)
      val b = GNDArray.matrix(Array(1.0f, 2.0f, 3.0f, 4.0f), 2, 2)
      val result = (a - b).run
      assertEquals(result.data.toSeq, Seq(9.0f, 18.0f, 27.0f, 36.0f))

  test("run: 2D binary mul"):
    VkCyfraRuntime.using:
      val a = GNDArray.matrix(Array(2.0f, 3.0f, 4.0f, 5.0f), 2, 2)
      val b = GNDArray.matrix(Array(10.0f, 10.0f, 10.0f, 10.0f), 2, 2)
      val result = (a * b).run
      assertEquals(result.data.toSeq, Seq(20.0f, 30.0f, 40.0f, 50.0f))

  test("run: fused chain exp then scalar add on 2D"):
    VkCyfraRuntime.using:
      val a = GNDArray.matrix(Array(0.0f, 0.0f, 0.0f, 0.0f), 2, 2)
      val result = (a.exp + 1.0f).run
      // exp(0) + 1 = 2.0 for every element
      result.data.foreach: v =>
        assert(math.abs(v - 2.0f) < 1e-5f, s"expected ~2.0, got $v")
      assertEquals(result.shape.toSeq, Seq(2, 2))

  test("run: 3D elementwise add"):
    VkCyfraRuntime.using:
      val data = Array.tabulate(24)(_.toFloat)
      val a = GNDArray(data, Array(2, 3, 4))
      val b = GNDArray(Array.fill(24)(1.0f), Array(2, 3, 4))
      val result = (a + b).run
      assertEquals(result.shape.toSeq, Seq(2, 3, 4))
      val expected = data.map(_ + 1.0f)
      assertEquals(result.data.toSeq, expected.toSeq)

  test("run: reshape then unary (contiguous passthrough)"):
    VkCyfraRuntime.using:
      val a = GNDArray.fromArray(Array(1.0f, 4.0f, 9.0f, 16.0f, 25.0f, 36.0f))
      val result = a.reshape(Array(2, 3)).sqrt.run
      assertEquals(result.shape.toSeq, Seq(2, 3))
      assertEquals(result.data.toSeq, Seq(1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f))

  test("run: clamp on 2D"):
    VkCyfraRuntime.using:
      val a = GNDArray.matrix(Array(-5.0f, 0.5f, 3.0f, 10.0f), 2, 2)
      val result = a.clamp(0.0f, 2.0f).run
      assertEquals(result.data.toSeq, Seq(0.0f, 0.5f, 2.0f, 2.0f))

  // ── GPU dispatch: error cases ────────────────────────────

  test("run: non-contiguous (row-major) leaf throws"):
    VkCyfraRuntime.using:
      val a = GNDArray(Array.ofDim[Float](6), Array(2, 3), Array(3, 1))
      interceptMessage[UnsupportedOperationException](
        "GNDExpr lowering requires contiguous col-major strides [1,2], got [3,1]"
      ):
        a.neg.run

  test("run: leaf with offset throws"):
    VkCyfraRuntime.using:
      val a = GNDArray(Array.ofDim[Float](10), Array(2, 3), Array(1, 2), offset = 2)
      interceptMessage[UnsupportedOperationException](
        "GNDExpr lowering requires offset=0, got 2"
      ):
        a.neg.run

  test("run: broadcast throws"):
    VkCyfraRuntime.using:
      val a = GNDArray.matrix(Array.ofDim[Float](3), 1, 3)
      interceptMessage[UnsupportedOperationException](
        "GNDExpr broadcast lowering to GExpr is not yet supported"
      ):
        a.broadcastTo(Array(2, 3)).run

  test("run: matmul throws"):
    VkCyfraRuntime.using:
      val a = GNDArray.matrix(Array.ofDim[Float](6), 2, 3)
      val b = GNDArray.matrix(Array.ofDim[Float](12), 3, 4)
      interceptMessage[UnsupportedOperationException](
        "GNDExpr matmul lowering to GExpr is not yet supported"
      ):
        (a @@ b).run

  test("run: transpose throws"):
    VkCyfraRuntime.using:
      val a = GNDArray.matrix(Array.ofDim[Float](6), 2, 3)
      interceptMessage[UnsupportedOperationException](
        "GNDExpr transpose lowering to GExpr is not yet supported \u2014 non-contiguous layout"
      ):
        a.T.run

  test("validateShape: matmul then transpose"):
    // A: [2,3], B: [3,4] → [2,4] → T → [4,2]
    val a = GNDArray.matrix(Array.ofDim[Float](6), 2, 3)
    val b = GNDArray.matrix(Array.ofDim[Float](12), 3, 4)
    assertEquals((a @@ b).T.validateShape.toSeq, Seq(4, 2))

end GNDArraySuite
