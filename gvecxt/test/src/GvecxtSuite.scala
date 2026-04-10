package gvecxt

import vecxt.gpu.*
import vecxt.all.{*, given}
import io.computenode.cyfra.runtime.VkCyfraRuntime

class GvecxtSuite extends munit.FunSuite:

  test("Array[Float].gpu creates a GLeaf"):
    val data = Array(1.0f, 2.0f, 3.0f)
    val g = data.gpu
    assert(g.isInstanceOf[GLeaf])
    assertEquals(g.asInstanceOf[GLeaf].data.toSeq, data.toSeq)

  test("chaining builds AST without GPU"):
    val expr = Array(1.0f).gpu.exp.sin.+(3.0f)
    // Just check it's a GScalarOp wrapping a chain — no CyfraRuntime needed
    assert(expr.isInstanceOf[GScalarOp])

  test("single unary op: neg"):
    VkCyfraRuntime.using:
      val input = Array(1.0f, -2.0f, 3.0f)
      val result = input.gpu.neg.run
      assertEquals(result.toSeq, Seq(-1.0f, 2.0f, -3.0f))

  test("single unary op: abs"):
    VkCyfraRuntime.using:
      val input = Array(-1.0f, 2.0f, -3.0f)
      val result = input.gpu.abs.run
      assertEquals(result.toSeq, Seq(1.0f, 2.0f, 3.0f))

  test("scalar add"):
    VkCyfraRuntime.using:
      val input = Array(1.0f, 2.0f, 3.0f)
      val result = input.gpu.+(10.0f).run
      assertEquals(result.toSeq, Seq(11.0f, 12.0f, 13.0f))

  test("scalar mul"):
    VkCyfraRuntime.using:
      val input = Array(1.0f, 2.0f, 3.0f)
      val result = input.gpu.*(2.0f).run
      assertEquals(result.toSeq, Seq(2.0f, 4.0f, 6.0f))

  test("fused chain: exp then scalar add"):
    VkCyfraRuntime.using:
      val input = Array(0.0f)
      val result = input.gpu.exp.+(1.0f).run
      // exp(0) + 1 = 2.0
      assertEqualsFloat(result(0), 2.0f, 1e-5f)

  test("fused chain: sin(exp(x)) + 3"):
    VkCyfraRuntime.using:
      val input = Array(0.0f, 1.0f)
      val result = input.gpu.exp.sin.+(3.0f).run
      // sin(exp(0)) + 3 = sin(1) + 3 ≈ 3.8414709
      // sin(exp(1)) + 3 = sin(e) + 3 ≈ 3.4107813
      assertEqualsFloat(result(0), Math.sin(1.0).toFloat + 3.0f, 1e-4f)
      assertEqualsFloat(result(1), Math.sin(Math.E).toFloat + 3.0f, 1e-4f)

  test("clamp"):
    VkCyfraRuntime.using:
      val input = Array(-5.0f, 0.5f, 10.0f)
      val result = input.gpu.clamp(0.0f, 1.0f).run
      assertEquals(result.toSeq, Seq(0.0f, 0.5f, 1.0f))

  test("pow via **"):
    VkCyfraRuntime.using:
      val input = Array(2.0f, 3.0f)
      val result = input.gpu.**(2.0f).run
      assertEqualsFloat(result(0), 4.0f, 1e-5f)
      assertEqualsFloat(result(1), 9.0f, 1e-4f)

  test("larger array round-trip"):
    VkCyfraRuntime.using:
      val n = 1024
      val input = Array.tabulate(n)(i => i.toFloat)
      val result = input.gpu.*(2.0f).+(1.0f).run
      var i = 0
      while i < n do
        assertEqualsFloat(result(i), input(i) * 2.0f + 1.0f, 1e-5f)
        i += 1
      end while

  private def assertEqualsFloat(actual: Float, expected: Float, eps: Float)(using munit.Location): Unit =
    assert(
      Math.abs(actual - expected) < eps,
      s"expected $expected but got $actual (diff=${Math.abs(actual - expected)}, eps=$eps)"
    )

  private def assertArrayEqualsFloat(actual: Array[Float], expected: Array[Float], eps: Float)(using
      munit.Location
  ): Unit =
    assertEquals(actual.length, expected.length, s"length mismatch: ${actual.length} vs ${expected.length}")
    var i = 0
    while i < actual.length do
      assertEqualsFloat(actual(i), expected(i), eps)
      i += 1
    end while
  end assertArrayEqualsFloat

  // ── Binary ops: GPU vs vecxt side-by-side ────────────────

  test("binary add: gpu vs vecxt"):
    VkCyfraRuntime.using:
      val a = Array(1.0f, 2.0f, 3.0f, 4.0f)
      val b = Array(10.0f, 20.0f, 30.0f, 40.0f)

      val cpuResult = a + b // vecxt
      val gpuResult = (a.gpu + b.gpu).run // gvecxt

      assertArrayEqualsFloat(gpuResult, cpuResult, 1e-5f)

  test("binary sub: gpu vs vecxt"):
    VkCyfraRuntime.using:
      val a = Array(10.0f, 20.0f, 30.0f)
      val b = Array(1.0f, 2.0f, 3.0f)

      val cpuResult = a - b
      val gpuResult = (a.gpu - b.gpu).run

      assertArrayEqualsFloat(gpuResult, cpuResult, 1e-5f)

  test("binary mul: gpu vs vecxt"):
    VkCyfraRuntime.using:
      val a = Array(2.0f, 3.0f, 4.0f)
      val b = Array(5.0f, 6.0f, 7.0f)

      val cpuResult = a * b
      val gpuResult = (a.gpu * b.gpu).run

      assertArrayEqualsFloat(gpuResult, cpuResult, 1e-5f)

  test("binary div: gpu vs vecxt"):
    VkCyfraRuntime.using:
      val a = Array(10.0f, 20.0f, 30.0f)
      val b = Array(2.0f, 4.0f, 5.0f)

      val cpuResult = a / b
      val gpuResult = (a.gpu / b.gpu).run

      assertArrayEqualsFloat(gpuResult, cpuResult, 1e-5f)

  test("mixed chain: (a.exp + b.sin) * 2.0 — gpu vs vecxt"):
    VkCyfraRuntime.using:
      val a = Array(0.0f, 0.5f, 1.0f)
      val b = Array(1.0f, 1.5f, 2.0f)

      val cpuResult = (a.exp + b.sin) * 2.0f // vecxt
      val gpuResult = ((a.gpu.exp + b.gpu.sin) * 2.0f).run

      assertArrayEqualsFloat(gpuResult, cpuResult, 1e-3f)

  test("scalar ops: gpu vs vecxt"):
    VkCyfraRuntime.using:
      val a = Array(1.0f, 2.0f, 3.0f, 4.0f)

      assertArrayEqualsFloat((a.gpu + 10.0f).run, a + 10.0f, 1e-5f)
      assertArrayEqualsFloat((a.gpu - 1.0f).run, a - 1.0f, 1e-5f)
      assertArrayEqualsFloat((a.gpu * 3.0f).run, a * 3.0f, 1e-5f)
      assertArrayEqualsFloat((a.gpu / 2.0f).run, a / 2.0f, 1e-5f)

  test("unary ops: gpu vs vecxt"):
    VkCyfraRuntime.using:
      val a = Array(0.1f, 0.5f, 1.0f, 1.5f)

      assertArrayEqualsFloat(a.gpu.exp.run, a.exp, 1e-4f)
      assertArrayEqualsFloat(a.gpu.sin.run, a.sin, 1e-4f)
      assertArrayEqualsFloat(a.gpu.cos.run, a.cos, 1e-4f)
      assertArrayEqualsFloat(a.gpu.sqrt.run, a.sqrt, 1e-4f)
      assertArrayEqualsFloat(a.gpu.log.run, a.log, 1e-4f)
      assertArrayEqualsFloat(a.gpu.abs.run, a.abs, 1e-5f)
      assertArrayEqualsFloat(a.gpu.tan.run, a.tan, 1e-4f)

  test("larger binary: gpu vs vecxt (1024 elements)"):
    VkCyfraRuntime.using:
      val n = 1024
      val a = Array.tabulate(n)(i => (i + 1).toFloat)
      val b = Array.tabulate(n)(i => (n - i).toFloat)

      val cpuResult = a + b
      val gpuResult = (a.gpu + b.gpu).run

      assertArrayEqualsFloat(gpuResult, cpuResult, 1e-5f)

  test("dimension mismatch throws"):
    VkCyfraRuntime.using:
      val a = Array(1.0f, 2.0f)
      val b = Array(1.0f, 2.0f, 3.0f)
      interceptMessage[IllegalArgumentException]("GExpr dimension mismatch: left has 2 elements, right has 3"):
        (a.gpu + b.gpu).run

  test("validateDimensions catches mismatch eagerly without GPU"):
    // No CyfraRuntime needed — shape analysis is pure CPU work
    val a = Array(1.0f, 2.0f)
    val b = Array(1.0f, 2.0f, 3.0f)
    interceptMessage[IllegalArgumentException]("GExpr dimension mismatch: left has 2 elements, right has 3"):
      (a.gpu + b.gpu).validateDimensions

  test("validateDimensions returns correct dimension"):
    val a = Array(1.0f, 2.0f, 3.0f)
    val b = Array(4.0f, 5.0f, 6.0f)
    assertEquals(a.gpu.validateDimensions, 3)
    assertEquals((a.gpu.exp.sin + 1.0f).validateDimensions, 3)
    assertEquals(a.gpu.clamp(0.0f, 2.0f).validateDimensions, 3)
    assertEquals((a.gpu + b.gpu).validateDimensions, 3)
    assertEquals(((a.gpu.exp + b.gpu.sin) * 2.0f).validateDimensions, 3)

  test("validateDimensions catches deeply nested mismatch"):
    val a = Array(1.0f, 2.0f)
    val b = Array(1.0f, 2.0f, 3.0f)
    // mismatch buried inside: (a.exp + b.sin) * 2.0
    interceptMessage[IllegalArgumentException]("GExpr dimension mismatch: left has 2 elements, right has 3"):
      ((a.gpu.exp + b.gpu.sin) * 2.0f).validateDimensions

  test("dimension mismatch caught before any GPU transfer"):
    VkCyfraRuntime.using:
      logGExprTransfers = true
      val logs = new java.io.ByteArrayOutputStream()
      val oldErr = System.err
      System.setErr(new java.io.PrintStream(logs))
      try
        val a = Array(1.0f, 2.0f)
        val b = Array(1.0f, 2.0f, 3.0f)
        interceptMessage[IllegalArgumentException]("GExpr dimension mismatch: left has 2 elements, right has 3"):
          (a.gpu + b.gpu).run
        // No transfer logs should have been emitted — error was caught in shape analysis
        val output = logs.toString
        assert(!output.contains("CPU→GPU"), s"Expected no GPU transfers but got: $output")
        assert(!output.contains("GPU→CPU"), s"Expected no GPU transfers but got: $output")
      finally
        System.setErr(oldErr)
        logGExprTransfers = false
      end try

  test("transfer log: (a.exp + b.sin) * 2.0"):
    VkCyfraRuntime.using:
      logGExprTransfers = true
      val a = Array(0.0f, 0.5f, 1.0f)
      val b = Array(1.0f, 1.5f, 2.0f)
      val _ = ((a.gpu.exp + b.gpu.sin) * 2.0f).run
      logGExprTransfers = false

  test("transfer log: a + b (simple binary)"):
    VkCyfraRuntime.using:
      logGExprTransfers = true
      val a = Array(1.0f, 2.0f, 3.0f)
      val b = Array(4.0f, 5.0f, 6.0f)
      val _ = (a.gpu + b.gpu).run
      logGExprTransfers = false

  test("transfer log: a.exp (single-input)"):
    VkCyfraRuntime.using:
      logGExprTransfers = true
      val a = Array(1.0f, 2.0f, 3.0f)
      val _ = a.gpu.exp.run
      logGExprTransfers = false

end GvecxtSuite
