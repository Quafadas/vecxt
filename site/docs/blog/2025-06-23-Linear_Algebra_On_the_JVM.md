# Linear Algebra and `Array[Double]` on the JVM. Beware All Ye Who Enter Here.

## Part I

Let's do some linear algebra on the JVM. `Array[Double]` seems like a good data structure. It's fast, and has contiguous memory layout which means we can do things like SIMD. Writing out these algorithms is fun!

For the sake of argument, let's accept `Array[Double]` as is as, our 1-D Tensor. For our 2D Tensor (aka matrix), it could be an `Array[Array[Double]]`, but this gives up some quite attractive looking performance benefits, and possibly that (cache friendly 🚀) contiguous memory layout. Instead, we'll go with something like

```scala sc:nocompile
class Matrix[A](val raw: Array[A], val rows: Row, val cols: Col)
```
In the end, it doesn't matter if it's a class, Tuple, opaque type or whatever. The point, is a single `Array[A] `, with some information to navigate it. Our assumption is that our raw array is densely laid out in column major order. i.e. something like a 2x2 matrix looks like;

```scala sc:nocompile
val mat1 = Matrix.fromRows[Double](
  NArray(1.0, 2.0),
  NArray(3.0, 4.0)
)

// In memory, column major:
// mat.raw === Array(1.0, 3.0, 2.0, 4.0)

```

Let's assume we can inline our way of the generic boxing problem and retain good performance characteristics for the specific case of `Array[Double]`. Performance is pretty solid for many simple operations - addition, subtraction, elementwise operations etc all hit hardware acceleration and we can benchmark some pretty sweet performance characteristics (we're ignoring for now the existence of GPU's, TPU's and whatever other whizz-bang PU's they have these days).

The data structure looks good... and actually it works really well, performs great until you hit the obvious implementation of transpose.

```scala sc:nocompile
inline def transpose: Matrix[Double] =
  val newArr = Array.ofSize[Double](m.numel)
  val newMat = Matrix(newArr, (m.cols, m.rows))
  var idx = 0

  while idx < newMat.numel do
    newMat(row, col) = m(col, row)
    idx += 1
  end while
  newMat

```
Can you see the problem yet? If you are (like me) doing this out of a misplaced sense of curiosity at small scale then actually, there isn't one! It's fine! Just **allocate** - oh look, a hot air balloon! - a new array and copy data over  it's cheap and JVM is quick.

Now, you take your hobby project and decide to see what [training a neural network](https://github.com/Quafadas/vecxt/blob/243b562ec2a5901c929e5b7ba3d296f7f907915f/experiments/src/mnist.scala) looks like. The MNIST datset seems to be fairly standard machine leanring rite of passage. It contains about 60,000 small images. So 60,000 * 27 * 27 (4.4m) doubles and we need to do matrix multipliation. My fag packet claims that we'll need about 4.4e6 * 8 (bytes) which is about 33.5MB of memory. Well within the capacity of a modern JVM / hardware. So ... no problem? We have a nice [hardware accelerated](https://github.com/luhenry/netlib) matrix multiplication library uses SIMD. Booyakashah!

And then we hit backpropogation. We check in with our friendly neighbourhood LLM - Let A be an m x n matrix, B be an n x p matrix, and C = AB be the resulting m x p matrix.

Partial derivative of C with respect to A:
∂C / ∂A = transpose(B)

Component-wise:

∂C_ij / ∂A_kl = δ_ik * B_lj

Partial derivative of C with respect to B:
∂C / ∂B = transpose(A)

Component-wise:
∂C_ij / ∂B_kl = A_ik * δ_jl

Probably your eyes glazed over. TL;DR: We need a transpose. FYI, that transpose is in the "hot" loop of our training run. Now, we're allocating 35Mb of memory per iteration in our training run. Fuuuuuuck.

So, the performance implications of our simple datastructure are apocalyptic at anything beyond toy scale.

We could _probably_ cleverise our way out some it userside by doing some of the transpose in advance and leaving some commentary. But some parts not. Further the "joy" of using your hobby project, starts to deteriorate along with it's performance.

And if you start trying to abstract over the automatic differentation part? Not gonna happen without solving this problem properly.

## Part II

So okay... I'm totally a sucker for punishment. We need zero copy transpose - how hard can it be? Let's see how deep this rabbit hole goes. In conversation with ChatGPT, we can update our `Matrix` to, instead of just having rows and column, have an offset, row and column strides.

```scala sc:nocompile
  class Matrix[A] (
      val raw: Array[A],
      val rows: Row,
      val cols: Col,
      val rowStride: Int,
      val colStride: Int,
      val offset: Int = 0
  )
```
And now, we have zero copy transpose, sub-matrix, and certain subsets of slicing... but either a _lot_ more complexity in the indexing parts of the algorithms, or we give up SIMD.

Gamely, we plow on. Implementing simple algorithms like `+` for non-contiguous memory layouts using SIMD is ... more fun.

It can work. We can make dgemm (matrix multiplication) netlib BLAS work for dense matricies with a contiguous memory layout.

And we're back to trying to train our neural network. Our zero transpose works well and naively, we're still working with all 60,000 imagines in each iteration. Observations of the training diagnostics and, again, some conversations with chatGPT yield the knowledge that training works better in "small batches". We should split our images into "batches" of 120 or so, and an "epoch" outer loops, and a "batch" inner loop.

We have zero copy sub-matricies so no problem... but our zero copy matricies have an offset. netlib BLAS, as far as I can tell, supports offset, but it _doesn't_ support offsets with the flexible "rowStride", "colStride" memory layout. As far as I can tell, it assumes a dense layout beyond that point, which means the results are, well... wrong.

And at this point, we're bust. Options;

1. Hard copy our sub-matrix into a contiguous memory layout and continue with netlib BLAS.
2. Implement our own matrix multiplication algorithm that works with a non-contiguous memory layout.
3. Go native and use a library that supports non-contiguous memory layouts (e.g. [BLIS](https://github.com/flame/blis))

I'm discarding 2. I don't consider myself capable of implementing a good matrix multiplication algorithm.

Option 1 is distasteful, we've jus expended significant effort trying exactly to wriggle _out_ of needless memory allocation (due to the computational overhead).

Option 3 is more interesting. Project panama and jextract actually get you pretty close to calling BLIS quite quickly. But when you do, you realise that you can't just wrap an `Array[Double]` in a memory segment as far as I'm aware. We have to _copy_ the data in, call BLIS, and _copy_ it back out!

# Conclusion
I believe this strikes to the heart of why the JVM has seen little adoption in the scientific computing space. Essentially, `Array[Double]` leads you inevitably to a catastrophic allocation strategy (in the absence of a modern hardware accelerated BLAS+  library). This seems a vicious circle, it's non-existence appears increasingly unlikely, that someone capable of such a library, will spend their time on the JVM.

I believe this to be quite fundamental - I don't think it can be easily worked around as a "user" of the JVM.

To summarise the argument:

- Zero-copy operations require flexible memory layout
- There's no hardware accelerated BLAS-like library that supports flexible memory layout on the JVM (that I know of).
- Which means Level 3 BLAS operations slow...
- The alternatives I've explored lead to aggressive memory allocation strategies which are terminal for performance beyond toy scale.

In this context, Oracle's Project Panama appears critical to any future of scientific computing on the JVM. Speculatively, given that the Vector API has converged with `MemorySegment`, suggests to me that Oracle themselves, are not seeing a way out of this hole in a backwards compatible manner.

[`MemorySegment`](https://download.java.net/java/early_access/loom/docs/api/java.base/java/lang/foreign/MemorySegment.html) appears to offer;

- a C compatible memory layout
- SIMD compatibility.
- in combination with jextract, native interop with modern libraries such as BLIS

Perhaps, it is the future.

_BUT_

It does _not_ provide C interop with `Array[Double]` (as far as I'm aware), outside of copy-allocation. To gain C interop, it's must be allocated "off-heap".

| Memory type                    | SIMD-accelerated | C-compatible |
|--------------------------------|------------------|--------------|
| `double[]`                     | ✅ Yes           | ❌ No        |
| `MemorySegment.allocate(...)`  | ✅ Yes           | ✅ Yes       |
| `MemorySegment.ofArray(...)`   | ❌ No            | ❌ No        |

This restriction means that a zero-copy suite of operations (so crucial to allocation avoidance) would need to be built, from the gronud up.

## Update

Fixed this bug in netlib-java, so offsets with non-contiguous memory layouts now work. So we can do zero copy sub-matricies with netlib BLAS.

https://github.com/luhenry/netlib/issues/23
