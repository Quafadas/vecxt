---
title:  Linear Algebra and Array[Double] on the JVM. Beware All Ye Who Enter Here.
---

## Part I

Let's do some linear algebra on the JVM. `Array[Double]` seems like a good data structure. It's fast, and has contiguous memory layout which means we can do things like SIMD. Writing out these algorithms is fun!

For the sake of argument, let's accept `Array[Double]` as is as, our 1-D Tensor. For our 2D Tensor, it could be an `Array[Array[Double]]`, but this gives up some quite attractive looking performance benefits, and possibly that (cache friendly 🚀) contiguous memory layout. Instead, we'll go with something like

```scala sc:no-compile
class Matrix[A](val raw: Array[A], val rows: Row, val cols: Col)
```
In the end, it doesn't matter if it's a class, Tuple, opaque type or whatever. The point, is that `Array[A] `, although with some information to navigate it.

Our assumption is that our raw array is densely laid out in column major order. i.e. something like a 2x2 eye looks like;

```scala sc:no-compile
val mat1 = Matrix.eye[Double](2)

// mat.raw === Array(1.0, 0.0, 0.0, 1.0)
// i.e. with implied coordinates
// ((0,0), (0,1) (1,0) (1,1) )
```
underneath.

Let's assume we can inline our way of the generic boxing problem and retain good performance characteristics for the specific case of `Array[Double]`. Performance is pretty solid for many simple operations - addition, subtraction, elementwise operations etc all hit hardware acceleration and we can fairly easily benchmark pretty sweet performance characteristics (we're ignoring for now the existence of GPU's, TPU's and whatever othey whizz-bang PU's they have these days).

The data structure looks good... and actually it works really well, performs great until you hit the obvious implementation of transpose.

```scala sc:no-compile
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
Can you see the problem yet? If you are (like me) doing this out of a misplaced sense of curiosity at small scale then actually, there isn't one! It's fine! Just **allocate** a new array and copy data over - oh look, a hot air balloon! - it's cheap and JVM is quick.

Now, you take your hobby project and decide to see what [training a neural network](https://github.com/Quafadas/vecxt/blob/243b562ec2a5901c929e5b7ba3d296f7f907915f/experiments/src/mnist.scala) looks like. The MNIST datset seems to be fairly standard machine leanring rite of passage. It contains about 60,000 small image. So 60,000 * 27 * 27 (4.4m) doubles and we need to do matrix multipliation. My fag packet claims that we'll need about 4.4e6 * 8 (bytes) which is about 33.5MB of memory. Well within the capacity of a modern JVM / hardware. So ... no problem? We have a nice [hardware accelerated](https://github.com/luhenry/netlib) matrix multiplication library uses SIMD. Booyakashah!

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

We could _probably_ cleverise our way out some it userside by doing some of the transpose in advance and leaving some commentary. But some parts not. Further the "joy" of using your hobby project, starts to deteriorate rapidly around this point. And if you start trying to abstract over the automatic differentation part? Screwed.

## Part II

So okay... I'm totally a sucker for punishment. We need zero copy transpose - how hard can it be? Let's see how deep this rabbit hole goes. In conversation with ChatGPT, we can update our `Matrix` to, instead of just having rows and column, have an offset, row and column strides.

```scala sc:no-compile
  class Matrix[@specialized(Double, Boolean, Int) A] @publicInBinary() private[matrix] (
      val raw: NArray[A],
      val rows: Row,
      val cols: Col,
      val rowStride: Int,
      val colStride: Int,
      val offset: Int = 0
  )
```
And now, we have zero copy transpose, sub-matrix, and certain subsets of slicing... but a a _lot_ more complexity in the datastructure.




