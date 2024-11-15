# Vecxt

Making cross plaform vector problem less... vexing

Scala cli
```scala
//> using dep io.github.quafadas::vecxt::{{projectVersion}}
```

### Mill
```scala sc:nocompile
ivy"io.github.quafadas::vecxt::{{projectVersion}}"
```

```scala
import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.no

val v1 = Array[Double](1, 2, 3)
val v2 = Array[Double](4, 5, 6)

println((v1 + v2).mkString("[",",","]"))

```

## Goals

- Where possible inline calls to platform-native-BLAS implementations for maximum performance
- Reasonable,, consistent cross platform ergonomics
- Very little / no data-structures - the vector part of the library is an extension method on `Array[Double]` for example
- A single cross platform test suite
- Simplicity, speed

## Non-Goals

- General mathematics - lets' try to keep a focus on linear algebra. See [slash](https://github.com/dragonfly-ai/slash) or [breeze](https://github.com/scalanlp/breeze/) for more general libraries
- Visualisation - see [dedav4s](https://quafadas.github.io/dedav4s/)