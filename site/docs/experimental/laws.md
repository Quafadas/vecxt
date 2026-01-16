# Laws Testing

vecxt provides a laws-based testing infrastructure to verify algebraic properties of vector operations using [cats](https://typelevel.org/cats/) and [discipline](https://github.com/typelevel/discipline).

## Overview

Vector operations form algebraic structures (Monoids, Semigroups, etc.) that must satisfy certain mathematical laws. The vecxt laws module provides:

- **Type-safe dimension tracking** via opaque types
- **Integration with cats laws** for automatic property testing
- **Cross-platform compatibility** (JVM, JS, Native)
- **Integration with vecxt's BoundsCheck system**

## Why Laws Testing?

Laws testing helps ensure that vector operations behave correctly by automatically checking properties like:

- **Identity**: `combine(empty, x) === x` and `combine(x, empty) === x`
- **Associativity**: `combine(combine(x, y), z) === combine(x, combine(y, z))`
- **Commutativity** (for commutative operations): `combine(x, y) === combine(y, x)`

These properties are verified across hundreds of generated test cases using property-based testing with ScalaCheck.

## The Dimension Context Pattern

Traditional Monoid typeclasses require a parameterless `empty` value:

```scala
trait Monoid[A]:
  def empty: A  // ← How do we know what length this should be?
  def combine(x: A, y: A): A
```

For vectors (backed by `Array[Double]`), the dimension is runtime information. We solve this using **dimension as implicit context**:

```scala
// Dimension is an opaque type for type safety
opaque type Dimension = Int

// VectorMonoid scoped to a specific dimension
trait VectorMonoid[A] extends Monoid[Array[A]]:
  def dimension: Dimension
  def empty: Array[A]
  def combine(x: Array[A], y: Array[A]): Array[A]
```

## Usage

### Setting Up a Dimension Context

```scala
import vecxt.laws.*
import vecxt.laws.instances.double.*

// Create a dimension witness
given dim: Dimension = Dimension(3)
```

### Creating Monoid Instances

```scala
// Create a commutative monoid for vector addition
given VectorCommutativeMonoid[Double] =
  vectorAdditionMonoid(using dim)

// Or for multiplication
given VectorCommutativeMonoid[Double] =
  vectorMultiplicationMonoid(using dim)
```

### Using in Computations

```scala
def sumVectors(vectors: List[Array[Double]])(using vm: VectorMonoid[Double]): Array[Double] =
  vectors.foldLeft(vm.empty)(vm.combine)

// Usage
val vectors = List(
  Array(1.0, 2.0, 3.0),
  Array(4.0, 5.0, 6.0),
  Array(7.0, 8.0, 9.0)
)

val result = sumVectors(vectors)
// result: Array(12.0, 15.0, 18.0)
```

## Testing Your Own Operations

You can test custom vector operations by creating your own VectorMonoid instances:

```scala
import cats.kernel.Semigroup
import vecxt.laws.{Dimension, VectorCommutativeMonoid}
import vecxt.BoundsCheck

def customVectorMonoid(using dim: Dimension): VectorCommutativeMonoid[Double] =
  given Semigroup[Double] = Semigroup.instance[Double](_ + _)
  VectorCommutativeMonoid.forDimension(dim)(
    emptyFn = Array.fill(dim.size)(0.0),
    combineFn = (x, y) => {
      // Your custom combination logic
      val result = new Array[Double](x.length)
      var i = 0
      while i < x.length do
        result(i) = x(i) + y(i)
        i += 1
      result
    }
  )(using Semigroup[Double], BoundsCheck.DoBoundsCheck.yes)
```

Then test it with discipline:

```scala
import cats.kernel.laws.discipline.CommutativeMonoidTests
import cats.kernel.Eq
import munit.DisciplineSuite
import org.scalacheck.{Arbitrary, Gen}

class CustomMonoidLawsSpec extends DisciplineSuite:
  given dim: Dimension = Dimension(10)

  given VectorCommutativeMonoid[Double] = customVectorMonoid

  given Arbitrary[Array[Double]] = Arbitrary(
    Gen.listOfN(10, Gen.choose(-100.0, 100.0)).map(_.toArray)
  )

  given Eq[Array[Double]] = Eq.instance((a, b) =>
    if a.length != b.length then false
    else
      var i = 0
      var equal = true
      while i < a.length && equal do
        equal = Math.abs(a(i) - b(i)) < 1e-10
        i += 1
      equal
  )

  checkAll(
    "CustomVectorMonoid",
    CommutativeMonoidTests[Array[Double]].commutativeMonoid
  )
```

## Available Laws Tests

The framework automatically tests:

### Monoid Laws
- `combine(empty, x) === x` (left identity)
- `combine(x, empty) === x` (right identity)
- `combine(combine(x, y), z) === combine(x, combine(y, z))` (associativity)
- `combineAll` correctness
- `combineN` correctness
- `repeat0` returns `empty`
- `collect0` returns `empty`
- `isEmpty` detects identity element

### Commutative Monoid Laws

All Monoid laws plus:

- `combine(x, y) === combine(y, x)` (commutativity)
- Intercalate operations preserve commutativity
- Reverse operations preserve commutativity

## Benefits

✅ **Correctness**: Automatically verify that operations satisfy mathematical laws

✅ **Property-Based Testing**: Tests with hundreds of generated inputs

✅ **Regression Prevention**: Catch bugs when refactoring implementations

✅ **Documentation**: Laws serve as executable specification

✅ **Integration**: Works with cats ecosystem and discipline

✅ **Zero Overhead**: Dimension validation can be disabled via BoundsCheck

## Dependencies

To use the laws module, add to your build:

```scala
// Mill
def mvnDeps = Seq(
  mvn"io.github.quafadas::vecxt-laws:$vecxtVersion"
)

// For testing
def testMvnDeps = Seq(
  mvn"org.scalameta::munit::$munitVersion",
  mvn"org.typelevel::discipline-munit:$disciplineVersion",
  mvn"org.scalacheck::scalacheck:$scalacheckVersion"
)
```

## Platform Support

- ✅ **JVM**: Full support with comprehensive tests
- ✅ **JavaScript**: Compiles successfully (test execution pending)
- ✅ **Native**: Compiles successfully (test execution pending)

## See Also

- [cats kernel](https://typelevel.org/cats/typeclasses.html)
- [discipline](https://github.com/typelevel/discipline) - Law checking for type classes
- [vecxt BoundsCheck system](../bounds.md)
