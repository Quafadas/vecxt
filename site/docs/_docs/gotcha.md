## On JVM

```
val v : Array[Double] = ???

v.sum

```
You will possibly bleed 8x performance. [Performance Perils of JVM Array Operations](https://blog.simon.codes/posts/jvm-array-performance-perils/)


## On JS

Despite my best efforts, there are frictions associated with cross platform initiatives.

In javascript

```javascript
const arr = new Float64Array(2)
arr[10]
```

will result in `undefined`, and your program will continue onwards with essentially undefined future behaviuor.

This is materially different to the behaviour on the JVM and native arrays, which are going to throw `ArrayOutOfBounds` or some similar exception letting you know things have gone wrong. Whilst enabling this  exception throwiung behaviour is _possible_ in scalaJS (use `Array[Double]`) , it comes with a performance impact.

It is an explicit goal of `vecxt` to be performant, and to introduce as few abstractions as possible. We make no effort to solve this problem in JS land.

