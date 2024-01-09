# JVM

If you are running on the JVM, The shim to a java SIMD [netlib](https://github.com/luhenry/netlib) will generate warnings when it's instantiated. That shim looks for potential execution pathways upon instantiation, and prefers;

1. JNI to C
2. Java SIMD
3. Fallback to vanilla Java

The console warnings on startup will tell you the implementations you are missing.

My understanding, is that for level 1 BLAS operations (i.e. this library), the performance of the SIMD implementation is comparable to native - i.e. C performance.

To enable the SIMD implementation, you'll need (at the time of writing) to enable the incubating `vector` API flags and be using JVM 18+. Consider adding this flag, to the JVM startup commands.

`--add-modules=jdk.incubator.vector`

# JS

You'll need this or better, available somewhere in your bundle / build / browser / whatever.

```json
{
  "dependencies": {
    "@stdlib/blas": "^0.1.1"
  }
}
```

# Native

Isn't 100% my thing, so this is less tested.

## Linux
On linux,

`sudo apt-get install -y libatlas-base-dev`

works, but not clear, if it's minimal.

## MacOS

I believe ships with CBLAS available, so shoudl work straight out the box.

## Windows

You'll need CBLAS on the path (I think).





