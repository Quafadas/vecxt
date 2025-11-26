# Setup

## JVM


Adding this flag, to your JVM startup commands.

`--add-modules=jdk.incubator.vector`

At the time of writing this is the incubating `vector` API flags and you need JVM 18+.

If you are running on the JVM, The shim to a java SIMD [netlib](https://github.com/luhenry/netlib) will generate warnings when it's instantiated. That shim looks for potential execution pathways upon instantiation, and prefers;

1. A JNI to C
2. Java SIMD
3. Fallback to vanilla Java

The console warnings on startup will tell you the implementations you are missing.

My understanding, is that for level 1 BLAS operations (i.e. this library), the performance of the SIMD implementation is comparable to native - i.e. C performance. If you want native performance, you'll need to work through getting your natives libs setup with netlib - follow that readme.


## JS

The best way is without a bundler using ESModules. Use this import map to load the module out of JSdelivr.

```json
{
  "imports": {
    "@stdlib/blas/base": "https://cdn.jsdelivr.net/npm/@stdlib/blas@0.2.0/base/+esm"
  }
}
```

If you gotta bundle, you gotta bundle

```json
{
  "dependencies": {
    "@stdlib/blas": "^0.2.0"
  }
}
```

## Native

Is less tested, but works according to the tests. You'll need CBLAS on the path one way or another.

### Linux
On linux,

`sudo apt-get install -y libatlas-base-dev`

works, but not clear, if it's minimal.

## MacOS

I believe ships with CBLAS available, so should work straight out the box.

## Windows

You'll need CBLAS on the path (I think).





