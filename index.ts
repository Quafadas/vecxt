"use strict";

var discreteUniform = require("@stdlib/random/array/discrete-uniform");
var dgemm = require("@stdlib/blas/base/dgemm/lib");

var opts = {
  dtype: "float64",
};

var M = 3;
var N = 4;
var K = 2;

var A = discreteUniform(M * K, 0, 10, opts); // 3x2
var B = discreteUniform(K * N, 0, 10, opts); // 2x4
var C = discreteUniform(M * N, 0, 10, opts); // 3x4

dgemm(
  "row-major",
  "no-transpose",
  "no-transpose",
  M,
  N,
  K,
  1.0,
  A,
  K,
  B,
  N,
  1.0,
  C,
  N
);
console.log(C);

dgemm.ndarray(
  "no-transpose",
  "no-transpose",
  M,
  N,
  K,
  1.0,
  A,
  K,
  1,
  0,
  B,
  N,
  1,
  0,
  1.0,
  C,
  N,
  1,
  0
);
console.log(C);
