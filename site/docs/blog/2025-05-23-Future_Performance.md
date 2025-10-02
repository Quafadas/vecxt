# Future Performance

$Date: 2025-05-23T10:00:00+01:00

This project tries to keep up with javas SIMD API. here's the latest video;

https://inside.java/2025/05/03/javaone-java-ai/

and some notes from it. They key takeaway for me, was that JAVA will probably never hit the kind of performance needed for hardcore compute bound problems. Instead they propose to ingegrate with libraries that will, and appear to be willing to try and move those ecoysystems to integrate with the JVM.

The example called out was BLIS, a C library for linear algebra. It is a drop in replacement for BLAS, and is used by many other libraries.

https://github.com/flame/blis

The claim is that one can use `jextract` to generate JVM friendly bindings to this library, and then use it from Java.

Worthy of investigation would be whether

[sn-bindgen](https://github.com/indoorvivants/sn-bindgen) can pull the same trick.

If so, then we would only need a JS equivalent library and we could pull it all into vecxt. Which would be kinda cool!

