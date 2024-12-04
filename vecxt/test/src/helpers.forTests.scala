/*
 * Copyright 2023 quafadas
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package vecxt

import narr.*

extension [A <: AnyRef](o: A) def some: Some[A] = Some(o)

inline def assertVecEquals(v1: NArray[Double], v2: NArray[Double])(implicit loc: munit.Location): Unit =
  assert(v1.length == v2.length)
  var i: Int = 0;
  while i < v1.length do
    munit.Assertions.assertEquals(v1(i), v2(i))
    i += 1
  end while
end assertVecEquals

inline def assertVecEquals[A](v1: NArray[A], v2: NArray[A])(implicit loc: munit.Location): Unit =
  var i: Int = 0;
  while i < v1.length do
    munit.Assertions.assertEquals(v1(i), v2(i))
    i += 1
  end while
end assertVecEquals
