
package vecxt.laws

/** Dimension as an opaque type for type safety
  */
opaque type Dimension = Int

object Dimension:
  def apply(n: Int): Dimension = n

  extension (d: Dimension) def size: Int = d
  end extension
end Dimension
