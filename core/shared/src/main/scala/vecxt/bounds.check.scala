package vecxt

// opaque type BoundsCheck = Boolean

// object BoundsCheck:
//   inline def apply(inline d: Boolean): BoundsCheck = d
// end BoundsCheck

// extension (inline bc: BoundsCheck) inline def value: Boolean = bc.self

type BoundsCheck = Boolean

object BoundsCheck:
  inline given yes : BoundsCheck = true
  inline given no : BoundsCheck = false
end BoundsCheck
