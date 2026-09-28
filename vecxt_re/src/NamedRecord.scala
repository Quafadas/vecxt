package vecxt_re

import scala.NamedTuple.{AnyNamedTuple, DropNames, NamedTuple, Names}
import scala.compiletime.{constValue, constValueTuple, erasedValue, error, summonFrom}

/** Compile-time helpers for accepting a named tuple by field name rather than by field position.
  *
  * A caller may supply `(id = "1", price = p, notional = n, name = "A")` where the required shape is
  * `(price: Price, notional: CurrencyAmount, name: String, id: String)`: field order is irrelevant and extra fields
  * are ignored, but every required field must be present with a conforming type or compilation fails.
  */
object NamedRecord:

  /** Marker produced by [[Lookup]] when a field name is absent. */
  sealed trait Missing

  /** The value type of field `K` in the named tuple with names `N` and values `V`, or [[Missing]]. */
  type Lookup[N <: Tuple, V <: Tuple, K] = (N, V) match
    case (K *: _, v *: _)     => v
    case (_ *: ns, _ *: vs)   => Lookup[ns, vs, K]
    case (EmptyTuple, EmptyTuple) => Missing

  /** Fails compilation unless the named tuple `(N, V)` has every field of `Required` with a conforming type.
    *
    * An unconstrained `N` (inferred as `Tuple` when passing `Nil`) is accepted, as there are no values to check.
    */
  inline def check[N <: Tuple, V <: Tuple, Required <: AnyNamedTuple]: Unit =
    summonFrom {
      case _: (N =:= Tuple) => ()
      case _                  => checkFields[N, V, Names[Required], DropNames[Required]]
    }

  private inline def checkFields[N <: Tuple, V <: Tuple, RN <: Tuple, RV <: Tuple]: Unit =
    inline erasedValue[RN] match
      case _: EmptyTuple => ()
      case _: (k *: rns) =>
        inline erasedValue[RV] match
          case _: (t *: rvs) =>
            inline erasedValue[Lookup[N, V, k]] match
              case _: Missing => error("Missing field: " + constValue[k & String])
              case _: t       => checkFields[N, V, rns, rvs]
              case _          => error("Field has the wrong type: " + constValue[k & String])

  /** The field names of `N` as runtime strings; empty for an unconstrained `N` (see [[check]]). */
  inline def names[N <: Tuple]: List[String] =
    summonFrom {
      case _: (N =:= Tuple) => Nil
      case _                => constValueTuple[N].toList.asInstanceOf[List[String]]
    }

  /** Zips pre-computed field `names` with the values of `nt` into a name -> value map. */
  def toMap[N <: Tuple, V <: Tuple](names: List[String], nt: NamedTuple[N, V]): Map[String, Any] =
    names.iterator.zip(nt.toTuple.productIterator).toMap

end NamedRecord
