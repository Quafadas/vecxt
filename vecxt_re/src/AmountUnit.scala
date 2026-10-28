package vecxt_re

/** The scale a monetary amount is quoted in. `multiplier` is the number of major currency units in one of this unit. */
enum AmountUnit(val multiplier: Long):
  case One extends AmountUnit(1L)
  case K extends AmountUnit(1_000L)
  case Mn extends AmountUnit(1_000_000L)
  case Bn extends AmountUnit(1_000_000_000L)
end AmountUnit
