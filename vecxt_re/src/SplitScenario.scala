package vecxt_re

import vecxt.BoundsCheck.BoundsCheck
import vecxt_re.SplitLosses.splitAmntFast

object SplitScenario:
  extension (tower: Tower)
    inline def splitScenarioAmounts(scenario: Scenarr)(using
        inline bc: BoundsCheck
    ): (
        ceded: Array[Double],
        retained: Array[Double],
        splits: IndexedSeq[(layer: Layer, cededToLayer: Array[Double])]
    ) =
      val tmp =
        if bc then scenario.sorted
        else scenario

      tower.splitAmntFast(
        tmp.iterations,
        tmp.amounts
      )
  end extension
end SplitScenario
