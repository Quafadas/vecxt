package vecxt_re

import vecxt_re.SplitLosses.splitAmntFast

object SplitScenario:
  extension (tower: Tower)
    inline def splitScenarioAmounts(scenario: Scenarr): (
        ceded: Array[Double],
        retained: Array[Double],
        splits: IndexedSeq[(layer: Layer, cededToLayer: Array[Double])]
    ) =
      val tmp =
        scenario.sorted

      tower.splitAmntFast(
        tmp.iterations,
        tmp.amounts
      )
  end extension
end SplitScenario
