package vecxt.reinsurance

extension (tower: Tower)
  inline def splitLosses(years: IArray[Int], losses: IArray[Double])(using
      inline bc: vecxt.BoundsCheck.BoundsCheck
  ): (ceded: Array[Double], retained: Array[Double], splits: IndexedSeq[(Layer, Array[Double])]) =
    if losses.isEmpty then (Array.empty[Double], Array.empty[Double], tower.layers.map((_, Array.empty[Double])))
    else tower.splitAmntFast(years.toArray, losses.toArray)
end extension
