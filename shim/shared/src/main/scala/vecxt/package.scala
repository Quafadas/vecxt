package vecxt

transparent trait vecxt {
  def update(i: Int, d: Double): Unit
  def apply(i:Int) : Double
  def length: Int
}

