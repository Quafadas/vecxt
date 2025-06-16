package vecxt

trait OneAndZero[A]:
  def zero: A
  def one: A

object OneAndZero:
  given OneAndZero[Boolean] with
    def zero: Boolean = false
    def one: Boolean = true
    
  given [A](using n: Numeric[A]): OneAndZero[A] with
    def zero: A = n.zero
    def one: A = n.one
