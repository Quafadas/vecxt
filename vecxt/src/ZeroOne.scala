package vecxt

trait OneAndZero[A]:
  def zero: A
  def one: A
end OneAndZero

object OneAndZero:
  given OneAndZero[Boolean] with
    def zero: Boolean = false
    def one: Boolean = true
  end given

  given [A](using n: Numeric[A]): OneAndZero[A] with
    def zero: A = n.zero
    def one: A = n.one
  end given
end OneAndZero
