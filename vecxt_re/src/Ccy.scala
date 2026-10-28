package vecxt_re

import com.github.plokhotnyuk.jsoniter_scala.core.*

/** An ISO 4217 currency code, e.g. `Ccy.USD` or `Ccy("SEK")`. Open, so callers may use currencies not listed here. */
opaque type Ccy = String
object Ccy:
  def apply(code: String): Ccy =
    require(code.length == 3 && code.forall(c => c >= 'A' && c <= 'Z'), s"Not an ISO 4217 currency code: '$code'")
    code
  end apply

  val USD: Ccy = "USD"
  val EUR: Ccy = "EUR"
  val GBP: Ccy = "GBP"
  val CHF: Ccy = "CHF"
  val JPY: Ccy = "JPY"
  val AUD: Ccy = "AUD"
  val CAD: Ccy = "CAD"
  val NZD: Ccy = "NZD"

  /** The currencies predefined above. */
  val majorCurrencies: IArray[Ccy] = IArray(USD, EUR, GBP, CHF, JPY, AUD, CAD, NZD)

  extension (c: Ccy) def code: String = c
  end extension

  given CanEqual[Ccy, Ccy] = CanEqual.derived

  /** Encodes as the bare code, e.g. `"USD"`. Decoding validates the code as [[apply]] does. */
  given JsonValueCodec[Ccy] with
    def decodeValue(in: JsonReader, default: Ccy): Ccy =
      val code = in.readString(null)
      if code == null then in.decodeError("expected a currency code, got null")
      else
        try Ccy(code)
        catch case e: IllegalArgumentException => in.decodeError(e.getMessage)
      end if
    end decodeValue
    def encodeValue(x: Ccy, out: JsonWriter): Unit = out.writeVal(x)
    def nullValue: Ccy = null.asInstanceOf[Ccy]
  end given
end Ccy
