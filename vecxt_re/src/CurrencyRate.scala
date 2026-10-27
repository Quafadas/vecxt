package vecxt_re

import java.time.LocalDate

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*

case class CurrencyRate(date: LocalDate, base: Ccy, quote: Ccy, rate: Double)

object CurrencyRate:
  /** e.g. `{"date":"2026-10-27","base":"EUR","quote":"USD","rate":1.08}` */
  given JsonValueCodec[CurrencyRate] = JsonCodecMaker.make
end CurrencyRate
