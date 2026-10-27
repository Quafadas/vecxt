package vecxt_re

import java.time.LocalDate

import com.github.plokhotnyuk.jsoniter_scala.core.*

class CurrencyRateSuite extends munit.FunSuite:

  private val eurUsd = CurrencyRate(LocalDate.of(2026, 10, 27), Ccy.EUR, Ccy.USD, 1.08)
  private val eurUsdJson = """{"date":"2026-10-27","base":"EUR","quote":"USD","rate":1.08}"""

  test("rate encodes to json"):
    assertEquals(writeToString(eurUsd), eurUsdJson)

  test("rate decodes from json"):
    assertEquals(readFromString[CurrencyRate](eurUsdJson), eurUsd)

  test("currencies not predefined round trip"):
    val r = CurrencyRate(LocalDate.of(2026, 1, 2), Ccy("SEK"), Ccy.CHF, 0.083)
    assertEquals(readFromString[CurrencyRate](writeToString(r)), r)

  test("ccy encodes as its bare code"):
    assertEquals(writeToString(Ccy.GBP), "\"GBP\"")
    assertEquals(readFromString[Ccy]("\"NOK\""), Ccy("NOK"))

  test("invalid currency codes are rejected when decoding"):
    intercept[JsonReaderException](readFromString[CurrencyRate](eurUsdJson.replace("\"EUR\"", "\"eur\"")))
    intercept[JsonReaderException](readFromString[CurrencyRate](eurUsdJson.replace("\"USD\"", "\"USDX\"")))
    intercept[JsonReaderException](readFromString[CurrencyRate](eurUsdJson.replace("\"USD\"", "null")))

  test("missing fields are rejected"):
    intercept[JsonReaderException](readFromString[CurrencyRate]("""{"date":"2026-10-27","base":"EUR","rate":1.08}"""))

end CurrencyRateSuite
