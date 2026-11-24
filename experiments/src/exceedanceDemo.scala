import vecxt_re.all.*

/** Illustrative data only: two made-up perils on a 1000-year catalogue. */
@main def exceedanceDemo(): Unit =
  val n = 1000
  val rng = scala.util.Random(1)
  def peril(name: String, source: Int, rate: Double, scale: Double) =
    val b = Scenarr.builder(n, name = name)
    var eventId = 0
    (1 to n).foreach { year =>
      var t = -math.log(rng.nextDouble()) / rate
      while t < 1 do
        eventId += 1
        val loss = scale * math.pow(rng.nextDouble(), -1 / 1.5) // Pareto, alpha 1.5
        b.add(year, 1 + (t * 365).toInt, loss, Scenarr.eventKey(source, eventId))
        t += -math.log(rng.nextDouble()) / rate
      end while
    }
    b.result()
  end peril

  val perils = Seq(peril("Hurricane", 1, 1.2, 20e6), peril("Earthquake", 2, 0.3, 40e6))
  val layers = Seq(Layer(limit = 100e6, ret = 100e6), Layer(limit = 200e6, ret = 200e6))
  val size = Seq(fixedSize(900, 500))

  perils.foreach(p => println(f"${p.name}%-12s 1-in-200 OEP ${p.oep.lossAt(200) / 1e6}%8.1f m"))

  import viz.WslBrowser.wslBrowser
  perils.plotOep(layers, unit = AmountUnit.Mn, mods = size)
  perils.plotAep(unit = AmountUnit.Mn, mods = size)
end exceedanceDemo
