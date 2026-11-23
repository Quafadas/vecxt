import java.time.LocalDate
import vecxt_re.all.*

/** Illustrative data only: shaped like a cat-loss history, not real losses. */
@main def lossStemsDemo(): Unit =
  val dates = Vector(
    "2019-05-20",
    "2020-08-10",
    "2022-05-12",
    "2022-06-01",
    "2023-08-01",
    "2024-03-15",
    "2025-05-20",
    "2026-04-02",
    "2026-04-20"
  ).map(LocalDate.parse)
  val amounts = Vector(180e6, 437e6, 440e6, 290e6, 292e6, 258e6, 335e6, 393e6, 283e6)
  val normalised = Vector(365e6, 832e6, 653e6, 430e6, 386e6, 310e6, 374e6, 427e6, 308e6)
  val size = Seq(fixedSize(900, 400))

  // Opens in the Windows browser. For the raw JSON instead: given viz.LowPriorityPlotTarget = viz.SpecDump("name")
  import viz.WslBrowser.wslBrowser
  plotLossStems(dates, amounts, normalised, rollingYears = 4, title = "Historical losses", mods = size)
  plotSeverityTrend(dates.map(_.getYear), normalised, mods = size)
end lossStemsDemo
