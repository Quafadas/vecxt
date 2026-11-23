import vecxt_re.all.*
import viz.WslBrowser.wslBrowser

@main def severityTrendDemo(): Unit =
  val years = Vector(
    2005, 2005, 2006, 2007, 2007, 2007, 2008, 2009, 2010, 2010, 2011, 2012, 2013, 2013, 2014, 2015, 2016, 2017, 2018,
    2018, 2019, 2020, 2021, 2022, 2023
  )
  val losses = Vector(
    1.2e6, 3.4e6, 2.1e6, 1.5e6, 8.9e6, 1.1e6, 2.7e6, 1.9e6, 4.4e6, 1.3e6, 2.2e6, 6.1e6, 1.7e6, 3.0e6, 2.5e6, 1.4e6,
    5.2e6, 2.9e6, 3.8e6, 1.6e6, 7.5e6, 2.4e6, 4.9e6, 3.3e6, 12.0e6
  )
  println(fitSeverityTrend(years, losses).summary)
  plotSeverityTrend(years, losses)
end severityTrendDemo
