// In package viz because viz.PlotTarget is private[viz]
package viz

/** A plot target for WSL: writes the plot to an HTML file and opens it in the Windows default browser.
  *
  * `viz.PlotTargets.desktopBrowser` falls back to `xdg-open`, which a bare WSL install does not have. This uses
  * `wslview` if it is on the path, and otherwise `explorer.exe` with the file's Windows (`\\wsl.localhost\...`) path.
  *
  * {{{
  * import viz.WslBrowser.wslBrowser
  * plotSeverityTrend(years, losses)
  * }}}
  */
object WslBrowser:

  given wslBrowser: viz.PlotTarget = new viz.PlotTarget:
    def show(spec: ujson.Value, library: ChartLibrary): viz.VizReturn =
      os.makeDir.all(plotDir)
      // Not os.temp: that deletes on JVM exit, before the browser has read the file
      val file = plotDir / s"plot-${System.currentTimeMillis()}-${spec.hashCode.toHexString}.html"
      os.write.over(file, html(spec.render(), library))
      open(file)
      file
    end show

  /** Outside /tmp so the files survive and are reachable from Windows. */
  val plotDir: os.Path = os.home / ".cache" / "vecxt-plots"

  private def onPath(cmd: String): Boolean =
    os.proc("sh", "-c", s"command -v $cmd").call(check = false, stderr = os.Pipe).exitCode == 0

  private def open(file: os.Path): Unit =
    println(s"plot written to $file")
    if onPath("wslview") then os.proc("wslview", file.toString).call(check = false)
    else
      val winPath = os.proc("wslpath", "-w", file.toString).call().out.trim()
      // explorer.exe returns exit code 1 even on success, so don't check it
      os.proc("explorer.exe", winPath).call(check = false)
    end if
    ()
  end open

  private def html(spec: String, library: ChartLibrary): String = library match
    case ChartLibrary.Vega =>
      s"""<!DOCTYPE html>
         |<html>
         |<head>
         |  <meta charset="utf-8" />
         |  <title>Plot</title>
         |  <script src="https://cdn.jsdelivr.net/npm/vega@6"></script>
         |  <script src="https://cdn.jsdelivr.net/npm/vega-lite@6"></script>
         |  <script src="https://cdn.jsdelivr.net/npm/vega-embed@7"></script>
         |  <style>
         |    html, body { margin: 0; height: 100%; }
         |    #vis { width: calc(100vw - 40px); height: calc(100vh - 40px); padding: 20px; }
         |  </style>
         |</head>
         |<body>
         |  <div id="vis"></div>
         |  <script>
         |    vegaEmbed('#vis', $spec, { renderer: "svg", actions: { editor: true } }).catch(console.error);
         |  </script>
         |</body>
         |</html>""".stripMargin
    case ChartLibrary.Echarts =>
      s"""<!DOCTYPE html>
         |<html>
         |<head>
         |  <meta charset="utf-8" />
         |  <title>Plot</title>
         |  <script src="https://cdn.jsdelivr.net/npm/echarts@5.6.0/dist/echarts.min.js"></script>
         |</head>
         |<body style="margin: 0">
         |  <div id="main" style="width: 100vw; height: 100vh;"></div>
         |  <script>echarts.init(document.getElementById('main')).setOption($spec);</script>
         |</body>
         |</html>""".stripMargin

end WslBrowser
