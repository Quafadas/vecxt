// In package viz because viz.PlotTarget is private[viz]
package viz

/** A plot target that writes the final Vega-Lite spec to `~/.cache/vecxt-plots/<name>.json`, for rendering elsewhere
  * (e.g. vl-convert) or inspecting the JSON.
  */
class SpecDump(name: String) extends PlotTarget:
  def show(spec: ujson.Value, library: ChartLibrary): VizReturn =
    val dir = os.home / ".cache" / "vecxt-plots"
    os.makeDir.all(dir)
    val file = dir / s"$name.json"
    os.write.over(file, spec.render(indent = 2))
    println(s"spec written to $file")
    file
  end show
end SpecDump
