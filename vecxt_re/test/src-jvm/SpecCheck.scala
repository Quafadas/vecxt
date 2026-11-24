package vecxt_re

import ujson.Value

/** Structural checks on a final Vega-Lite spec, as a plot sends it to its target.
  *
  * For every mark, finds the data rows it draws (inline `values`, a named dataset, or inherited from its parent) and
  * every field it refers to: encoding channels (including tooltip arrays, conditions, and encodings inherited from a
  * parent), `datum.x` references in `filter` and `calculate` expressions, and selection `fields`. Fields created by
  * transforms (`as`) count as present.
  *
  * Reports a mark with no rows, a field missing from its rows, and a quantitative field that is null in every row
  * (circe writes NaN and infinity as null). Data from a `url` or a generator is not checked.
  */
object SpecCheck:

  private type Rows = Option[IndexedSeq[collection.Map[String, Value]]] // None: not inline, so not checked

  def problems(spec: Value): List[String] =
    val datasets = spec.obj.get("datasets").map(_.obj).getOrElse(collection.Map.empty[String, Value])
    val out = List.newBuilder[String]
    walk(spec, "spec", datasets, rows = None, produced = Set.empty, parentEncoding = Map.empty, out)
    out.result()
  end problems

  private def rowsOf(node: Value, datasets: collection.Map[String, Value], inherited: Rows, path: String)(
      out: collection.mutable.Builder[String, List[String]]
  ): Rows =
    node.obj.get("data") match
      case None    => inherited
      case Some(d) =>
        val o = d.obj
        def asRows(v: Value) = Some(v.arr.toIndexedSeq.collect { case r: ujson.Obj => r.value })
        if o.contains("values") then asRows(o("values"))
        else if o.contains("name") then
          val name = o("name").str
          datasets.get(name) match
            case Some(v) => asRows(v)
            case None    =>
              out += s"$path: data refers to dataset '$name', which is not in datasets"
              Some(IndexedSeq.empty)
          end match
        else None
        end if
  end rowsOf

  /** Field names created by a node's transforms. */
  private def producedBy(node: Value): Set[String] =
    def asNames(v: Value): Seq[String] = v match
      case ujson.Str(s) => Seq(s)
      case ujson.Arr(a) => a.toSeq.collect { case ujson.Str(s) => s }
      case _            => Nil
    def inEntry(entry: Value): Seq[String] = entry match
      case o: ujson.Obj =>
        o.value.get("as").toSeq.flatMap(asNames) ++
          // aggregate, joinaggregate and window put their `as` one level down
          o.value.values.toSeq.collect { case a: ujson.Arr => a.value.toSeq.flatMap(inEntry) }.flatten
      case _ => Nil
    node.obj.get("transform").toSeq.flatMap(_.arr).flatMap(inEntry).toSet
  end producedBy

  private val datumRef = """datum\.([A-Za-z_][A-Za-z0-9_]*)|datum\[['"]([^'"]+)['"]\]""".r

  private def expressionFields(node: Value): Seq[String] =
    node.obj
      .get("transform")
      .toSeq
      .flatMap(_.arr)
      .flatMap(t => Seq("filter", "calculate").flatMap(k => t.obj.get(k)))
      .collect { case ujson.Str(expr) => expr }
      .flatMap(e => datumRef.findAllMatchIn(e).map(m => Option(m.group(1)).getOrElse(m.group(2))))

  /** (field, isQuantitative) for each field a channel definition refers to. */
  private def channelFields(channel: Value): Seq[(String, Boolean)] = channel match
    case o: ujson.Obj =>
      val own = o.value.get("field").collect { case ujson.Str(f) =>
        f -> o.value.get("type").contains(ujson.Str("quantitative"))
      }
      val cond = o.value.get("condition").toSeq.flatMap {
        case a: ujson.Arr => a.value.toSeq.flatMap(channelFields)
        case c            => channelFields(c)
      }
      own.toSeq ++ cond
    case a: ujson.Arr => a.value.toSeq.flatMap(channelFields) // tooltip arrays
    case _            => Nil

  private def walk(
      node: Value,
      path: String,
      datasets: collection.Map[String, Value],
      rows: Rows,
      produced: Set[String],
      parentEncoding: Map[String, Value],
      out: collection.mutable.Builder[String, List[String]]
  ): Unit =
    val myRows = rowsOf(node, datasets, rows, path)(out)
    val myProduced = produced ++ producedBy(node)
    val encoding = parentEncoding ++ node.obj.get("encoding").map(_.obj.toMap).getOrElse(Map.empty)

    node.obj.get("layer") match
      case Some(layers) =>
        layers.arr.zipWithIndex.foreach { (l, i) =>
          walk(l, s"$path.layer[$i]", datasets, myRows, myProduced, encoding, out)
        }
      case None if node.obj.contains("mark") =>
        myRows.foreach { rs =>
          if rs.isEmpty then out += s"$path: no data rows"
          else
            val fromEncoding = encoding.toSeq.flatMap((ch, v) => channelFields(v).map((f, q) => (ch, f, q)))
            val fromExpressions = expressionFields(node).map(f => ("transform", f, false))
            val fromParams = node.obj
              .get("params")
              .toSeq
              .flatMap(_.arr)
              .flatMap(p => p.obj.get("select").flatMap(_.objOpt).flatMap(_.get("fields")).toSeq.flatMap(_.arr))
              .collect { case ujson.Str(f) => ("params", f, false) }

            for (where, field, quantitative) <- (fromEncoding ++ fromExpressions ++ fromParams).distinct
            do
              if !myProduced.contains(field) then
                val missing = rs.count(r => !r.contains(field))
                if missing > 0 then
                  out += s"$path: $where uses field '$field', missing from $missing of ${rs.length} rows"
                else if quantitative && rs.forall(_(field).isNull) then
                  out += s"$path: $where field '$field' is null in every row"
                end if
            end for
          end if
        }
      case None => ()
    end match
  end walk

end SpecCheck
