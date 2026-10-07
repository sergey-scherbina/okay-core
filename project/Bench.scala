import sbt._
import sbt.Keys._
import pl.project13.scala.sbt.JmhPlugin.JmhKeys.Jmh

/** `sbt bench`: JMH with its results to CSV, then one table at the very end — every benchmark's time per run
 * with its error, per operation (a run's time over its `n` parameter, when it has one), relative to the
 * fastest per operation, and the whole run's wall time. JMH's own options and a regexp pass through:
 * `sbt "bench -f 1 -p n=100000 tailcall"` */
object Bench {
  val benchRun = inputKey[Unit]("JMH run, then a summary table of the results")

  private final case class Row(name: String, mode: String, score: Double, error: Double, unit: String, n: Option[Long])

  def settings: Seq[Setting[_]] = Seq(
    benchRun := Def.inputTaskDyn {
      val args = Def.spaceDelimited("<jmh options>").parsed
      val csv = target.value / "jmh" / "results.csv"
      IO.delete(csv)
      IO.createDirectory(csv.getParentFile)
      val start = System.nanoTime()
      Def.sequential(
        (Jmh / run).toTask((Seq("-rf", "csv", "-rff", csv.getAbsolutePath) ++ args).mkString(" ", " ", "")),
        Def.task(report(csv, System.nanoTime() - start, streams.value.log))
      )
    }.evaluated
  )

  /** a line of JMH's CSV: fields, quoted or not, split on the commas outside quotes */
  private def fields(line: String): Vector[String] = {
    val out = Vector.newBuilder[String]
    val cur = new StringBuilder
    var quoted = false
    line.foreach {
      case '"' => quoted = !quoted
      case ',' if !quoted => out += cur.result(); cur.clear()
      case c => cur += c
    }
    out += cur.result()
    out.result()
  }

  private def number(s: String): Double = try s.trim.replace(',', '.').toDouble catch { case _: NumberFormatException => Double.NaN }

  private def read(csv: File): Seq[Row] = {
    val lines = IO.readLines(csv).filter(_.trim.nonEmpty)
    if (lines.isEmpty) Nil
    else {
      val head = fields(lines.head)
      def col(name: String) = head.indexWhere(_ == name)
      val (b, m, sc, er, u, n) = (col("Benchmark"), col("Mode"), col("Score"), head.indexWhere(_.startsWith("Score Error")), col("Unit"), col("Param: n"))
      lines.tail.map(fields).map { f =>
        Row(f(b).split('.').takeRight(2).mkString("."), f(m), number(f(sc)), number(f(er)), f(u),
          if (n >= 0) scala.util.Try(f(n).trim.toLong).toOption else None)
      }
    }
  }

  /** a time in its unit, to nanoseconds; `us/op` → microseconds */
  private def toNanos(unit: String): Option[Double] = unit.takeWhile(_ != '/') match {
    case "ns" => Some(1.0)
    case "us" => Some(1e3)
    case "ms" => Some(1e6)
    case "s" => Some(1e9)
    case _ => None
  }

  /** a time in nanoseconds, in the unit that keeps it between 1 and 1000 */
  private def time(ns: Double): String =
    if (ns.isNaN) "—"
    else if (ns < 1e3) f"$ns%.2f ns"
    else if (ns < 1e6) f"${ns / 1e3}%.2f µs"
    else if (ns < 1e9) f"${ns / 1e6}%.2f ms"
    else f"${ns / 1e9}%.2f s"

  private def wall(ns: Long): String = {
    val s = ns / 1000000000L
    f"${s / 60}%d:${s % 60}%02d"
  }

  private def report(csv: File, elapsed: Long, log: Logger): Unit =
    if (!csv.exists) log.warn("bench: no results — JMH ran nothing")
    else {
      val rows = read(csv)
      val timed = rows.map(r => r -> toNanos(r.unit))
      /** what `× fastest` compares: the time of one operation where `n` says how many a run has, else of a run —
       * so the same benchmark at another `n` is 1× */
      def cost(r: Row, k: Double): Double = r.n.filter(_ > 0).fold(r.score * k)(n => r.score * k / n)
      val costs = timed.collect { case (r, Some(k)) if r.mode == "avgt" || r.mode == "sample" || r.mode == "ss" => cost(r, k) }
      val best = if (costs.isEmpty) Double.NaN else costs.min
      val header = Vector("Benchmark", "n", "per run", "± error", "per op", "× fastest")
      val body = timed.map { case (r, k) =>
        val run = k.map(r.score * _).getOrElse(Double.NaN)
        val err = k.map(r.error * _).getOrElse(Double.NaN)
        Vector(
          r.name,
          r.n.fold("")(_.toString),
          if (k.isDefined) time(run) else f"${r.score}%.3f ${r.unit}",
          if (k.isDefined) time(err) else f"${r.error}%.3f",
          r.n.filter(_ > 0).fold("")(n => time(run / n)),
          k.filter(_ => !best.isNaN).fold("")(k => f"${cost(r, k) / best}%.2f×")
        )
      }
      val all = header +: body
      val widths = header.indices.map(i => all.map(_(i).length).max)
      val right = Set(1, 2, 3, 4, 5)
      def line(cells: Vector[String]) = cells.zipWithIndex.map { case (c, i) =>
        if (right(i)) " " * (widths(i) - c.length) + c else c + " " * (widths(i) - c.length)
      }.mkString("│ ", " │ ", " │")
      def rule(l: String, m: String, r: String) = widths.map(w => "─" * (w + 2)).mkString(l, m, r)
      val out = Seq(rule("┌", "┬", "┐"), line(header), rule("├", "┼", "┤")) ++ body.map(line) ++
        Seq(rule("└", "┴", "┘"), s"${rows.size} benchmarks, total time ${wall(elapsed)}, results in $csv")
      log.info("")
      out.foreach(log.info(_))
    }
}
