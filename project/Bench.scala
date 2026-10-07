import sbt._
import sbt.Keys._
import pl.project13.scala.sbt.JmhPlugin.JmhKeys.Jmh

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.sys.process._

/** `sbt bench`: JMH with its results to CSV, then one table at the very end — every benchmark's time per run
 * with its error, per operation (a run's time over its `n` parameter, when it has one), relative to the
 * fastest per operation, the bytes allocated per operation when run with `-prof gc`, the change against the
 * last run of the same benchmark, and the whole run's wall time. A parameter other than `n` goes with the name:
 * `forwardedDepth depth=4`. JMH's own options and a regexp pass through: `sbt "bench -f 1 -p n=100000 tailcall"`.
 *
 * THE HISTORY, `bench.d/`: a run with `save` among its arguments — `sbt "bench save"`, `sbt "bench save -prof gc
 * tailcall"` — is kept as `<time>_<commit>.csv` (`+` after the commit when the tree had changes), with a line for
 * it in `bench.d/index.tsv`: time, commit, changed, JMH arguments, wall time, file. A run without it is not kept;
 * its `Δ` still compares with the history. `sbt "benchHistory <regexp>"`: the benchmarks matching it, run by run,
 * oldest first. */
object Bench {
  val benchRun = inputKey[Unit]("JMH run, then a summary table of the results, kept in bench.d")
  val benchHistory = inputKey[Unit]("the benchmarks matching a regexp, through every run in bench.d")

  private final case class Row(name: String, mode: String, score: Double, error: Double, unit: String, n: Option[Long],
                               alloc: Option[Double]) {
    /** the row across runs: its name with its parameters, and `n` */
    def key: (String, Option[Long]) = (name, n)
    private def nanos: Option[Double] = toNanos(unit)
    /** a run's time in nanoseconds */
    def run: Option[Double] = nanos.map(score * _)
    def err: Option[Double] = nanos.map(error * _)
    /** one operation's time where `n` says how many a run has, else a run's — what `× fastest` and `Δ` compare */
    def cost: Option[Double] = run.map(t => n.filter(_ > 0).fold(t)(t / _))
    def allocOp: Option[Double] = alloc.map(_ / n.filter(_ > 0).getOrElse(1L))
    def timed: Boolean = mode == "avgt" || mode == "sample" || mode == "ss"
  }

  private val stamp = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH-mm-ss")

  def settings: Seq[Setting[_]] = Seq(
    benchRun := Def.inputTaskDyn {
      val (flags, args) = Def.spaceDelimited("<save> <jmh options>").parsed.partition(_ == "save")
      val save = flags.nonEmpty
      val csv = target.value / "jmh" / "results.csv"
      val history = baseDirectory.value / "bench.d"
      IO.delete(csv)
      IO.createDirectory(csv.getParentFile)
      val start = System.nanoTime()
      Def.sequential(
        (Jmh / run).toTask((Seq("-rf", "csv", "-rff", csv.getAbsolutePath) ++ args).mkString(" ", " ", "")),
        Def.task {
          val log = streams.value.log
          val elapsed = System.nanoTime() - start
          if (!csv.exists) log.warn("bench: no results — JMH ran nothing")
          else {
            val previous = runs(history)
            val kept = if (save) Some(keep(csv, history, baseDirectory.value, args, elapsed)) else None
            report(read(csv), previous, elapsed, kept, log)
          }
        }
      )
    }.evaluated,
    benchHistory := {
      val pattern = Def.spaceDelimited("<regexp>").parsed.mkString(" ").r
      history(runs(baseDirectory.value / "bench.d"), pattern, streams.value.log)
    }
  )

  /** the commit, short, and whether the tree had changes; `?` outside git */
  private def commit(base: File): (String, Boolean) = {
    def git(cmd: String*) = scala.util.Try(Process("git" +: cmd, base).!!(ProcessLogger(_ => ()))).toOption.map(_.trim)
    (git("rev-parse", "--short", "HEAD").getOrElse("?"), git("status", "--porcelain").exists(_.nonEmpty))
  }

  /** the run's CSV into the history, and its line in the index; the kept file */
  private def keep(csv: File, history: File, base: File, args: Seq[String], elapsed: Long): File = {
    IO.createDirectory(history)
    val now = LocalDateTime.now.format(stamp)
    val (sha, changed) = commit(base)
    val kept = history / s"${now}_$sha${if (changed) "+" else ""}.csv"
    IO.copyFile(csv, kept)
    val index = history / "index.tsv"
    if (!index.exists) IO.write(index, "time\tcommit\tchanged\targs\twall\tfile\n")
    IO.append(index, Seq(now, sha, changed.toString, args.mkString(" "), wall(elapsed), kept.getName).mkString("", "\t", "\n"))
    kept
  }

  /** every run in the history, oldest first: its file and its rows */
  private def runs(history: File): Seq[(File, Seq[Row])] =
    if (!history.exists) Nil
    else IO.listFiles(history).filter(_.getName.endsWith(".csv")).sortBy(_.getName).toSeq.map(f => f -> read(f))

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

  /** the primary rows, each with its bytes per run from the gc profiler's secondary row, when there is one */
  private def read(csv: File): Seq[Row] = {
    val lines = IO.readLines(csv).filter(_.trim.nonEmpty)
    if (lines.isEmpty) Nil
    else {
      val head = fields(lines.head)
      def col(name: String) = head.indexWhere(_ == name)
      val (b, m, sc, er, u, n) = (col("Benchmark"), col("Mode"), col("Score"), head.indexWhere(_.startsWith("Score Error")), col("Unit"), col("Param: n"))
      val params = head.zipWithIndex.collect { case (h, i) if h.startsWith("Param: ") && h != "Param: n" => (h.stripPrefix("Param: "), i) }
      val rows = lines.tail.map(fields)
      def key(f: Vector[String]) = (f(b).takeWhile(_ != ':'), params.map { case (_, i) => f(i) })
      val allocs = rows.filter(_(b).endsWith(":gc.alloc.rate.norm")).map(f => key(f) -> number(f(sc))).toMap
      rows.filterNot(_(b).contains(':')).map { f =>
        val shown = params.collect { case (p, i) if f(i).trim.nonEmpty => s" $p=${f(i)}" }.mkString
        Row(f(b).split('.').takeRight(2).mkString(".") + shown, f(m), number(f(sc)), number(f(er)), f(u),
          if (n >= 0) scala.util.Try(f(n).trim.toLong).toOption else None, allocs.get(key(f)))
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

  /** the change from `before` to `now`, in percent; blank when either is missing */
  private def delta(before: Option[Double], now: Option[Double]): String = (before, now) match {
    case (Some(b), Some(a)) if b > 0 && !a.isNaN && !b.isNaN =>
      val d = (a - b) / b * 100
      if (math.abs(d) < 0.5) "0%" else f"$d%+.0f%%"
    case _ => ""
  }

  /** the rows as a table: the first column left, the rest right */
  private def table(header: Vector[String], body: Seq[Vector[String]]): Seq[String] = {
    val all = header +: body
    val widths = header.indices.map(i => all.map(_(i).length).max)
    def line(cells: Vector[String]) = cells.zipWithIndex.map { case (c, i) =>
      if (i > 0) " " * (widths(i) - c.length) + c else c + " " * (widths(i) - c.length)
    }.mkString("│ ", " │ ", " │")
    def rule(l: String, m: String, r: String) = widths.map(w => "─" * (w + 2)).mkString(l, m, r)
    Seq(rule("┌", "┬", "┐"), line(header), rule("├", "┼", "┤")) ++ body.map(line) :+ rule("└", "┴", "┘")
  }

  private def report(rows: Seq[Row], previous: Seq[(File, Seq[Row])], elapsed: Long, kept: Option[File], log: Logger): Unit = {
    val costs = rows.filter(_.timed).flatMap(_.cost)
    val best = if (costs.isEmpty) Double.NaN else costs.min
    val profiled = rows.exists(_.alloc.isDefined)
    /** the last run before this one that has the row */
    def last(r: Row): Option[Row] = previous.reverseIterator.flatMap(_._2.find(_.key == r.key)).toSeq.headOption
    val header = Vector("Benchmark", "n", "per run", "± error", "per op", "× fastest", "Δ time") ++
      (if (profiled) Vector("alloc/op", "Δ alloc") else Vector())
    val body = rows.map { r =>
      val before = last(r)
      Vector(
        r.name,
        r.n.fold("")(_.toString),
        r.run.fold(f"${r.score}%.3f ${r.unit}")(time),
        r.err.fold(f"${r.error}%.3f")(time),
        r.n.filter(_ > 0).flatMap(_ => r.cost).fold("")(time),
        r.cost.filter(_ => !best.isNaN).fold("")(c => f"${c / best}%.2f×"),
        delta(before.flatMap(_.cost), r.cost)
      ) ++ (if (profiled) Vector(r.allocOp.fold("")(b => f"$b%.0f B"), delta(before.flatMap(_.allocOp), r.allocOp)) else Vector())
    }
    val since = previous.lastOption.fold("no run before")(p => s"Δ against the last run with each benchmark, latest ${p._1.getName}")
    log.info("")
    (table(header, body) ++ Seq(
      s"${rows.size} benchmarks, total time ${wall(elapsed)}; $since",
      kept.fold("not kept: `sbt \"bench save …\"` keeps a run in bench.d")(k => s"kept in ${k.getParentFile.getName}/${k.getName}")
    )).foreach(log.info(_))
  }

  private def history(runs: Seq[(File, Seq[Row])], pattern: scala.util.matching.Regex, log: Logger): Unit = {
    val body = for {
      (file, rows) <- runs
      r <- rows if pattern.findFirstIn(r.name).isDefined
    } yield (r.name, file, r)
    if (body.isEmpty) log.warn(s"benchHistory: no run in bench.d has a benchmark matching '$pattern'")
    else {
      val lines = body.groupBy(_._1).toSeq.sortBy(_._1).flatMap { case (_, runs) =>
        runs.sortBy(_._2.getName).foldLeft((Vector.empty[Vector[String]], Option.empty[Row])) { case ((acc, prev), (name, file, r)) =>
          val cells = Vector(name, file.getName.stripSuffix(".csv"), r.n.fold("")(_.toString),
            r.cost.fold("")(time), delta(prev.flatMap(_.cost), r.cost), r.allocOp.fold("")(b => f"$b%.0f B"))
          (acc :+ cells, Some(r))
        }._1
      }
      log.info("")
      table(Vector("Benchmark", "run", "n", "per op", "Δ time", "alloc/op"), lines).foreach(log.info(_))
    }
  }
}
