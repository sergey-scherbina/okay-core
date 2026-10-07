package okay.core

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import Cont.*
import Effects.*

/** N QUEENS by `choose`: a queen per row, its column chosen among the safe ones, every solution — multi-shot,
 * a resumption per safe square, the solutions joined on the way back */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class QueensBenchmark:

  @Param(Array("8"))
  var size: Int = 0

  def safe(col: Int, placed: List[Int]): Boolean =
    placed.zipWithIndex.forall((c, d) => c != col && math.abs(c - col) != d + 1)

  def place(row: Int, placed: List[Int])(using c: Effects, ch: Has[Choose, c.R]): Cont[c.R, c.S, c.S, List[Int]] =
    if row == size then pure(placed)
    else among((0 until size).filter(safe(_, placed))).flatMap(col => place(row + 1, col :: placed))

  @Benchmark
  def queens(): Int = run(choose[List[Int]](place(0, Nil))).length
