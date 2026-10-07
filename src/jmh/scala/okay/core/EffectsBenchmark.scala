package okay.core

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import Cont.*
import Effects.*
import okay.std.*

/**
 * THE EFFECTS, `n` operations each:
 *
 *  - stateGetPut: `get` then `put`, `n` times — answered in place; stateGetPutBang: the same written `Int ! State % Int`
 *  - writerTell: `n` tells — the general clause, the log built on the way back
 *  - throwsRaise: `n` handlers of `throws`, each body raising at once — a handler installed and aborted
 *  - chooseAmong: one choice among `n`, every path — multi-shot, `n` resumptions, `n` results joined
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class EffectsBenchmark:

  @Param(Array("1000"))
  var n: Int = 0

  def counting(i: Int)(using c: Effects, s: Has[State % Int, c.R]): Cont[c.R, c.S, c.S, Int] =
    if i == 0 then get[Int] else get[Int].flatMap(x => put(x + 1)).flatMap(_ => counting(i - 1))

  @Benchmark
  def stateGetPut(): Int = run(state[Int](0)(counting(n)))._1

  /** the same, written `Int ! State % Int`: the effect's path found through the program's context */
  def countingBang(i: Int): Int ! State % Int =
    if i == 0 then get[Int] else get[Int].flatMap(x => put(x + 1)).flatMap(_ => countingBang(i - 1))

  @Benchmark
  def stateGetPutBang(): Int = run(state[Int](0)(countingBang(n)))._1

  def telling(i: Int)(using c: Effects, w: Has[Writer % Int, c.R]): Cont[c.R, c.S, c.S, Unit] =
    if i == 0 then pure(()) else tell(i).flatMap(_ => telling(i - 1))

  @Benchmark
  def writerTell(): Int = run(writer[Int, Unit](telling(n)))._1.length

  def raising(i: Int, acc: Int)(using c: Effects): Cont[c.R, c.S, c.S, Int] =
    if i == 0 then pure(acc)
    else throws[String, Int](raise[String, Int]("no")).flatMap(e => raising(i - 1, acc + e.fold(_.length, identity)))

  @Benchmark
  def throwsRaise(): Int = run(raising(n, 0))

  @Benchmark
  def chooseAmong(): Int = run(choose[Int](among(0 until n).map(_ + 1))).length
