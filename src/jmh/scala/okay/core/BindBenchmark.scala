package okay.core

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import Cont.*

/**
 * THE MONAD ALONE, no effect: how a chain of `n` binds or maps is read by the loop.
 *
 *  - tailcallDelay: `isEven`/`isOdd` through `delay` — a tail call a node (beside ContBenchmark.tailcallPure,
 *    the same through `pure(()).flatMap`)
 *  - leftBinds: `n` binds nested to the LEFT, `((p >>= f) >>= f) >>= …` — each a level the loop takes apart
 *  - leftMaps: `n` maps nested to the left, `p.map(f).map(f)…`
 *  - rightMaps: `n` maps nested to the right, a recursion whose rest maps, `go(i) = delay(go(i - 1)).map(_ + 1)`
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class BindBenchmark:

  /** the binds or maps of a run: the report divides a run's time by it */
  @Param(Array("10000"))
  var n: Int = 0

  def isEven(i: Int): Cont[Pure, Boolean, Boolean, Boolean] = if i == 0 then pure(true) else delay(isOdd(i - 1))
  def isOdd(i: Int): Cont[Pure, Boolean, Boolean, Boolean] = if i == 0 then pure(false) else delay(isEven(i - 1))

  @Benchmark
  def tailcallDelay(): Boolean = isEven(n).value

  @Benchmark
  def leftBinds(): Int =
    var p: Cont[Pure, Int, Int, Int] = pure(0)
    var i = 0
    while i < n do
      p = p.flatMap(x => pure(x + 1))
      i += 1
    p.value

  @Benchmark
  def leftMaps(): Int =
    var p: Cont[Pure, Int, Int, Int] = pure(0)
    var i = 0
    while i < n do
      p = p.map(_ + 1)
      i += 1
    p.value

  def go(i: Int): Cont[Pure, Int, Int, Int] = if i == 0 then pure(0) else delay(go(i - 1)).map(_ + 1)

  @Benchmark
  def rightMaps(): Int = go(n).value
