package okay.core

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import Cont.*
import Effects.*
import okay.std.*

/**
 * A FIBONACCI GENERATOR, the first `n` numbers (`Long`, wrapping past the 92nd — the work is the same):
 *
 *  - fibIterator: Scala's own `Iterator`, no monad — the floor
 *  - fibPure: the same recursion in `Cont`, no effect — the monad's binds alone
 *  - fibCollect: a `yield_` per number under `collect` — an operation, a clause, the rest resumed at once
 *  - fibGenerate: an infinite body under `generate`, `n` numbers pulled one at a time — each pull a run of its
 *    own to the next `yield_`, the rest captured as the generator's next step
 *
 * Every result is the sum of the numbers, so none is computed in vain.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class FibBenchmark:

  /** the numbers of a run: the report divides a run's time by it, for the time of one number */
  @Param(Array("10000"))
  var n: Int = 0

  @Benchmark
  def fibIterator(): Long =
    Iterator.iterate((0L, 1L))((a, b) => (b, a + b)).map(_._1).take(n).sum

  def sums(i: Int, a: Long, b: Long, acc: Long): Cont[Pure, Long, Long, Long] =
    if i == 0 then pure(acc) else pure(()).flatMap(_ => sums(i - 1, b, a + b, acc + a))

  @Benchmark
  def fibPure(): Long = sums(n, 0, 1, 0).value

  def fibs(i: Int, a: Long, b: Long)(using c: Effects, e: Has[Emit % Long, c.R]): Cont[c.R, c.S, c.S, Unit] =
    if i == 0 then pure(()) else yield_(a).flatMap(_ => fibs(i - 1, b, a + b))

  @Benchmark
  def fibCollect(): Long = run(collect[Long, Unit](fibs(n, 0, 1)))._1.sum

  def forever(a: Long, b: Long)(using c: Effects, e: Has[Emit % Long, c.R]): Cont[c.R, c.S, c.S, Unit] =
    yield_(a).flatMap(_ => forever(b, a + b))

  type G = Gen[Long, Pure]

  @Benchmark
  def fibGenerate(): Long =
    var g: Cont[Pure, G, G, G] = generate[Long](using At[Pure, G]())(forever(0, 1))
    var i = 0
    var sum = 0L
    while i < n do
      g.value match
        case Gen.Next(w, rest) => sum += w; g = rest; i += 1
        case Gen.Done() => i = n
    sum
