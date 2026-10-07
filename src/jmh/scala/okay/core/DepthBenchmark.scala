package okay.core

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import Cont.*
import Effects.*

/** an operation of the outermost handler, crossing every handler between */
enum Far[+A]:
  case Value[A](a: A) extends Far[A]
/** the effect of a handler between, performed never */
enum Near[+A]:
  case Value[A](a: A) extends Near[A]

/**
 * AN OPERATION THROUGH `depth` HANDLERS: `n` operations of the outermost handler, each crossing `depth` handlers
 * of another effect on its way out, and their folds on the way back. `depth = 0` is ContBenchmark.tailcallHandled.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class DepthBenchmark:

  @Param(Array("10000"))
  var n: Int = 0

  @Param(Array("0", "1", "4", "16"))
  var depth: Int = 0

  def far[A](using c: Effects): Handler[Far, c.R, c.S, A, A] = new Answering[Far, c.R, c.S, A, A]:
    def ret(a: A): A = a
    def value[X](op: Far[X]): X = op match
      case Far.Value(a) => a
  def near[A](using c: Effects): Handler[Near, c.R, c.S, A, A] = new Answering[Near, c.R, c.S, A, A]:
    def ret(a: A): A = a
    def value[X](op: Near[X]): X = op match
      case Near.Value(a) => a

  def loop(i: Int, acc: Int)(using c: Effects, f: Has[Far, c.R]): Cont[c.R, c.S, c.S, Int] =
    if i == 0 then pure(acc) else perform(Far.Value(1)).flatMap(x => loop(i - 1, acc + x))

  /** the body under `d` handlers of `Near`, each in the context of the one outside */
  def nested(d: Int)(using c: Effects, f: Has[Far, c.R]): Cont[c.R, c.S, c.S, Int] =
    if d == 0 then loop(n, 0) else Effects.handle(near)(nested(d - 1))

  @Benchmark
  def forwardedDepth(): Int = run(Effects.handle(far)(nested(depth)))
