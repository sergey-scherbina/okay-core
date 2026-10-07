package okay.core

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import Cont.*
import Effects.*

/** an operation with its own answer, forwarded or answered by its handler */
enum Tick[+A]:
  case Value[A](a: A) extends Tick[A]
/** the effect of the inner handler, performed never: every `Tick` crosses it */
enum Skip[+A]:
  case Value[A](a: A) extends Skip[A]

/**
 * MUTUAL TAIL RECURSION, `isEven`/`isOdd` over `n` (the basis: okay-cont's `tailcallChain`): a bind per call,
 * no node for a delay — `pure(()).flatMap(_ => …)` is the delay, read by the loop in constant stack.
 *
 *  - tailcallPure: no effect — the loop over binds alone (BindBenchmark.tailcallDelay: the same through `delay`)
 *  - tailcallHandled: a `Tick` per call, answered IN PLACE by the nearest handler (`Answering`): the fold goes
 *    on with the rest, no resumption built
 *  - tailcallHandledGeneral: the same handler with its clause `k(a)` written out: an operation, its clause, the
 *    rest resumed lazily, every call
 *  - tailcallForwarded: the same `Tick` per call, through an inner handler of another effect to the outer:
 *    the operation goes out by its path, re-folded by the inner on the way back
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class ContBenchmark:

  /** the calls of a run: the report divides a run's time by it, for the time of one call */
  @Param(Array("10000"))
  var n: Int = 0

  def isEven(n: Int): Cont[Pure, Boolean, Boolean, Boolean] = if n == 0 then pure(true) else pure(()).flatMap(_ => isOdd(n - 1))
  def isOdd(n: Int): Cont[Pure, Boolean, Boolean, Boolean] = if n == 0 then pure(false) else pure(()).flatMap(_ => isEven(n - 1))

  @Benchmark
  def tailcallPure(): Boolean = isEven(n).value

  def isEvenT(n: Int)(using c: Effects, t: Has[Tick, c.R]): Cont[c.R, c.S, c.S, Boolean] =
    if n == 0 then pure(true) else perform(Tick.Value(n)).flatMap(_ => isOddT(n - 1))
  def isOddT(n: Int)(using c: Effects, t: Has[Tick, c.R]): Cont[c.R, c.S, c.S, Boolean] =
    if n == 0 then pure(false) else perform(Tick.Value(n)).flatMap(_ => isEvenT(n - 1))

  /** `Tick` answered with its value: answered in place, its clause `k(a)` */
  def ticking[Q, A](using c: Effects): Handler[Tick, c.R, c.S, A, A] = new Answering[Tick, c.R, c.S, A, A]:
    def ret(a: A): A = a
    def value[X](op: Tick[X]): X = op match
      case Tick.Value(a) => a
  /** the same handler with its clause written out, `k(a)`: the general road, a resumption built per operation */
  def tickingK[Q, A](using c: Effects): Handler[Tick, c.R, c.S, A, A] = new Handler[Tick, c.R, c.S, A, A]:
    def ret(a: A): A = a
    def apply[X](op: Tick[X], k: X => Cont[c.R, c.S, c.S, A]): Cont[c.R, c.S, c.S, A] = op match
      case Tick.Value(a) => k(a)
  /** the inner handler, of `Skip`: crossed by every `Tick` */
  def skipping[A](using c: Effects): Handler[Skip, c.R, c.S, A, A] = new Answering[Skip, c.R, c.S, A, A]:
    def ret(a: A): A = a
    def value[X](op: Skip[X]): X = op match
      case Skip.Value(a) => a

  @Benchmark
  def tailcallHandled(): Boolean = run(Effects.handle(ticking)(isEvenT(n)))

  @Benchmark
  def tailcallHandledGeneral(): Boolean = run(Effects.handle(tickingK)(isEvenT(n)))

  @Benchmark
  def tailcallForwarded(): Boolean = run(Effects.handle(ticking)(Effects.handle(skipping)(isEvenT(n))))
