package okay.std

import okay.core.*

import java.util.concurrent.atomic.AtomicReference
import scala.util.control.NonFatal

/** ASYNC: a program that waits — on a computation suspended, `Run`, or on a callback, `Await`, whose `Left` is the
 * error channel: it fails the whole run. Handled at the top, by `runAsync`: the run goes on in the callback's
 * thread when the callback comes later, in place when it came already — so a callback answered at once is a
 * loop, not a frame */
enum Async[+A]:
  case Run[A](run: () => A) extends Async[A]
  case Await[A](register: (Either[Throwable, A] => Unit) => Unit) extends Async[A]

/** a computation, suspended — run when the run gets to it */
def async[A](a: => A)(using c: Effects, has: Has[Async, c.R]): Cont[c.R, c.S, c.S, A] = perform[Async, A](Async.Run(() => a))
/** the value a callback is called with, once */
def await[A](register: (Either[Throwable, A] => Unit) => Unit)(using c: Effects, has: Has[Async, c.R]): Cont[c.R, c.S, c.S, A] =
  perform[Async, A](Async.Await(register))

object Async:
  /** what is under `Async` at the top: the failure, `Throws % Throwable` — a `Left` at a callback, a throw in a
   * computation, or the program's own `raise` */
  type Fails = Throws % Throwable +: Pure

  /** where a callback's value is, against the run that waits on it: not yet; come while the run registered, to be
   * taken in place; or the run gone, to be resumed by the callback */
  private enum Slot[+A]:
    case Waiting
    case Ready(r: Either[Throwable, A])
    case Detached

  /** `runAsync(body)(done)`: the body run, `done` given its value or its failure, once — in this thread, or in a
   * callback's */
  def runAsync[A](body: Effects.At[Async +: Fails, Unit] ?=> Free[Async +: Fails, A])(done: Either[Throwable, A] => Unit): Unit =
    given Effects.At[Fails, Unit] = Effects.At()
    // the run up to its end or to a wait, a failure raised in it `done`'s, the rest of the run aborted
    def go(c: Free[Fails, Unit]): Unit =
      c.handle(new Handler[Throws % Throwable, Pure, Unit, Unit, Unit]:
        def ret(u: Unit): Unit = ()
        def apply[X](op: Throws[Throwable, X], k: X => Free[Pure, Unit]): Free[Pure, Unit] = op match
          case Throws.Raise(e) => done(Left(e)); Cont.pure(())).value
    val h = new Handler[Async, Fails, Unit, A, Unit]:
      def ret(a: A): Unit = done(Right(a))
      def apply[X](op: Async[X], k: X => Free[Fails, Unit]): Free[Fails, Unit] =
        def resume(r: Either[Throwable, X]): Free[Fails, Unit] = r.fold(raise[Throwable, Unit], k)
        op match
          case Run(run) => resume(try Right(run()) catch { case NonFatal(e) => Left(e) })
          case Await(register) =>
            val slot = AtomicReference[Slot[X]](Slot.Waiting)
            register: r =>
              if !slot.compareAndSet(Slot.Waiting, Slot.Ready(r)) then go(resume(r))
            if slot.compareAndSet(Slot.Waiting, Slot.Detached) then Cont.pure(())
            else slot.get match
              case Slot.Ready(r) => resume(r)
              case _ => Cont.pure(())
    go(body(using Effects.At()).handle(h))

  /** `run(body)`: the body's value, this thread waiting for it — at the edge of a program only; its failure raised
   * in the context */
  def run[A](body: Effects.At[Async +: Fails, Unit] ?=> Free[Async +: Fails, A])
            (using c: Effects, has: Has[Throws % Throwable, c.R]): Cont[c.R, c.S, c.S, A] =
    Cont.Delay: () =>
      val result = java.util.concurrent.CompletableFuture[Either[Throwable, A]]()
      runAsync(body)(result.complete)
      result.get().fold(raise[Throwable, A], Cont.pure)
