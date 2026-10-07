package okay.core

import scala.annotation.tailrec

/** a ROW: the effects a program may perform, nominal — `Ask +: Say +: Pure`. Nominal, not a union: a union
 * value cannot be told apart without a cast, and a union type cannot be taken apart by the compiler; a class
 * can, so every walk here is total, no claim, no cast, no runtime test of an operation's class */
sealed trait Row
/** the effect `E` in front of the row `T` */
final class +:[E[_], T <: Row] extends Row
/** the empty row: a program over it performs nothing */
sealed trait Pure extends Row
/** the row grown by an effect, read left to right as the handlers are nested, outside in: `Pure + Ask + Say` is
 * `Say +: Ask +: Pure`, `Say` handled nearest */
type +[R <: Row, E[_]] = E +: R

/** the effect `E` IN the row `R`: a path to it, the compiler builds it — the first, by priority. An operation
 * carries its path; a handler follows it: at `Here`, the operation is the handler's; at `There`, it goes on
 * outside, one effect off the row */
sealed trait In[E[_], R <: Row]:
  /** the head for an operation at this path: the row has the effect, the path says so */
  def op[I, O, X, A](op: E[X], k: X => Cont[R, I, O, A]): Head[R, I, O, A]
  /** the operation out at this path, and the rest of the body (over its own row, `R2`) resumed with the value
   * under `rest` — re-delimited, or folded by the handler whose delimiter the operation crossed. No lazy wrapper:
   * the continuation is applied only by whoever answers the operation outside, while folding (answered in place)
   * or inside a resumption of its own (a clause's `k`, lazy already) */
  def resume[R2 <: Row, Q, I, O, X, A, Z](op: E[X], k: X => Cont[R2, I, O, A])(rest: Cont[R2, I, O, A] => Cont[R, Q, Q, Z]): Cont[R, Q, Q, Z] =
    Cont.Inject(op, this).flatMap(x => rest(k(x)))
object In extends InLow:
  final case class Here[E[_], T <: Row]() extends In[E, E +: T]:
    def op[I, O, X, A](op: E[X], k: X => Cont[E +: T, I, O, A]): Head[E +: T, I, O, A] = Head.Op(op, this, k)
  final case class There[E[_], E2[_], T <: Row](in: In[E, T]) extends In[E, E2 +: T]:
    def op[I, O, X, A](op: E[X], k: X => Cont[E2 +: T, I, O, A]): Head[E2 +: T, I, O, A] = Head.Op(op, this, k)
  given here[E[_], T <: Row]: In[E, E +: T] = Here()
sealed trait InLow:
  given there[E[_], E2[_], T <: Row](using in: In[E, T]): In[E, E2 +: T] = In.There(in)

/**
 * THE MONAD OF DELIMITED CONTINUATIONS over a row `R`, one level: `Cont[R, I, O, A]` is a program of value `A`
 * which, with a continuation answering `I`, answers `O` — Danvy–Filinski's `(A => I) => O`, the answer type
 * modified. An operation is a leaf, `Inject`, with the path to its effect in the row, and means nothing by
 * itself — a handler is not in the tree; it is a fold over the tree's head (`handle`, in Effects.scala). `Bind`
 * composes the answers end to end; `Return` and `Inject` keep them; `Shift` and `Reset` move them. `step` reads
 * the five nodes, to the head.
 */
enum Cont[R <: Row, I, O, A]:
  case Return[R <: Row, S, A](a: A) extends Cont[R, S, S, A]
  /** an operation of `E`, at its path in the row: a leaf, to whoever folds the tree */
  case Inject[E[_], R <: Row, S, X](op: E[X], in: In[E, R]) extends Cont[R, S, S, X]
  /** the answers composed end to end: `m` from `T` to `O`, `f`'s from `I` to `T` */
  case Bind[R <: Row, I, T, O, A, B](m: Cont[R, T, O, A], f: A => Cont[R, I, T, B]) extends Cont[R, I, O, B]
  /** `shift(k => body)`: `k` is the context up to the nearest delimiter, under a delimiter of its own, delivering
   * the answer at the hole, `I`; the body runs under the delimiter in place of the context, at its final answer
   * `O`, and its value is that answer */
  case Shift[R <: Row, I, O, A](f: (A => Cont[R, O, O, I]) => Cont[R, O, O, O]) extends Cont[R, I, O, A]
  /** the delimiter: the body's value to its initial answer `S` by `ret`, its final answer `O` the delimiter's
   * value outside, at the answer `Q` there. An operation inside goes out as it is, the rest re-delimited */
  case Reset[R <: Row, Q, S, O, A](body: Cont[R, S, O, A], ret: A => S) extends Cont[R, Q, Q, O]

  def flatMap[I2, B](f: A => Cont[R, I2, I, B]): Cont[R, I2, O, B] = Bind(this, f)
  def map[B](f: A => B): Cont[R, I, O, B] = Bind(this, a => Return(f(a)))

/** a program's head, what the loop over its binds comes to: its value, an operation with the rest, or a capture
 * with the rest. An operation's row has its effect's path in it, so a program over `Pure` has no operation. The
 * rest is LAZY to resume, `Bind(Return(x), k)`: it runs when stepped, not when `k` is applied */
enum Head[R <: Row, I, O, A]:
  case Done[R <: Row, S, A](a: A) extends Head[R, S, S, A]
  case Op[E[_], E2[_], T <: Row, I, O, X, A](op: E[X], in: In[E, E2 +: T], k: X => Cont[E2 +: T, I, O, A]) extends Head[E2 +: T, I, O, A]
  case Cut[R <: Row, I, T, O, X, A](f: (X => Cont[R, O, O, T]) => Cont[R, O, O, O], k: X => Cont[R, I, T, A]) extends Head[R, I, O, A]

object Cont:
  def pure[R <: Row, S, A](a: A): Cont[R, S, S, A] = Return(a)
  def inject[R <: Row, S, E[_], X](op: E[X])(using in: In[E, R]): Cont[R, S, S, X] = Inject(op, in)
  def shift[R <: Row, I, O, A](f: (A => Cont[R, O, O, I]) => Cont[R, O, O, O]): Cont[R, I, O, A] = Shift(f)
  extension [R <: Row, S, O](body: Cont[R, S, O, S])
    /** `body.reset`: the body's value is its initial answer, the delimiter's value its final one */
    def reset[Q]: Cont[R, Q, Q, O] = Reset(body, identity)

  extension [R <: Row, I, O, A](c: Cont[R, I, O, A])
    /** to the head: binds reassociated and followed, a delimiter entered, in constant stack */
    def step: Head[R, I, O, A] = loop(c)

  @tailrec private def loop[R <: Row, I, O, A](c: Cont[R, I, O, A]): Head[R, I, O, A] = c match
    case Return(a) => Head.Done(a)
    case Inject(op, in) => in.op(op, a => Return[R, I, A](a))
    case Shift(f) => Head.Cut(f, a => Return(a))
    case Reset(body, ret) => loop(delimited(body, ret))
    case Bind(m, g) => m match
      case Return(a) => loop(g(a))
      case Inject(op, in) => in.op(op, g)
      case Shift(f) => Head.Cut(f, g)
      case Reset(body, ret) => loop(Bind(delimited(body, ret), g))
      case Bind(m2, f) => loop(Bind(m2, a => Bind(f(a), g)))

  /** the delimiter's body to its head: a value is answered by `ret`; an operation goes out, with the rest of the
   * body under the delimiter again; a capture's body goes under the delimiter in place of the rest, with the
   * rest under a delimiter of its own, `ret` included, as `k` */
  private def delimited[R <: Row, Q, S, O, A](body: Cont[R, S, O, A], ret: A => S): Cont[R, Q, Q, O] =
    body.step match
      case Head.Done(a) => Return(ret(a))
      case Head.Op(op, in, k) => in.resume(op, k)(Reset(_, ret))
      case Head.Cut(f, k) => Reset(f(x => Reset(Return(x).flatMap(k), ret)), identity)

  extension [A](c: Cont[Pure, A, A, A])
    /** a program at the top, nothing to perform, its answer its value: the value — the top is a delimiter; a
     * program that moves the answer goes under a `reset` first */
    def value: A = c.step match
      case Head.Done(a) => a
      case Head.Cut(f, k) => f(x => k(x).reset).value
