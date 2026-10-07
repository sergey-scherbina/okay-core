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
  /** the operation out at this path, bound to `k`: an operation crossing a delimiter goes on outside so, `k` the
   * rest put back under the delimiter it crossed */
  def bind[Q, X, Z](op: E[X], k: X => Cont[R, Q, Q, Z]): Cont[R, Q, Q, Z] = Cont.Suspend(op, this, k)
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
 * composes the answers end to end; `Return`, `Inject`, `Map` and `Delay` keep them; `Shift` and `Reset` move
 * them. Five nodes are the monad; `Suspend`, `Map`, `Delay` and `Push` are four of its programs as one node each,
 * for speed — an operation with its rest, a map, a tail call, a resumed stack, each without a bind. `step` reads
 * the nine, to the head, with a stack of frames.
 */
enum Cont[R <: Row, I, O, A]:
  case Return[R <: Row, S, A](a: A) extends Cont[R, S, S, A]
  /** an operation of `E`, at its path in the row: a leaf, to whoever folds the tree */
  case Inject[E[_], R <: Row, S, X](op: E[X], in: In[E, R]) extends Cont[R, S, S, X]
  /** an operation of `E` with the rest bound to its value: `Bind(Inject(op, in), k)` as one node — what
   * `flatMap` on an operation makes, and what an operation crossing a delimiter goes on outside as */
  case Suspend[E[_], R <: Row, I, O, X, A](op: E[X], in: In[E, R], k: X => Cont[R, I, O, A]) extends Cont[R, I, O, A]
  /** the answers composed end to end: `m` from `T` to `O`, `f`'s from `I` to `T` */
  case Bind[R <: Row, I, T, O, A, B](m: Cont[R, T, O, A], f: A => Cont[R, I, T, B]) extends Cont[R, I, O, B]
  /** `shift(k => body)`: `k` is the context up to the nearest delimiter, under a delimiter of its own, delivering
   * the answer at the hole, `I`; the body runs under the delimiter in place of the context, at its final answer
   * `O`, and its value is that answer */
  case Shift[R <: Row, I, O, A](f: (A => Cont[R, O, O, I]) => Cont[R, O, O, O]) extends Cont[R, I, O, A]
  /** `m.map(f)`: one node, where `m.flatMap(a => pure(f(a)))` is a bind, a closure and a return */
  case Map[R <: Row, I, O, A, B](m: Cont[R, I, O, A], f: A => B) extends Cont[R, I, O, B]
  /** a program built when the loop gets to it: a tail call one node, where `pure(()).flatMap(_ => c)` is two */
  case Delay[R <: Row, I, O, A](c: () => Cont[R, I, O, A]) extends Cont[R, I, O, A]
  /** the delimiter: the body's value to its initial answer `S` by `ret`, its final answer `O` the delimiter's
   * value outside, at the answer `Q` there. An operation inside goes out as it is, the rest re-delimited */
  case Reset[R <: Row, Q, S, O, A](body: Cont[R, S, O, A], ret: A => S) extends Cont[R, Q, Q, O]
  /** `m` with the rest as a stack already: what applying a captured `k` makes — `Bind(m, k)`, where the loop can
   * take `k` as its stack as it is, not as a function to push. Only `Frames` make it */
  case Push[R <: Row, I, T, O, A, B](m: Cont[R, T, O, A], k: Frames[R, A, I, T, B]) extends Cont[R, I, O, B]

  def flatMap[I2, B](f: A => Cont[R, I2, I, B]): Cont[R, I2, O, B] = this match
    case Inject(op, in) => Suspend(op, in, f)
    case _ => Bind(this, f)
  def map[B](f: A => B): Cont[R, I, O, B] = Map(this, f)

/** WHAT IS BOUND TO A VALUE, the loop's stack: the functions a value goes through, innermost first — each
 * `Bind` the loop enters pushes its function, no tree rebuilt. As a function it is the rest itself, `k`: a head's
 * continuation is the stack as it stands. Applying it runs one function, the rest bound after it — one call,
 * whatever the depth */
enum Frames[R <: Row, A, I, T, B] extends (A => Cont[R, I, T, B]):
  /** nothing bound: the value is the program's */
  case End[R <: Row, S, A]() extends Frames[R, A, S, S, A]
  /** `f` bound to the value, the rest of the stack to its result */
  case Then[R <: Row, A, X, I, T1, T, B](f: A => Cont[R, T1, T, X], next: Frames[R, X, I, T1, B]) extends Frames[R, A, I, T, B]
  /** `f` mapped over the value, the rest of the stack to its result. Applied, it builds the next step and stops —
   * a chain of maps is not run through here, in the caller's stack */
  case Mapped[R <: Row, A, X, I, T, B](f: A => X, next: Frames[R, X, I, T, B]) extends Frames[R, A, I, T, B]

  def apply(a: A): Cont[R, I, T, B] = this match
    case End() => Cont.Return(a)
    case Then(f, next) => next match
      case End() => f(a)
      case _ => Cont.Push(f(a), next)
    case Mapped(f, next) => next match
      case End() => Cont.Return(f(a))
      case _ => Cont.Push(Cont.Return(f(a)), next)

/** a program's head, what the loop over its binds comes to: its value, an operation with the rest, or a capture
 * with the rest. An operation's row has its effect's path in it, so a program over `Pure` has no operation. The
 * rest is LAZY to resume, `Bind(Return(x), k)`: it runs when stepped, not when `k` is applied */
enum Head[R <: Row, I, O, A]:
  case Done[R <: Row, S, A](a: A) extends Head[R, S, S, A]
  case Op[E[_], E2[_], T <: Row, I, O, X, A](op: E[X], in: In[E, E2 +: T], k: X => Cont[E2 +: T, I, O, A]) extends Head[E2 +: T, I, O, A]
  case Cut[R <: Row, I, T, O, X, A](f: (X => Cont[R, O, O, T]) => Cont[R, O, O, O], k: Frames[R, X, I, T, A]) extends Head[R, I, O, A]

object Cont:
  def pure[R <: Row, S, A](a: A): Cont[R, S, S, A] = Return(a)
  def inject[R <: Row, S, E[_], X](op: E[X])(using in: In[E, R]): Cont[R, S, S, X] = Inject(op, in)
  def shift[R <: Row, I, O, A](f: (A => Cont[R, O, O, I]) => Cont[R, O, O, O]): Cont[R, I, O, A] = Shift(f)
  /** `delay(c)`: the program `c`, built when the loop gets to it — a tail call in constant stack. Inline, so the
   * thunk closes over `c`'s own variables, not over a by-name wrapper of them */
  inline def delay[R <: Row, I, O, A](inline c: Cont[R, I, O, A]): Cont[R, I, O, A] = Delay(() => c)
  extension [R <: Row, S, O](body: Cont[R, S, O, S])
    /** `body.reset`: the body's value is its initial answer, the delimiter's value its final one */
    def reset[Q]: Cont[R, Q, Q, O] = Reset(body, identity)

  extension [R <: Row, I, O, A](c: Cont[R, I, O, A])
    /** to the head: binds reassociated and followed, a delimiter entered, in constant stack */
    def step: Head[R, I, O, A] = loop0(c)

  /** the loop while nothing is bound under the program — the common case, a tail call's or a resumption's — the
   * common heads taken here with no stack made; `loop` from the first bind, map or delimiter on, with its stack.
   * A head here has the program's own rest */
  @tailrec private def loop0[R <: Row, I, O, A](c: Cont[R, I, O, A]): Head[R, I, O, A] = c match
    case Return(a) => Head.Done(a)
    case Suspend(op, in, g) => in.op(op, g)
    case Delay(t) => loop0(t())
    case Push(m, ks) => loop(m, ks)
    case Bind(m, g) => m match
      case Return(a) => loop0(g(a))
      case Delay(t) => loop0(Bind(t(), g))
      case _ => loop(m, Frames.Then(g, Frames.End()))
    case _ => loop(c, Frames.End())

  @tailrec private def loop[R <: Row, I, T, O, A, B](c: Cont[R, T, O, A], k: Frames[R, A, I, T, B]): Head[R, I, O, B] = c match
    case Return(a) => k match
      case Frames.End() => Head.Done(a)
      case Frames.Then(f, next) => loop(f(a), next)
      case Frames.Mapped(f, next) => next match
        case Frames.End() => Head.Done(f(a))
        case Frames.Then(g, rest) => loop(g(f(a)), rest)
        case Frames.Mapped(g, rest) => loop(Return(g(f(a))), rest)
    case Inject(op, in) => in.op(op, k)
    case Suspend(op, in, g) => k match
      case Frames.End() => in.op(op, g)
      case _ => in.op(op, Frames.Then(g, k))
    case Shift(f) => Head.Cut(f, k)
    case Reset(body, ret) => loop(delimited(body, ret), k)
    case Delay(t) => loop(t(), k)
    case Map(m, f) => loop(m, Frames.Mapped(f, k))
    case Push(m, ks) => k match
      case Frames.End() => loop(m, ks)
      case _ => loop(m, Frames.Then(ks, k))
    case Bind(m, g) => m match
      case Return(a) => loop(g(a), k)
      case _ => loop(m, Frames.Then(g, k))

  /** the delimiter's body to its head: a value is answered by `ret`; an operation goes out, with the rest of the
   * body under the delimiter again; a capture's body goes under the delimiter in place of the rest, with the
   * rest under a delimiter of its own, `ret` included, as `k` */
  private[core] def delimited[R <: Row, Q, S, O, A](body: Cont[R, S, O, A], ret: A => S): Cont[R, Q, Q, O] =
    body.step match
      case Head.Done(a) => Return(ret(a))
      case Head.Op(op, in, k) => in.bind(op, Redelimit(k, ret))
      case Head.Cut(f, k) => f(under(k, ret)) match
        // a body under a delimiter already — `k(x)` in tail position, most often — is not put under a second: the
        // two would nest, each the next capture's `step` inside the last's, a stack frame a capture
        case Reset(b, r) => Reset(b, r)
        case b => Reset(b, identity)

  /** a captured rest as `k`: applied, the stack it holds resumed, under a delimiter of its own with `ret` */
  private[core] def under[R <: Row, Q, S, T, X, A](k: Frames[R, X, S, T, A], ret: A => S): X => Cont[R, Q, Q, T] =
    x => Reset(Push(Return(x), k), ret)

  /** the rest of a delimiter's body, once the operation it waits on is answered outside: under the delimiter
   * again. One object, where a closure over a closure was two */
  private final class Redelimit[R <: Row, Q, S, O, A, X](k: X => Cont[R, S, O, A], ret: A => S) extends (X => Cont[R, Q, Q, O]):
    def apply(x: X): Cont[R, Q, Q, O] = Reset(k(x), ret)

  extension [A](c: Cont[Pure, A, A, A])
    /** a program at the top, nothing to perform, its answer its value: the value — the top is a delimiter; a
     * program that moves the answer goes under a `reset` first */
    def value: A = c.step match
      case Head.Done(a) => a
      case Head.Cut(f, k) => f(under(k, identity)).value
