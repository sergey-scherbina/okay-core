package okay.core

import Cont.*

/** THE EFFECTS a program is written with, its context: the row `R` there, and the answer `S` at this point — what an operation
 * and a `shift` take their types from, by its members: `c.R`, `c.S`. A delimiter gives its body one, `At`: a
 * handler's has the handler's effect in front of what the handler leaves, `E +: c.R`, so the row is built by
 * the handlers, outside in, and never named. The nearest context wins — a nested scope's given is preferred. A
 * marker, no fields */
sealed trait Effects:
  type R <: Row
  type S

/** `E` is in the row of the context `R`: what an operation of `E` needs of it */
type Has[E[_], R <: Row] = In[E, R]

object Effects:
  /** the context at the row `R0`, the answer `S0` */
  final class At[R0 <: Row, S0] extends Effects:
    type R = R0
    type S = S0

  /** `run(body)`: the body at the top, nothing to perform, its answer its value */
  def run[A](body: At[Pure, A] ?=> Cont[Pure, A, A, A]): A = body(using At()).value

  /** `reset[S](body)`: the body written at the answer `S`, its value; the delimiter answers what the body moved
   * the answer to, `O` */
  def reset[S]: ResetAt[S] = ResetAt[S]()
  final class ResetAt[S]:
    def apply[O](using c: Effects)(body: At[c.R, S] ?=> Cont[c.R, S, O, S]): Cont[c.R, c.S, c.S, O] =
      body(using At()).reset

  /** `shift[A, O](k => body)`: the hole's value `A` and the answer the delimiter is moved to, `O`; the hole's
   * answer is the context's. The body is written at `O`, under the delimiter */
  def shift[A, O]: ShiftAt[A, O] = ShiftAt[A, O]()
  final class ShiftAt[A, O]:
    def apply(using c: Effects)(f: At[c.R, O] ?=> (A => Cont[c.R, O, O, c.S]) => Cont[c.R, O, O, O]): Cont[c.R, c.S, O, A] =
      Cont.shift(f(using At()))

  /** `handle(h)(body)` in context: the body written in the handler's context, over `E +: c.R`, at its answer */
  def handle[E[_], A, Ans](using c: Effects)(h: Handler[E, c.R, c.S, A, Ans])
            (body: At[E +: c.R, Ans] ?=> Cont[E +: c.R, Ans, Ans, A]): Cont[c.R, c.S, c.S, Ans] =
    body(using At()).handle(h)

/** a handler of the effect `E`, over the row `E +: T`, leaving `T`, written at the answer `Q` outside its
 * delimiter: the body's value `A` to the answer `Ans`; each operation with the rest of the body up to the
 * delimiter, `k`, deep — the handler stays in force through a resumption. Not a node: `handle` folds the tree
 * with it; an operation of another effect goes on outside, its path one shorter */
trait Handler[E[_], T <: Row, Q, A, Ans]:
  def ret(a: A): Ans
  def apply[X](op: E[X], k: X => Cont[T, Q, Q, Ans]): Cont[T, Q, Q, Ans]

/** a TAIL-RESUMPTIVE handler: each clause is `k(value(op))`, the value of the operation alone — so the operation is
 * ANSWERED IN PLACE: the fold goes on with the rest of the body at once, in its own loop, no resumption built,
 * nothing handed out (Koka's, Effekt's optimisation; here by the handler's declaration) */
trait Answering[E[_], T <: Row, Q, A, Ans] extends Handler[E, T, Q, A, Ans]:
  def value[X](op: E[X]): X
  final def apply[X](op: E[X], k: X => Cont[T, Q, Q, Ans]): Cont[T, Q, Q, Ans] = k(value(op))

extension [E[_], T <: Row, A, Ans](body: Cont[E +: T, Ans, Ans, A])
  /** `body.handle(h)`: the tree folded at its head — a value answered by `ret`, an operation of `E` by its
   * clause with the rest of the body folded the same way, lazily, another effect's forwarded; a capture's body
   * folded in place of the rest, the rest under a delimiter of its own as `k`. The body is read up to its first
   * operation here, the rest when resumed */
  def handle[Q](h: Handler[E, T, Q, A, Ans]): Cont[T, Q, Q, Ans] = fold0(body, h.ret, h)

/** the fold while nothing is bound under the body — a tail call's, a resumption's — the common heads taken here
 * with no stack made; `fold` from the first bind or map on, with its stack. One method each, every loop a tail
 * call: a helper (an inline one too, through its accessor) would make the answered operation's fold a call, a
 * frame each */
@scala.annotation.tailrec
private def fold0[E[_], T <: Row, Q, S, Ans, A](body: Cont[E +: T, S, Ans, A], ret: A => S, h: Handler[E, T, Q, ?, Ans]): Cont[T, Q, Q, Ans] =
  body match
    case o: Suspend[e1, r, i, so, x, a] => o.in match
      case In.Here() => h match
        case an: Answering[E, T, Q, ?, Ans] => fold0(o.k(an.value(o.op)), ret, h)
        case _ => clause(o.op, o.k, ret, h)
      case In.There(out) => out.bind(o.op, Refold(o.k, ret, h))
    case Return(a) => Return(ret(a))
    case Bind(Return(a), g) => fold0(g(a), ret, h)
    case Delay(t) => fold0(t(), ret, h)
    case Push(m, ks) => fold(m, ks, ret, h)
    case _ => fold(body, Frames.End(), ret, h)

/** THE FOLD over the body's binds with a stack of its own, `k`, as `loop`'s: an operation of `E` answered in
 * place goes on in the same loop with the same stack — no head made, no stack rebuilt; a clause, a forwarded
 * operation, a capture get the stack as the rest */
@scala.annotation.tailrec
private def fold[E[_], T <: Row, Q, S, Ans, A, T1, X](c: Cont[E +: T, T1, Ans, X], k: Frames[E +: T, X, S, T1, A], ret: A => S,
                                                    h: Handler[E, T, Q, ?, Ans]): Cont[T, Q, Q, Ans] =
  c match
    case o: Suspend[e1, r, i, so, x, a] => o.in match
      case In.Here() => h match
        case an: Answering[E, T, Q, ?, Ans] => fold(o.k(an.value(o.op)), k, ret, h)
        case _ => k match
          case Frames.End() => clause(o.op, o.k, ret, h)
          case _ => clause(o.op, Frames.Then(o.k, k), ret, h)
      case In.There(out) => k match
        case Frames.End() => out.bind(o.op, Refold(o.k, ret, h))
        case _ => out.bind(o.op, Refold(Frames.Then(o.k, k), ret, h))
    case Return(a) => k match
      case Frames.End() => Return(ret(a))
      case Frames.Then(f, next) => fold(f(a), next, ret, h)
      case Frames.Mapped(f, next) => next match
        case Frames.End() => Return(ret(f(a)))
        case Frames.Then(g, rest) => fold(g(f(a)), rest, ret, h)
        case Frames.Mapped(g, rest) => fold(Return(g(f(a))), rest, ret, h)
    case i: Inject[e1, r, s, x] => i.in match
      case In.Here() => h match
        case an: Answering[E, T, Q, ?, Ans] => fold(Return(an.value(i.op)), k, ret, h)
        case _ => clause(i.op, k, ret, h)
      case In.There(out) => out.bind(i.op, Refold(k, ret, h))
    case Bind(m, g) => m match
      case Return(a) => fold(g(a), k, ret, h)
      case _ => fold(m, Frames.Then(g, k), ret, h)
    case Map(m, f) => fold(m, Frames.Mapped(f, k), ret, h)
    case Delay(t) => fold(t(), k, ret, h)
    case Push(m, ks) => k match
      case Frames.End() => fold(m, ks, ret, h)
      case _ => fold(m, Frames.Then(ks, k), ret, h)
    case Shift(f) => fold(f(under(k, ret)), Frames.End(), identity, h)
    case Reset(b, r) => fold(delimited(b, r), k, ret, h)

/** this handler's operation given to its clause, with the rest as `k`, resumed lazily */
private def clause[E[_], T <: Row, Q, S, Ans, A, X](op: E[X], k: X => Cont[E +: T, S, Ans, A], ret: A => S, h: Handler[E, T, Q, ?, Ans]): Cont[T, Q, Q, Ans] =
  h(op, Resume(k, ret, h))

/** the rest of a body folded again by its handler, once the operation it waits on is answered outside: the
 * continuation a forwarded operation goes out with. One object, where a closure over a closure was two. Applied
 * by whoever answers the operation outside — while folding, or inside a resumption of its own, lazy already */
private final class Refold[E[_], T <: Row, Q, S, Ans, A, X](k: X => Cont[E +: T, S, Ans, A], ret: A => S, h: Handler[E, T, Q, ?, Ans])
  extends (X => Cont[T, Q, Q, Ans]):
  def apply(x: X): Cont[T, Q, Q, Ans] = fold0(k(x), ret, h)

/** a clause's `k`: the rest of the body, folded again by the handler, LAZILY — a `Delay`, built when stepped, so a
 * clause that keeps `k` (a generator's next step) runs nothing by making it. One object and its delay */
private final class Resume[E[_], T <: Row, Q, S, Ans, A, X](k: X => Cont[E +: T, S, Ans, A], ret: A => S, h: Handler[E, T, Q, ?, Ans])
  extends (X => Cont[T, Q, Q, Ans]):
  def apply(x: X): Cont[T, Q, Q, Ans] = Delay(() => fold0(k(x), ret, h))

/** `perform(op)`: an operation of `E`, in the context — its row has `E`, its answer is the operation's */
def perform[E[_], X](op: E[X])(using c: Effects, in: In[E, c.R]): Cont[c.R, c.S, c.S, X] = Inject(op, in)
