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
  def handle[Q](h: Handler[E, T, Q, A, Ans]): Cont[T, Q, Q, Ans] = fold(body, h.ret, h)
@scala.annotation.tailrec
private def fold[E[_], T <: Row, Q, S, Ans, A](body: Cont[E +: T, S, Ans, A], ret: A => S, h: Handler[E, T, Q, ?, Ans]): Cont[T, Q, Q, Ans] =
  // the common heads are taken here, before `step` is asked — an operation bound to the rest, a resumption's
  // `Bind(Return(x), k)` — so no head is made to be taken apart at once. One method, every loop a tail call: a
  // helper (an inline one too, through its accessor) would make the answered operation's fold a call, a frame each
  body match
    case Bind(i: Inject[e1, r, s, x], g) => i.in match
      case In.Here() => h match
        case an: Answering[E, T, Q, ?, Ans] => fold(g(an.value(i.op)), ret, h)
        case _ => clause(i.op, g, ret, h)
      case In.There(out) => out.resume(i.op, g)(resumed(_, ret, h))
    case Bind(Return(a), g) => fold(g(a), ret, h)
    case _ => body.step match
      case Head.Done(a) => Return(ret(a))
      case Head.Op(op, in, k) => in match
        case In.Here() => h match
          case an: Answering[E, T, Q, ?, Ans] => fold(k(an.value(op)), ret, h)
          case _ => clause(op, k, ret, h)
        case In.There(out) => out.resume(op, k)(resumed(_, ret, h))
      case Head.Cut(f, k) => fold(f(x => Reset(Return(x).flatMap(k), ret)), identity, h)

/** this handler's operation given to its clause, the rest resumed lazily, folded the same way */
private def clause[E[_], T <: Row, Q, S, Ans, A, X](op: E[X], k: X => Cont[E +: T, S, Ans, A], ret: A => S, h: Handler[E, T, Q, ?, Ans]): Cont[T, Q, Q, Ans] =
  h(op, x => Return(x).flatMap(y => resumed(k(y), ret, h)))

/** the fold of a rest resumed later, in a frame of its own: not the loop's tail call */
private def resumed[E[_], T <: Row, Q, S, Ans, A](body: Cont[E +: T, S, Ans, A], ret: A => S, h: Handler[E, T, Q, ?, Ans]): Cont[T, Q, Q, Ans] =
  fold(body, ret, h)

/** `perform(op)`: an operation of `E`, in the context — its row has `E`, its answer is the operation's */
def perform[E[_], X](op: E[X])(using c: Effects, in: In[E, c.R]): Cont[c.R, c.S, c.S, X] = Inject(op, in)
