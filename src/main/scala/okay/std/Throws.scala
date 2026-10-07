package okay.std

import okay.core.*

/** THROWS, an abort: `raise` is an operation whose clause never resumes — the delimiter answers `Left(e)` in
 * place of the body */
enum Throws[E, +A]:
  case Raise[E](e: E) extends Throws[E, Nothing]

/** the failure: a program of any value, which no continuation ever gets — `Raise` is a `Throws[E, A]` for any
 * `A`, `Throws` covariant, so no map from `Nothing` */
def raise[E, A](e: E)(using c: Effects, has: Has[Throws % E, c.R]): Cont[c.R, c.S, c.S, A] =
  perform[Throws % E, A](Throws.Raise(e))
/** `throws[E, A](body)`: the value as `Right`, or the first `raise` as `Left` */
def throws[E, A]: ThrowsAt[E, A] = ThrowsAt[E, A]()
final class ThrowsAt[E, A]:
  type Ans = Either[E, A]
  def apply(using c: Effects)(body: Effects.At[Throws % E +: c.R, Ans] ?=> Cont[Throws % E +: c.R, Ans, Ans, A]): Cont[c.R, c.S, c.S, Ans] =
    Effects.handle(new Handler[Throws % E, c.R, c.S, A, Ans]:
      def ret(a: A): Ans = Right(a)
      def apply[X](op: Throws[E, X], k: X => Cont[c.R, c.S, c.S, Ans]): Cont[c.R, c.S, c.S, Ans] = op match
        case Throws.Raise(e) => Cont.Return(Left(e)))(body)
