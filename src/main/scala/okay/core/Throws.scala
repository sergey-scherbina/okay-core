package okay.core

/** THROWS, an abort: `raise` is an operation whose clause never resumes — the delimiter answers `Left(e)` in
 * place of the body */
enum Throws[E, +A]:
  case Raise[E](e: E) extends Throws[E, Nothing]
type ThrowsOf[E] = [X] =>> Throws[E, X]

/** the failure: a program of any value, which no continuation ever gets */
def raise[E, A](e: E)(using c: Effects, has: Has[ThrowsOf[E], c.R]): Cont[c.R, c.S, c.S, A] =
  perform[ThrowsOf[E], Nothing](Throws.Raise(e)).map(n => n)
/** `throws[E, A](body)`: the value as `Right`, or the first `raise` as `Left` */
def throws[E, A]: ThrowsAt[E, A] = ThrowsAt[E, A]()
final class ThrowsAt[E, A]:
  type Ans = Either[E, A]
  def apply(using c: Effects)(body: Effects.At[ThrowsOf[E] +: c.R, Ans] ?=> Cont[ThrowsOf[E] +: c.R, Ans, Ans, A]): Cont[c.R, c.S, c.S, Ans] =
    Effects.handle(new Handler[ThrowsOf[E], c.R, c.S, A, Ans]:
      def ret(a: A): Ans = Right(a)
      def apply[X](op: Throws[E, X], k: X => Cont[c.R, c.S, c.S, Ans]): Cont[c.R, c.S, c.S, Ans] = op match
        case Throws.Raise(e) => Cont.Return(Left(e)))(body)
