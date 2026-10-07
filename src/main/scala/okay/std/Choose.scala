package okay.std

import okay.core.*

/** CHOICE, multi-shot: `among(as)` is answered once per element, the continuation resumed for each, and the
 * delimiter answers every value the body came to, in order */
enum Choose[+A]:
  case Among[A](as: Seq[A]) extends Choose[A]

def among[A](as: Seq[A])(using c: Effects, has: Has[Choose, c.R]): Cont[c.R, c.S, c.S, A] = perform[Choose, A](Choose.Among(as))
/** `choose[A](body)`: every value of the body, one per path through its choices */
def choose[A]: ChooseAt[A] = ChooseAt[A]()
final class ChooseAt[A]:
  def apply(using c: Effects)(body: Effects.At[Choose +: c.R, Seq[A]] ?=> Cont[Choose +: c.R, Seq[A], Seq[A], A]): Cont[c.R, c.S, c.S, Seq[A]] =
    Effects.handle(new Handler[Choose, c.R, c.S, A, Seq[A]]:
      def ret(a: A): Seq[A] = Vector(a)
      // the paths' values joined in a Vector: appending a path's to the ones before copies nothing of them — a
      // List or a Seq's `++` copied all the values so far for each path, quadratic in their number
      def apply[X](op: Choose[X], k: X => Cont[c.R, c.S, c.S, Seq[A]]): Cont[c.R, c.S, c.S, Seq[A]] = op match
        case Choose.Among(as) =>
          as.foldLeft(Cont.pure[c.R, c.S, Seq[A]](Vector.empty))((acc, x) => acc.flatMap(v => k(x).map(v ++ _))))(body)
