package okay.core

/** WRITER: a log, every `tell` kept, in order */
enum Writer[W, +A]:
  case Tell[W](w: W) extends Writer[W, Unit]
type WriterOf[W] = [X] =>> Writer[W, X]

def tell[W](w: W)(using c: Effects, has: Has[WriterOf[W], c.R]): Cont[c.R, c.S, c.S, Unit] = perform[WriterOf[W], Unit](Writer.Tell(w))
/** `writer[W, A](body)`: the log and the value */
def writer[W, A]: WriterAt[W, A] = WriterAt[W, A]()
final class WriterAt[W, A]:
  type Ans = (List[W], A)
  def apply(using c: Effects)(body: Effects.At[WriterOf[W] +: c.R, Ans] ?=> Cont[WriterOf[W] +: c.R, Ans, Ans, A]): Cont[c.R, c.S, c.S, Ans] =
    Effects.handle(new Handler[WriterOf[W], c.R, c.S, A, Ans]:
      def ret(a: A): Ans = (Nil, a)
      def apply[X](op: Writer[W, X], k: X => Cont[c.R, c.S, c.S, Ans]): Cont[c.R, c.S, c.S, Ans] = op match
        case Writer.Tell(w) => k(()).map((log, a) => (w :: log, a)))(body)
