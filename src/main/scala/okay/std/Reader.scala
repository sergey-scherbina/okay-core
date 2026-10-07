package okay.std

import okay.core.*

/** READER: the environment, answered in place */
enum Reader[E, +A]:
  case Ask[E]() extends Reader[E, E]

def ask[E](using c: Effects, has: Has[Reader % E, c.R]): Cont[c.R, c.S, c.S, E] = perform[Reader % E, E](Reader.Ask[E]())
/** `reader[A](e)(body)`: every `ask` answered `e` */
def reader[A]: ReaderAt[A] = ReaderAt[A]()
final class ReaderAt[A]:
  def apply[E](e: E)(using c: Effects)(body: Effects.At[Reader % E +: c.R, A] ?=> Cont[Reader % E +: c.R, A, A, A]): Cont[c.R, c.S, c.S, A] =
    Effects.handle(new Answering[Reader % E, c.R, c.S, A, A]:
      def ret(a: A): A = a
      def value[X](op: Reader[E, X]): X = op match
        case Reader.Ask() => e)(body)
