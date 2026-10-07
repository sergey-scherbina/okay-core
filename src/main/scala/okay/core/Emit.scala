package okay.core

/** EMIT: a body that yields. Two handlers: `collect` keeps every element; `generate` is LAZY — each `yield`
 * captures the rest of the body as the next step, a program over what the handler leaves, run when the consumer
 * asks */
enum Emit[W, +A]:
  case Yield[W](w: W) extends Emit[W, Unit]
type EmitOf[W] = [X] =>> Emit[W, X]

def yield_[W](w: W)(using c: Effects, has: Has[EmitOf[W], c.R]): Cont[c.R, c.S, c.S, Unit] = perform[EmitOf[W], Unit](Emit.Yield(w))
/** `collect[W, A](body)`: every element yielded, and the value */
def collect[W, A]: CollectAt[W, A] = CollectAt[W, A]()
final class CollectAt[W, A]:
  type Ans = (List[W], A)
  def apply(using c: Effects)(body: Effects.At[EmitOf[W] +: c.R, Ans] ?=> Cont[EmitOf[W] +: c.R, Ans, Ans, A]): Cont[c.R, c.S, c.S, Ans] =
    Effects.handle(new Handler[EmitOf[W], c.R, c.S, A, Ans]:
      def ret(a: A): Ans = (Nil, a)
      def apply[X](op: Emit[W, X], k: X => Cont[c.R, c.S, c.S, Ans]): Cont[c.R, c.S, c.S, Ans] = op match
        case Emit.Yield(w) => k(()).map((ws, a) => (w :: ws, a)))(body)

/** a lazy generator over the row `R`: the next element and the rest, a program over `R` at the generator's own
 * answer; or done */
enum Gen[W, R <: Row]:
  case Next(w: W, rest: Cont[R, Gen[W, R], Gen[W, R], Gen[W, R]])
  case Done()
/** `generate[W](body)`: the body as a lazy generator — nothing runs until the consumer pulls. The generator is a
 * program at its own answer, `Gen`, so it is written in a context at that answer; at the top, `run` pulls its
 * next element */
def generate[W]: GenerateAt[W] = GenerateAt[W]()
final class GenerateAt[W]:
  def apply(using c: Effects)(body: Effects.At[EmitOf[W] +: c.R, Gen[W, c.R]] ?=> Cont[EmitOf[W] +: c.R, Gen[W, c.R], Gen[W, c.R], Unit])
           (using c.S =:= Gen[W, c.R]): Cont[c.R, c.S, c.S, Gen[W, c.R]] =
    type G = Gen[W, c.R]
    val h = new Handler[EmitOf[W], c.R, G, Unit, G]:
      def ret(a: Unit): G = Gen.Done()
      def apply[X](op: Emit[W, X], k: X => Cont[c.R, G, G, G]): Cont[c.R, G, G, G] = op match
        case Emit.Yield(w) => Cont.Return(Gen.Next(w, k(())))
    val p: Cont[c.R, G, G, G] = body(using Effects.At()).handle(h)
    summon[c.S =:= G].substituteContra[[Q] =>> Cont[c.R, Q, Q, G]](p)
