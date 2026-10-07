package okay.std

import okay.core.*

/** PSTATE: a state whose TYPE may change, carried by the ANSWER TYPE — Danvy–Filinski's state passing, the type
 * moved by Asai–Kameyama's answer-type modification. Not an effect of the row: no operation, no handler; `get`
 * and `put` are captures up to the run's delimiter, whose answer is the rest of the run given the state, `St`.
 * `put` of another type moves the answer from `St[S2]` to `St[S]`, and the binds compose the moves — a `get` of
 * the wrong type, a `put` out of order, does not compile */
/** the answer of a stateful program: given the state `S`, the rest of the run, of value `V`, at the answer `Q`
 * outside */
type St[R <: Row, Q, S, V] = S => Cont[R, Q, Q, V]

/** the operations of one run: the row `R`, the answer `Q` outside it, the run's value `V` */
final class PState[R <: Row, Q, V]:
  type At[S] = St[R, Q, S, V]
  /** the state, its type `S` unchanged: `shift(k => s => k(s)(s))` */
  def get[S]: Cont[R, At[S], At[S], S] =
    Cont.shift(k => Cont.pure((s: S) => k(s).reset[Q].flatMap(f => f(s))))
  /** a state of another type, `S2`, for the rest; the state before was `S`: `shift(k => _ => k(())(s2))` */
  def put[S]: PutFrom[S] = PutFrom[S]()
  final class PutFrom[S]:
    def apply[S2](s2: S2): Cont[R, At[S2], At[S], Unit] =
      // the rest's value, the run from `S2` on, to `S`'s answer — so it is delimited as the body it is
      Cont.shift(k => Cont.pure((s: S) => k(()).map(f => ((_: S) => f(s2)): At[S]).reset[Q].flatMap(g => g(s))))

/** `pstate(s0)(st => body)`: the body run from the state `s0`, of type `S0`, to a state of type `S1`; the last
 * state and the value */
def pstate[S0](s0: S0): PStateFrom[S0] = PStateFrom(s0)
final class PStateFrom[S0](s0: S0):
  def apply[S1, W](using c: Effects)(body: PState[c.R, c.S, (S1, W)] => Cont[c.R, St[c.R, c.S, S1, (S1, W)], St[c.R, c.S, S0, (S1, W)], W])
    : Cont[c.R, c.S, c.S, (S1, W)] =
    Cont.Reset(body(PState()), (w: W) => (s1: S1) => Cont.pure((s1, w))).flatMap(f => f(s0))
