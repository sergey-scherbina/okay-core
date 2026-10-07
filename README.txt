okay-core
=========

Algebraic effects and handlers for Scala 3, on one monad of delimited
continuations with answer-type modification (Danvy-Filinski shift/reset,
Asai-Kameyama answer types). No macros, no casts, no runtime class tests:
an operation carries a path to its effect in the row, and a handler follows it.


Layout
------

  okay.core   the core
    Cont.scala      Row, +:, Pure, In (a path in a row), Cont[R, I, O, A] (the
                    monad: Return, Inject, Suspend, Bind, Shift, Map, Delay,
                    Reset, Push), Frames (the loop's stack), Free[R, A]
    Effects.scala   Effects (a program's context: its row c.R, its answer c.S),
                    run, reset, shift, handle, Handler, Answering (tail-
                    resumptive handlers, answered in place), perform,
                    % and A ! Es

  okay.std    the effects, written on the core
    Reader    ask                         reader(e)(body)
    State     get, put, modify            state(s0)(body)      -> (S, A)
    Writer    tell                        writer[W, A](body)   -> (List[W], A)
    Throws    raise                       throws[E, A](body)   -> Either[E, A]
    Choose    among (multi-shot)          choose[A](body)      -> Seq[A]
    Emit      yield_                      collect(body), generate(body) (lazy)
    PState    st.get, st.put: a state whose TYPE may change, carried by the
              answer type, not by the row   pstate(s0)(st => body)
    Async     async, await (callbacks)    Async.runAsync(body)(done),
                                          Async.run(body); failures are
                                          Throws % Throwable


Writing programs
----------------

The row is built by the handlers, outside in; a program names only the
effects it needs. Two ways to write one:

  // the context as givens
  def counting(i: Int)(using c: Effects, s: Has[State % Int, c.R]): Cont[c.R, c.S, c.S, Int] =
    if i == 0 then get[Int] else get[Int].flatMap(x => put(x + 1)).flatMap(_ => counting(i - 1))

  // A ! Es: a program of value A, in any context whose row has every effect of Es
  def inc: Int ! Reader % Int = ask[Int].map(_ + 1)
  def both: Int ! Reader % Int +: Writer % String = ask[Int].flatMap(a => tell(a.toString).map(_ => a))

  run(state[Int](0)(counting(10)))              // (10, 10)
  run(reader(5)(inc))                           // 6
  run(writer[String, Int](reader(5)(both)))     // (List("5"), 5)

`Reader % Int` is the effect `[X] =>> Reader[Int, X]`. In `A ! Es` the order
of Es is the program's own, the context's row may be any.

shift and reset in context:

  run(reset[Int](shift[Int, Int](k => k(10).flatMap(k)).map(_ + 1)))         // 12
  run(reset[Int](shift[Int, String](k => k(5).map(_.toString)).map(_ * 2)))  // "10"

Async at the top, its failure raised in the context:

  run(throws[Throwable, Int](Async.run(async(20).flatMap(a => async(a + 1)))))  // Right(21)


Build
-----

  sbt test                      the tests (munit)
  sbt bench                     JMH, then a summary table (project/Bench.scala)
  sbt "bench -f 1 -p n=100000 tailcall"
  sbt "bench save -prof gc"     keep the run in bench.d/ (index.tsv + CSV)
  sbt "benchHistory <regexp>"   the benchmarks matching it, run by run

Scala 3.9.0, sbt 1.13.0, JDK 25.
