package okay.core

import Effects.*
import okay.std.*
import java.util.concurrent.{Executors, TimeUnit}

/** `PState`: the state's type in the answer type; `Async`: a run that waits on callbacks */
class TestPStateAsync extends munit.FunSuite:

  /** an async run at the top, its failure the `Left` */
  def runIO[A](body: Effects.At[Async +: Async.Fails, Unit] ?=> Free[Async +: Async.Fails, A]): Either[Throwable, A] =
    run(throws[Throwable, A](Async.run(body)))

  test("pstate: get after put, the type unchanged"):
    val r: (Int, Int) = run(pstate(1)(st => st.put[Int](41).flatMap(_ => st.get[Int]).map(_ + 1)))
    assertEquals(r, (41, 42))

  test("pstate: the TYPE changed — an Int in, a String put, read back as a String"):
    val r: (String, Int) = run(pstate(7)(st => st.get[Int].flatMap(n => st.put[Int]("x" * n)).flatMap(_ => st.get[String]).map(_.length)))
    assertEquals(r, ("xxxxxxx", 7))

  test("pstate: a get of the wrong type does not compile — the answer types do not meet"):
    assert(compileErrors("""val r: (Int, Int) = run(pstate(1)(st => st.put[Int]("s").flatMap(_ => st.get[Int])))""").nonEmpty)

  test("pstate under an effect of the row: an operation between the moves, at the answer there"):
    // in the body the answer moves, so an operation is the answer-polymorphic node, not `ask` at the context's answer
    def body(using c: Effects, has: Has[Reader % Int, c.R])(st: PState[c.R, c.S, (String, Int)]) =
      st.get[Int].flatMap(n => Cont.inject[c.R, st.At[Int], Reader % Int, Int](Reader.Ask()).flatMap(a => st.put[Int]((n + a).toString))).map(_ => 0)
    val r: (String, Int) = run(reader(5)(pstate(1)(body)))
    assertEquals(r, ("6", 0))

  test("async: computations run in order, their values bound"):
    assertEquals(runIO(async(20).flatMap(a => async(a + 1)).map(_ * 2)), Right(42))

  test("await: a callback from another thread, the run going on in it"):
    val pool = Executors.newFixedThreadPool(2)
    try
      def later(x: Int)(using c: Effects, has: Has[Async, c.R]): Cont[c.R, c.S, c.S, Int] =
        await[Int](cb => pool.execute(() => { Thread.sleep(1); cb(Right(x)) }))
      assertEquals(runIO(later(1).flatMap(a => later(2).map(_ + a)).flatMap(a => later(3).map(_ + a))), Right(6))
    finally pool.shutdown()

  test("await: 100 000 callbacks answered at once, in constant stack"):
    def loop(i: Int, acc: Int)(using c: Effects, has: Has[Async, c.R]): Cont[c.R, c.S, c.S, Int] =
      if i == 0 then Cont.pure(acc) else await[Int](cb => cb(Right(1))).flatMap(x => loop(i - 1, acc + x))
    assertEquals(runIO(loop(100000, 0)), Right(100000))

  test("async: a failure — a Left at a callback, a throw in a computation — raised, the rest never runs, the rest never runs"):
    var ran = false
    val e1 = runIO(await[Int](cb => cb(Left(IllegalStateException("cb")))).map(x => { ran = true; x }))
    val e2 = runIO(async(1 / 0).map(x => { ran = true; x }))
    assertEquals((e1.left.map(_.getMessage), ran), (Left("cb"), false))
    assertEquals(e2.left.map(_.getMessage), Left("/ by zero"))

  test("async: the program's own raise, a Throws of the row, fails the run"):
    assertEquals(runIO(async(1).flatMap(_ => raise[Throwable, Int](IllegalStateException("own")))).left.map(_.getMessage), Left("own"))

  test("async with state inside: the state's handler goes on through the callbacks"):
    val pool = Executors.newSingleThreadExecutor()
    try
      val r = runIO(state[Int](0)(
        await[Int](cb => pool.execute(() => cb(Right(5)))).flatMap(x => modify[Int](_ + x)).flatMap(_ =>
          await[Int](cb => pool.execute(() => cb(Right(7)))).flatMap(y => modify[Int](_ + y)))))
      assertEquals(r, Right((12, 12)))
    finally pool.shutdown()

  def bang: Int ! Async + Reader % Int = ask[Int].flatMap(a => async(a + 1))
  test("async in A ! Es"):
    assertEquals(runIO(reader(41)(bang)), Right(42))
