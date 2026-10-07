package okay.core

import Effects.*

/** the effects in context: the syntax and the answer never named — the handlers build the syntax, outside in */
class TestEffects extends munit.FunSuite:

  test("reader: ask answered in place"):
    assertEquals(run(reader[Int](41)(ask[Int].map(_ + 1))), 42)

  test("writer: the log in order, and the value"):
    assertEquals(run(writer[String, Int](tell("a").flatMap(_ => tell("b")).map(_ => 1))), (List("a", "b"), 1))

  test("reader and writer, the handlers in either order: a program needs only its capabilities"):
    def prog(using c: Effects, r: Has[ReaderOf[Int], c.R], w: Has[WriterOf[String], c.R]): Cont[c.R, c.S, c.S, Int] =
      ask[Int].flatMap(n => tell(n.toString).map(_ => n + 1))
    assertEquals(run(writer[String, Int](reader[Int](41)(prog))), (List("41"), 42))
    assertEquals(run(reader[(List[String], Int)](41)(writer[String, Int](prog))), (List("41"), 42))

  test("state: get, put, modify; the last state and the value"):
    val r = run:
      state[String](1):
        for
          a <- get[Int]
          _ <- put(a + 1)
          b <- modify[Int](_ * 10)
        yield s"$a,$b"
    assertEquals(r, (20, "1,20"))

  test("100 000 operations answered in place, and forwarded through a handler, in constant stack"):
    def loop(i: Int)(using c: Effects, s: Has[StateOf[Int], c.R]): Cont[c.R, c.S, c.S, Int] =
      if i == 0 then get[Int] else modify[Int](_ + 1).flatMap(_ => loop(i - 1))
    assertEquals(run(state[Int](0)(loop(100000))), (100000, 100000))
    def asks(i: Int, acc: Int)(using c: Effects, r: Has[ReaderOf[Int], c.R]): Cont[c.R, c.S, c.S, Int] =
      if i == 0 then Cont.pure(acc) else ask[Int].flatMap(x => asks(i - 1, acc + x))
    assertEquals(run(reader[(List[String], Int)](1)(collect[String, Int](asks(100000, 0)))), (Nil, 100000))

  test("an operation through 16 handlers of other effects, and back, 10 000 times"):
    def loop(i: Int, acc: Int)(using c: Effects, r: Has[ReaderOf[Int], c.R]): Cont[c.R, c.S, c.S, Int] =
      if i == 0 then Cont.pure(acc) else ask[Int].flatMap(x => loop(i - 1, acc + x))
    def nested(d: Int)(using c: Effects, r: Has[ReaderOf[Int], c.R]): Cont[c.R, c.S, c.S, Int] =
      if d == 0 then loop(10000, 0) else reader[Int](0.5)(nested(d - 1))
    assertEquals(run(reader[Int](2)(nested(16))), 20000)

  test("state: the cell is per run — the same program run twice starts from s0 twice"):
    val p: Cont[Pure, (Int, Int), (Int, Int), (Int, Int)] = state[Int](0)(using At[Pure, (Int, Int)]())(modify[Int](_ + 1))
    assertEquals(p.value, (1, 1))
    assertEquals(p.value, (1, 1))

  test("two instances of one effect, State[Int] and State[String]: each goes to its own handler, by its path"):
    def body(using c: Effects, i: Has[StateOf[Int], c.R], s: Has[StateOf[String], c.R]): Cont[c.R, c.S, c.S, Unit] =
      modify[Int](_ + 1).flatMap(n => modify[String](_ + n)).map(_ => ())
    assertEquals(run(state[(Int, Unit)]("x")(state[Unit](41)(body))), ("x42", (42, ())))
    assertEquals(run(state[(String, Unit)](41)(state[Unit]("x")(body))), (42, ("x42", ())))

  test("throws: raise aborts the body, Left; no raise, Right"):
    assertEquals(run(throws[String, Int](raise[String, Int]("boom").map(_ + 1))), Left("boom"))
    assertEquals(run(throws[String, Int](Cont.pure(1).map(_ + 1))), Right(2))

  test("state and throws: the order of handlers is the semantics — state kept or lost on a failure"):
    def body(using c: Effects, s: Has[StateOf[Int], c.R], t: Has[ThrowsOf[String], c.R]): Cont[c.R, c.S, c.S, Int] =
      put(5).flatMap(_ => raise[String, Int]("boom"))
    assertEquals(run(state[Either[String, Int]](0)(throws[String, Int](body))), (5, Left("boom")))
    assertEquals(run(throws[String, (Int, Int)](state[Int](0)(body))), Left("boom"))

  test("choose: every path, in order — multi-shot"):
    val r = run:
      choose[(Int, Int)]:
        for
          x <- among(Seq(1, 2, 3))
          y <- among(Seq(10, 20))
        yield (x, y)
    assertEquals(r, Seq((1, 10), (1, 20), (2, 10), (2, 20), (3, 10), (3, 20)))

  test("choose among 10 000: every path, in order"):
    assertEquals(run(choose[Int](among(0 until 10000).map(_ * 2))), (0 until 10000).map(_ * 2))

  test("8 queens by choose: 92 solutions"):
    def safe(col: Int, placed: List[Int]) = placed.zipWithIndex.forall((c, d) => c != col && math.abs(c - col) != d + 1)
    def place(row: Int, placed: List[Int])(using c: Effects, ch: Has[Choose, c.R]): Cont[c.R, c.S, c.S, List[Int]] =
      if row == 8 then Cont.pure(placed) else among((0 until 8).filter(safe(_, placed))).flatMap(col => place(row + 1, col :: placed))
    val all = run(choose[List[Int]](place(0, Nil)))
    assertEquals(all.length, 92)
    assertEquals(all.head, List(3, 1, 6, 2, 5, 7, 4, 0))

  test("choose with an empty choice prunes the path"):
    assertEquals(run(choose[Int](among(Seq(1, 2)).flatMap(x => among(if x == 1 then Seq.empty else Seq(x))))), Seq(2))

  test("choose and state: a resumption shares the cell — the second path sees the first's last state"):
    assertEquals(run(state[Seq[Int]](0)(choose[Int](among(Seq(1, 2)).flatMap(x => modify[Int](_ + x))))), (3, Seq(1, 3)))

  test("collect: every element yielded, and the value"):
    assertEquals(run(collect[Int, String](yield_(1).flatMap(_ => yield_(2)).map(_ => "done"))), (List(1, 2), "done"))

  test("collect: the buffer is per run — the same program run twice collects from empty twice"):
    val p: Cont[Pure, (List[Int], Unit), (List[Int], Unit), (List[Int], Unit)] =
      collect[Int, Unit](using At[Pure, (List[Int], Unit)]())(yield_(1).flatMap(_ => yield_(2)))
    assertEquals(p.value, (List(1, 2), ()))
    assertEquals(p.value, (List(1, 2), ()))

  test("collect and choose: a resumption shares the buffer — both paths' elements, in the order they ran"):
    assertEquals(run(collect[Int, Seq[Int]](choose[Int](among(Seq(1, 2)).flatMap(x => yield_(x).map(_ => x * 10))))), (List(1, 2), Seq(10, 20)))

  test("generate is lazy: an infinite body, three elements pulled, nothing past them runs"):
    type G = Gen[Int, Pure]
    var produced = 0
    def from(n: Int)(using c: Effects, e: Has[EmitOf[Int], c.R]): Cont[c.R, c.S, c.S, Unit] =
      yield_(n).flatMap(_ => { produced += 1; from(n + 1) })
    val gen: Cont[Pure, G, G, G] = generate[Int](using At[Pure, G]())(from(0))
    def take(n: Int, g: Cont[Pure, G, G, G]): List[Int] = if n == 0 then Nil else g.value match
      case Gen.Done() => Nil
      case Gen.Next(w, rest) => w :: take(n - 1, rest)
    assertEquals(take(3, gen), List(0, 1, 2))
    assertEquals(produced, 2)

  test("generate over a remaining effect: the rest of the generator still asks, under the handler it is run in"):
    type F = ReaderOf[Int] +: Pure
    type G = Gen[Int, F]
    def all(g: Cont[F, G, G, G]): List[Int] = run(reader[G](7)(g)) match
      case Gen.Done() => Nil
      case Gen.Next(w, rest) => w :: all(rest)
    val gen: Cont[F, G, G, G] =
      generate[Int](using At[F, G]())(ask[Int].flatMap(n => yield_(n)).flatMap(_ => ask[Int].flatMap(n => yield_(n * 2))))
    assertEquals(all(gen), List(7, 14))

  test("Fibonacci: collected and generated agree, the generator infinite, and 10 000 of them in constant stack"):
    def fibs(i: Int, a: Long, b: Long)(using c: Effects, e: Has[EmitOf[Long], c.R]): Cont[c.R, c.S, c.S, Unit] =
      if i == 0 then Cont.pure(()) else yield_(a).flatMap(_ => fibs(i - 1, b, a + b))
    def forever(a: Long, b: Long)(using c: Effects, e: Has[EmitOf[Long], c.R]): Cont[c.R, c.S, c.S, Unit] =
      yield_(a).flatMap(_ => forever(b, a + b))
    type G = Gen[Long, Pure]
    def take(n: Int, g: Cont[Pure, G, G, G]): List[Long] = if n == 0 then Nil else g.value match
      case Gen.Done() => Nil
      case Gen.Next(w, rest) => w :: take(n - 1, rest)
    val first = List(0L, 1, 1, 2, 3, 5, 8, 13, 21, 34)
    assertEquals(run(collect[Long, Unit](fibs(10, 0, 1)))._1, first)
    assertEquals(take(10, generate[Long](using At[Pure, G]())(forever(0, 1))), first)
    assertEquals(run(collect[Long, Unit](fibs(10000, 0, 1)))._1.length, 10000)

  test("generate stays lazy over a forwarded effect: an infinite body asking at every step, three pulled"):
    type F = ReaderOf[Int] +: Pure
    type G = Gen[Int, F]
    var produced = 0
    def from(n: Int)(using c: Effects, e: Has[EmitOf[Int], c.R], r: Has[ReaderOf[Int], c.R]): Cont[c.R, c.S, c.S, Unit] =
      ask[Int].flatMap(x => yield_(n * x)).flatMap(_ => { produced += 1; from(n + 1) })
    def take(i: Int, g: Cont[F, G, G, G]): List[Int] = if i == 0 then Nil else run(reader[G](10)(g)) match
      case Gen.Done() => Nil
      case Gen.Next(w, rest) => w :: take(i - 1, rest)
    assertEquals(take(3, generate[Int](using At[F, G]())(from(1))), List(10, 20, 30))
    assertEquals(produced, 2)

  test("handlers are dynamic: a generator's rest asks the reader it is run under, not the one it was built under"):
    type F = ReaderOf[Int] +: Pure
    type G = Gen[Int, F]
    // built under reader(7): its rest is still a program over Reader — its type says it needs one — and the
    // consumer runs it under reader(8). A handler found where the program was BUILT (evidence passing) would say 7
    val gen: Cont[Pure, G, G, G] =
      reader[G](7)(using At[Pure, G]())(generate[Int](ask[Int].flatMap(yield_(_)).flatMap(_ => ask[Int].flatMap(yield_(_)))))
    gen.value match
      case Gen.Next(first, rest) =>
        assertEquals(first, 7)
        run(reader[G](8)(rest)) match
          case Gen.Next(second, _) => assertEquals(second, 8)
          case Gen.Done() => fail("a second element")
      case Gen.Done() => fail("a first element")

  test("an effect its context does not have cannot be performed"):
    assert(compileErrors("""run(reader[Unit](1)(tell("x")))""").nonEmpty)
