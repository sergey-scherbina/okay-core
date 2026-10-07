package okay.core

import Cont.*

/** the monad alone: `shift`, `reset`, a handler written against `Handler` itself, the loop */
class TestCont extends munit.FunSuite:
  enum Ask[+A]:
    case Number extends Ask[Int]
  enum Say[+A]:
    case Line(s: String) extends Say[Unit]
  type Eff = Ask +: Say +: Pure

  def asking[Q, A](n: Int): Handler[Ask, Say +: Pure, Q, A, A] = new Handler[Ask, Say +: Pure, Q, A, A]:
    def ret(a: A): A = a
    def apply[X](op: Ask[X], k: X => Cont[Say +: Pure, Q, Q, A]): Cont[Say +: Pure, Q, Q, A] = op match
      case Ask.Number => k(n)
  def saying[Q, A]: Handler[Say, Pure, Q, A, (List[String], A)] = new Handler[Say, Pure, Q, A, (List[String], A)]:
    def ret(a: A): (List[String], A) = (Nil, a)
    def apply[X](op: Say[X], k: X => Cont[Pure, Q, Q, (List[String], A)]): Cont[Pure, Q, Q, (List[String], A)] = op match
      case Say.Line(s) => k(()).map((log, a) => (s :: log, a))

  test("reset(1 + shift(k => k(k(10)))) is 12: the context captured as a program, applied twice"):
    val p: Cont[Pure, Int, Int, Int] = shift[Pure, Int, Int, Int](k => k(10).flatMap(k)).map(_ + 1).reset
    assertEquals(p.value, 12)

  test("the answer type modified: reset(shift(k => k(5).toString) * 2) is a String"):
    val p: Cont[Pure, String, String, String] = shift[Pure, Int, String, Int](k => k(5).map(_.toString)).map(_ * 2).reset
    assertEquals(p.value, "10")

  test("a shift whose body drops k aborts the delimiter: the rest never runs"):
    var ran = false
    val p: Cont[Pure, Int, Int, Int] = shift[Pure, Int, Int, Int](_ => Return(-1)).map(x => { ran = true; x + 1 }).reset
    assertEquals(p.value, -1)
    assert(!ran)

  test("the top is a delimiter: a shift with no reset captures up to it"):
    assertEquals(shift[Pure, Int, Int, Int](k => k(1).flatMap(k)).map(_ * 3).value, 9)

  test("a shift captures to the NEAREST delimiter: an inner reset is not crossed"):
    val inner: Cont[Pure, Int, Int, Int] = shift[Pure, Int, Int, Int](k => k(1)).map(_ + 10).reset
    val p: Cont[Pure, Int, Int, Int] = inner.map(_ * 2).reset
    assertEquals(p.value, 22)

  test("a shift's body runs under the delimiter: a shift in it captures up to the same delimiter"):
    val p: Cont[Pure, Int, Int, Int] =
      shift[Pure, Int, Int, Int](k => shift[Pure, Int, Int, Int](k2 => k2(1).map(_ * 100)).flatMap(k)).map(_ + 1).reset
    assertEquals(p.value, 200)

  test("a handler answers its operations with the rest of the body; another effect's go on outside, by their path"):
    val prog: Cont[Eff, Int, Int, Int] =
      inject[Eff, Int, Ask, Int](Ask.Number).flatMap(n => inject[Eff, Int, Say, Unit](Say.Line(n.toString)).map(_ => n + 1))
    val asked: Cont[Say +: Pure, (List[String], Int), (List[String], Int), Int] = prog.handle(asking(41))
    val said: Cont[Pure, (List[String], Int), (List[String], Int), (List[String], Int)] = asked.handle(saying)
    assertEquals(said.value, (List("41"), 42))

  test("a shift under a handler: operations in k and in the shift's body reach the handler"):
    val prog: Cont[Eff, Int, Int, Int] =
      shift[Eff, Int, Int, Int](k => k(1).flatMap(a => inject[Eff, Int, Ask, Int](Ask.Number).flatMap(n => k(n).map(_ + a))))
        .flatMap(x => inject[Eff, Int, Ask, Int](Ask.Number).map(_ * x))
    val asked: Cont[Say +: Pure, Int, Int, Int] = prog.handle(asking(5))
    val said: Cont[Pure, Int, Int, Int] = asked.handle(new Handler[Say, Pure, Int, Int, Int]:
      def ret(a: Int) = a
      def apply[X](op: Say[X], k: X => Cont[Pure, Int, Int, Int]) = op match
        case Say.Line(_) => k(()))
    // k(1) = 5, k(5) = 25, 25 + 5
    assertEquals(said.value, 30)

  test("a handler resuming twice runs the rest twice: a pure tree, nothing consumed"):
    var runs = 0
    val twice: Handler[Ask, Pure, List[Int], Int, List[Int]] = new Handler[Ask, Pure, List[Int], Int, List[Int]]:
      def ret(a: Int) = List(a)
      def apply[X](op: Ask[X], k: X => Cont[Pure, List[Int], List[Int], List[Int]]) = op match
        case Ask.Number => k(1).flatMap(xs => k(2).map(xs ++ _))
    val prog: Cont[Ask +: Pure, List[Int], List[Int], Int] = inject[Ask +: Pure, List[Int], Ask, Int](Ask.Number).map(n => { runs += 1; n * 10 })
    assertEquals(prog.handle(twice).value, List(10, 20))
    assertEquals(runs, 2)

  test("100 000 binds nested to the left, and to the right, in constant stack"):
    val left = (1 to 100000).foldLeft(pure[Pure, Int, Int](0))((acc, _) => acc.map(_ + 1))
    assertEquals(left.value, 100000)
    def right(n: Int): Cont[Pure, Int, Int, Int] = if n == 0 then Return(0) else Return(1).flatMap(x => right(n - 1).map(_ + x))
    assertEquals(right(100000).value, 100000)

  test("100 000 operations under a handler in constant stack"):
    def loop(n: Int, acc: Int): Cont[Ask +: Pure, Int, Int, Int] =
      if n == 0 then Return(acc) else inject[Ask +: Pure, Int, Ask, Int](Ask.Number).flatMap(x => loop(n - 1, acc + x))
    val one: Handler[Ask, Pure, Int, Int, Int] = new Handler[Ask, Pure, Int, Int, Int]:
      def ret(a: Int) = a
      def apply[X](op: Ask[X], k: X => Cont[Pure, Int, Int, Int]) = op match
        case Ask.Number => k(1)
    assertEquals(loop(100000, 0).handle(one).value, 100000)

  test("delay: the program is built when the loop gets to it, not before"):
    var built = 0
    val p: Cont[Pure, Int, Int, Int] = delay { built += 1; pure(1) }
    assertEquals(built, 0)
    assertEquals(p.value, 1)
    assertEquals(p.value, 1)
    assertEquals(built, 2)

  test("100 000 tail calls through delay in constant stack"):
    def isEven(i: Int): Cont[Pure, Boolean, Boolean, Boolean] = if i == 0 then pure(true) else delay(isOdd(i - 1))
    def isOdd(i: Int): Cont[Pure, Boolean, Boolean, Boolean] = if i == 0 then pure(false) else delay(isEven(i - 1))
    assertEquals(isEven(100000).value, true)
    assertEquals(isEven(100001).value, false)

  test("100 000 maps nested to the left, and to the right through delay, in constant stack"):
    val left = (1 to 100000).foldLeft(pure[Pure, Int, Int](0))((acc, _) => acc.map(_ + 1))
    assertEquals(left.value, 100000)
    def go(i: Int): Cont[Pure, Int, Int, Int] = if i == 0 then pure(0) else delay(go(i - 1)).map(_ + 1)
    assertEquals(go(100000).value, 100000)

  test("maps and binds mixed keep their order"):
    val p: Cont[Pure, String, String, String] =
      pure[Pure, String, Int](1).map(_ + 1).flatMap(x => pure(x * 10)).map(_.toString).flatMap(s => pure(s + "!")).map(_ * 2)
    assertEquals(p.value, "20!20!")

  test("a shift under left-nested binds and maps: k is the whole context to the delimiter, applied twice"):
    val inner: Cont[Pure, Int, Int, Int] = shift[Pure, Int, Int, Int](k => k(1).flatMap(a => k(10).map(b => a + b)))
    val p: Cont[Pure, Int, Int, Int] = inner.map(_ + 1).flatMap(x => pure(x * 2)).map(_ + 100).reset
    // k(x) = (x + 1) * 2 + 100: k(1) = 104, k(10) = 122
    assertEquals(p.value, 226)

  test("a map after an operation of a handler resuming twice: the map runs on each path"):
    val twice: Handler[Ask, Pure, List[Int], Int, List[Int]] = new Handler[Ask, Pure, List[Int], Int, List[Int]]:
      def ret(a: Int) = List(a)
      def apply[X](op: Ask[X], k: X => Cont[Pure, List[Int], List[Int], List[Int]]) = op match
        case Ask.Number => k(1).flatMap(xs => k(2).map(xs ++ _))
    val prog: Cont[Ask +: Pure, List[Int], List[Int], Int] =
      inject[Ask +: Pure, List[Int], Ask, Int](Ask.Number).map(_ * 10).flatMap(x => pure(x + 1)).map(_ * 2)
    assertEquals(prog.handle(twice).value, List(22, 42))

  test("an operation inside a reset, handled outside it by a handler resuming twice: the reset is put back each time"):
    val twice: Handler[Ask, Pure, List[Int], List[Int], List[Int]] = new Handler[Ask, Pure, List[Int], List[Int], List[Int]]:
      def ret(a: List[Int]) = a
      def apply[X](op: Ask[X], k: X => Cont[Pure, List[Int], List[Int], List[Int]]) = op match
        case Ask.Number => k(1).flatMap(xs => k(2).map(xs ++ _))
    // inside the reset: the operation, then a shift capturing up to the reset, its k applied twice
    val inner: Cont[Ask +: Pure, Int, Int, Int] =
      inject[Ask +: Pure, Int, Ask, Int](Ask.Number).flatMap(n => shift[Ask +: Pure, Int, Int, Int](k => k(n).flatMap(a => k(n * 10).map(_ + a))).map(_ + 1))
    val prog: Cont[Ask +: Pure, List[Int], List[Int], List[Int]] = inner.reset.map(List(_))
    // n = 1: (1 + 1) + (10 + 1) = 13; n = 2: (2 + 1) + (20 + 1) = 24
    assertEquals(prog.handle(twice).value, List(13, 24))

  test("value needs every operation handled: a program over a row is not a program at the top"):
    assert(compileErrors("""inject[Ask +: Pure, Int, Ask, Int](Ask.Number).value""").nonEmpty)

  test("`+` grows a row on the right: Pure + Ask + Say is Say +: Ask +: Pure, the same type"):
    summon[(Pure + Ask + Say) =:= (Say +: Ask +: Pure)]
    val prog: Cont[Pure + Say + Ask, Int, Int, Int] =
      inject[Pure + Say + Ask, Int, Ask, Int](Ask.Number).flatMap(n => inject[Pure + Say + Ask, Int, Say, Unit](Say.Line(n.toString)).map(_ => n + 1))
    assertEquals(prog.handle(asking(41)).handle(saying).value, (List("41"), 42))

  test("an effect not in the row cannot be injected"):
    assert(compileErrors("""inject[Ask +: Pure, Int, Say, Unit](Say.Line("x"))""").nonEmpty)
