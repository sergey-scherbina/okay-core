package okay.core

import Effects.*
import okay.std.*

/** `reset` and `shift` in context: the syntax and the hole's answer from the context, only the value and the
 * answer moved to are named */
class TestContext extends munit.FunSuite:

  test("reset(1 + shift(k => k(k(10)))) is 12"):
    assertEquals(run(reset[Int](shift[Int, Int](k => k(10).flatMap(k)).map(_ + 1))), 12)

  test("the answer type modified: the shift names the answer it moves the delimiter to"):
    assertEquals(run(reset[Int](shift[Int, String](k => k(5).map(_.toString)).map(_ * 2))), "10")

  test("a shift's body is written under the delimiter, in a context at the answer moved to: it may perform"):
    val r = run:
      reader[Int](3):
        reset[Int](shift[Int, Int](k => ask[Int].flatMap(k)).map(_ * 10))
    assertEquals(r, 30)

  test("a shift at the top: the context there is the top's, its answer the value"):
    assertEquals(run[Int](shift[Int, Int](k => k(1).flatMap(k)).map(_ * 3)), 9)

  test("an operation under a handler, under a reset: the capture is re-delimited, the handler still answers"):
    val r = run:
      writer[String, Int]:
        reset[Int](shift[Int, Int](k => k(1).flatMap(k)).flatMap(x => tell(x.toString).map(_ => x + 1)))
    assertEquals(r, (List("1", "2"), 3))

  def inc: Int ! Reader % Int = ask[Int].map(_ + 1)
  def twice: Int ! Reader % Int = inc.flatMap(a => inc.map(_ + a))

  test("A ! E: a program over any row with E, under its handler alone or under others"):
    assertEquals(run(reader(5)(twice)), 12)
    assertEquals(run(writer[String, Int](reader(5)(twice))), (Nil, 12))
    assertEquals(run(reader(5)(writer[String, Int](twice))), (Nil, 12))

  test("A ! E: an effect not declared is not in reach"):
    assert(compileErrors("""def both: Int ! Reader % Int = ask[Int].flatMap(a => tell("x").map(_ => a))""").contains("No given instance"))

  def both: Int ! Reader % Int +: Writer % String = ask[Int].flatMap(a => tell(a.toString).map(_ => a + 1))
  def both2: Int ! Reader % Int +: Writer % String = both.flatMap(a => both.map(_ + a))

  test("A !! Es: a program over several effects, under their handlers in any order, among others"):
    assertEquals(run(writer[String, Int](reader(5)(both2))), (List("5", "5"), 12))
    assertEquals(run(reader(5)(writer[String, Int](both2))), (List("5", "5"), 12))
    assertEquals(run(state[(List[String], Int)](0)(reader(5)(writer[String, Int](both2))))._2, (List("5", "5"), 12))
    // a program of one of the effects, inside
    def mixed: Int ! Reader % Int +: Writer % String = twice.flatMap(a => both.map(_ + a))
    assertEquals(run(writer[String, Int](reader(5)(mixed))), (List("5"), 18))

  test("A ! E1 + E2: the effects needed joined by `+`, as a row is grown, `Pure` in front or not"):
    def plus: Int ! Reader % Int + Writer % String = ask[Int].flatMap(a => tell(a.toString).map(_ => a + 1))
    def plus3: Int ! Reader % Int + Writer % String + State % Int = plus.flatMap(a => get[Int].map(_ + a))
    def pure: Int ! Pure + Reader % Int + Writer % String = plus
    assertEquals(run(reader(5)(writer[String, Int](plus))), (List("5"), 6))
    assertEquals(run(writer[String, Int](reader(5)(pure))), (List("5"), 6))
    assertEquals(run(state[(List[String], Int)](10)(writer[String, Int](reader(5)(plus3))))._2, (List("5"), 16))
