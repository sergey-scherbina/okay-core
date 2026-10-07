package okay.core

import Effects.*

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
