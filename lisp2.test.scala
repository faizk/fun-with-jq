//> using test.dep org.scalameta::munit::0.7.29
//> using test.dep org.scalameta::munit-scalacheck:0.7.29

package tut

import cats._, data._, syntax.all._
import org.scalacheck._, Prop._, Arbitrary.arbitrary, org.scalacheck.Prop.propBoolean

class Lisp2PropTests extends munit.ScalaCheckSuite {
  import lisp2._
  import Lisp2PropTests._

  import pc2utils._

  property("all s-expressions can be parsed") {
    forAll { (e: Sxpr) =>
      println(e.show)
      assertEquals(readP.friendly(e.show), Right(e))
    }
 }

  property("all s-expressions can be parsed, even when cons-list rendering is off") {
    implicit val rPrefs = ShowPrefs(renderConsList = false)
    forAll { (e: Sxpr) =>
      println(e.show)
      assertEquals(readP.friendly(e.show), Right(e))
    }
  }

  val eval = lisp2.readP.friendly andThenF (lisp2.eval(_))

  property("built-in `empty?` considers only `'()` as empty") {
    forAll { (e: Sxpr) =>
      val empty = eval(show"(empty? '$e)")
      assertEquals(empty, Right(Lit(e == NIL)))
    }
  }

  type OK = (String, Sxpr)
  def checkOK(name: String, expectation: OK, expectations: OK*)(implicit loc: munit.Location): Unit =
    check(name = name, expectation.map(Right.apply), expectations.map(_ map Right.apply): _*)
  type Expect = (String, Either[Err, Value])
  def check(name: String, expectation: Expect, expectations: Expect*)(implicit loc: munit.Location): Unit =
    test(name) {
      for ((in, value) <- (expectation :: expectations.toList))
        assertEquals(eval(in), value)
    }

  checkOK("number", "331" -> Lit(331))
  checkOK("zilch", "()" -> NIL)

  checkOK("symbol ref", "'a" -> Sym("a"))
  checkOK("zilch quoted", "'()" -> NIL)
  checkOK("cons list", "(cons 1 (cons 2 '()))" -> Pair(Lit(1), Pair(Lit(2), NIL)))

  check("let",
    "(let ((x 23)) x)" -> Lit(23).asRight,
    "(let () 21)" -> Lit(21).asRight,
    "(let ((x 11)) y)" -> "undefined variable: [y]".asLeft,
  )
  check("let recursive bindings",
    "(let ((x 1) (y x)) y)" -> Lit(1).asRight,
    "(let ((x 1) (y (let ((x x)) x))) y)" -> Lit(1).asRight,
    "(let ((x 3)) (let ((y x)) y))" -> Lit(3).asRight,
  )

  checkOK("arith[+] arity-2", "(+ 3 8)" -> Lit(11))
  checkOK("arith[+] arity-1", "(+ 41)" -> Lit(41))
  checkOK("arith[+] arity-0", "(+ )" -> Lit(0))
  checkOK("arith[+] arity-n", "(+ 71 32 6)" -> Lit(109))
  checkOK("arith[+] arity-(..)", "(+ 71 (+ 4 3))" -> Lit(78))
  checkOK("arith[-] arity-2", "(- 3 8)" -> Lit(-5))
  checkOK("arith[-] arity-1", "(- 41)" -> Lit(-41))
  checkOK("arith[-] arity-0", "(- )" -> Lit(0))
  checkOK("arith[-] arity-n", "(- 71 32 6)" -> Lit(33))
  checkOK("arith[-] arity-(..)", "(- 71 (- 4 3))" -> Lit(70))
  checkOK("let body evaluation", "(let ((x 7)) (* 7 7))" -> Lit(49))
  check("lambda arity-0",
    "((lambda () 8))" -> Lit(8).asRight,
    "((lambda () 8) 9)" -> "arity mismatch: given 1 for expected 0".asLeft,
  )
  checkOK("let body providing lexical scope",
    "((let ((y 2)) (lambda (x) (+ x y))) 7)" -> Lit(9))

  checkOK("lambda arity-1", "((lambda (x) (+ 7 x)) 2)" -> Lit(9))
  checkOK("lambda arity-2", "((lambda (x y) (+ y x)) 2 3)" -> Lit(5))

  check("bodiless lambdas aren't", "(lambda (x))" -> "undefined variable: [lambda]".asLeft)
  test("[TODO] bodiless lambdas aren't".fail){
    assert(clue(eval("(lambda (x))").swap).exists(_ contains "syntax error"))
  }

  checkOK("simple if/else", "(if (= 1 2) 'y 'n)" -> Sym("n"))

  checkOK("recursion with letrec", """
    (letrec ((len (lambda (l) (if (empty? l) 0 (+ 1 (len (cdr l))))))
             (l2 (cons 'a (cons 'b '())))
             (l7 '(1 2 3 4 5 6 7))
             (l0 '()))
       (cons (len l2) (cons (len l7) (cons (len l0) '()))))
    """ -> Pair(Lit(2), Pair(Lit(7), Pair(Lit(0), NIL)))
  )

  checkOK("HOF", """
    (letrec ((map (lambda (f l)
                    (if (empty? l)
                      l
                      (cons (f (car l))
                            (map f (cdr l)))))))
      (map (lambda (x) (+ x x))
           '(1 2 3)))
    """ -> Pair(Lit(2), Pair(Lit(4), Pair(Lit(6), NIL)))
  )

  test("lexical scope") {
    assertEquals(
      eval(
        """
        (let ((y 1))
          ((let ((y 2)) (lambda (x) (+ x y))) 4))
        """),
      Right(Lit(6)))
  }

  check("uq not in qq", """
    (+ 1 ,2)
    """ -> "unqoute: not in quasiquote: [,2]".asLeft)
  property("qq like q without uq") {
    forAll { (e: Sxpr) =>
      println(e.show)
      assertEquals(eval(show"'$e"), eval(show"`$e"))
    }
  }
  checkOK("qq like q without", """
    `(+ 1 2)
    """ -> Pair(Sym("+"), Pair(Lit(1), Pair(Lit(2), NIL))))
  checkOK("qq with uq atom", """
    `(+ 1 ,2)
    """ -> Pair(Sym("+"), Pair(Lit(1), Pair(Lit(2), NIL))))
  checkOK("qq with uq sxpr eval'd", """
    `(+ 1 ,(+ 1 2))
    """ -> Pair(Sym("+"), Pair(Lit(1), Pair(Lit(3), NIL))))
}

object Lisp2PropTests {
  import Gen._
  import lisp2._

  val genSymC: Gen[Char] = asciiPrintableChar.suchThat {
    case '('|')'|' '|'\n'|'\t'|','|'`'|'\'' => false
    case '.' => false // FIXME
    case _ => true }

  val genNumE: Gen[Lit[Int]] = posNum[Int] map (Lit(_))
  val genSymE: Gen[Sym] = for {
    a <- alphaChar
    len <- choose[Int](0, 8)
    s <- stringOfN(len, genSymC)
  } yield Sym(a +: s)

  val genNIL: Gen[Sxpr] = const(NIL)

  def genQt(size: Int): Gen[Qt] =
    genSxpr(size) map Qt.apply

  def genSxpr(size: Int): Gen[Sxpr] =
    if (size >= 2) frequency(
      1 -> genQt(size * 2/3),
      7 -> genPair(size / 3),
    )
    else frequency(
      1 -> const(NIL),
      4 -> oneOf(genNumE, genSymE),
    )

  def genPair(size: Int): Gen[Pair] =
    for {
      car <- genSxpr(size * 3/4)
      cdr <- genSxpr(size * 1/4)
    } yield Pair(car, cdr)

  val genSxpr: Gen[Sxpr] = sized(genSxpr)

  implicit val arbSxpr: Arbitrary[Sxpr] = Arbitrary(genSxpr)
}
