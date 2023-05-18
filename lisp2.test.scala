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
