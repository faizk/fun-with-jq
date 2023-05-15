package tut

import cats._, cats.data._, cats.syntax.all._
import pc._

object calc {
  implicit def charIsCharP(c: Char): Parser[Char] = char(c)
  def op2[A,B,C](lp: => Parser[A], opp: Parser[_], rp: => Parser[B])(f: (A,B) => C): Parser[C] =
    for { l <- lp <* opp; r <- rp } yield f(l,r)
    //(lp <* opp).map(f.curried) <*> (yawn >> rp)
  
  def parens[A](pa: => Parser[A]): Parser[A] =
    yawn >> '(' >> pa <* ')'
  val ws = zeroOrMore(" \n\t".map(char).reduce(_ <+> _))
  
  lazy val expr:  Parser[Int] = op2(expr1, '-',  expr)(_-_) <+> expr1
  lazy val expr1: Parser[Int] = op2(expr2, '+', expr1)(_+_) <+> expr2
  lazy val expr2: Parser[Int] = op2(expr3, '*', expr2)(_*_) <+> expr3
  lazy val expr3: Parser[Int] = op2(expr4, '/', expr3)(_/_) <+> expr4
  lazy val expr4: Parser[Int] = ws >> { parens(expr) <+> posIntP } <* ws
}
