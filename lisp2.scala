package tut

import cats._, cats.data._, cats.syntax.all._

package object lisp2 { import pc2._

  sealed trait Sxpr
  sealed trait Atom extends Sxpr
  case class Lit[A](a: A) extends Atom
  case class Sym(s: String) extends Atom
  case object NIL extends Sxpr
  case class  Pair(l: Sxpr, r: Sxpr) extends Sxpr
  case class Qt(sxpr: Sxpr) extends Sxpr

  val wsChars: Set[Char] = " \t\n".toSet
  val ws: Parser[Unit] = wsChars.map(char).reduce(_ <+> _).repeated.void
  val litPosIntP: Parser[Lit[Int]] = posIntP map (Lit(_))
  val symCP: Parser[Char] = anyChar
    .reject { case c@(','|'`'|'\''|'('|')') => err(s"can't contain [$c]") }
    .reject { case c if wsChars contains c  => err("can't contain whitespace") }
  val symP: Parser[Sym] = symCP
    .reject { case c if c >= '0' && c <= '9' => err("can't start with number") }
    .reject { case '.' => err("can't contain '.'") } // TODO: they actually can, just that `.` isn't a valid sym
    .map[String=>String](c => c +: _) <*> symCP.repeated.orEmpty.map(_.mkString) map (Sym(_))
  val zilchP: Parser[NIL.type] = char('(') >> (ws|yawn) >> char(')') as NIL
  lazy val qteP: Parser[Qt] = char('\'') >> sexprP map Qt.apply
  lazy val listP: Parser[Sxpr] =
    yawn >> sexprP >>= (l => (((ws|yawn) >> listP) <+> happy(NIL)) map (r => Pair(l, r)))
  lazy val cellP: Parser[Pair] = yawn >>
    (sexprP <* (ws >> char('.') >> ws) map(l => Pair(l, _))) <*> sexprP
  lazy val cellP1: Parser[Pair] = yawn >>
    (sexprP <* (ws >> char('.') >> ws)) >>= (l => sexprP map (Pair(l, _)))
  lazy val consP:  Parser[Sxpr] = char('(') >> (cellP|listP) <* (ws|yawn) <* char(')')
  lazy val sexprP: Parser[Sxpr] = yawn >> qteP | litPosIntP | symP | consP <+> zilchP
  lazy val readP:  Parser[Sxpr] = (ws|yawn) >> sexprP <* (ws|yawn)

  implicit val showSxpr: Show[Sxpr] = {
    case Lit(v)                         => s"$v"
    case Sym(name)                      => show"""$name"""
    case Pair(car: Sxpr, NIL)           => show"""($car)"""
    case Pair(car, NIL)                 => s"""($car)"""
    //case Pair(h: Sxpr, SxprSeq(t@_*)) => (h +: t).map(_.show).mkString("(", " ", ")")
    //case Pair(h, SxprSeq(t@_*))       => (h.toString +: t.map(_.show)).mkString("(", " ", ")")
    case Pair(car: Sxpr, cdr: Sxpr)     => show"""($car . $cdr)"""
    case Pair(car, cdr)                 => s"""($car . $cdr)"""
    case NIL                            => "()"
    case Qt(sxpr)                       => show"""'$sxpr"""
  }
}
