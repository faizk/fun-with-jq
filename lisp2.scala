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
  object SxprSeq {
    def unapplySeq(sxpr: Sxpr): Option[Seq[Sxpr]] = sxpr match {
      case Pair(h: Sxpr, SxprSeq(t@_*)) => Some(h +: t)
      case NIL => Some(Nil)
      case _ => None
    }
  }
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
  lazy val qteP: Parser[Qt] = char('\'') >> sxprP map Qt.apply
  lazy val listP: Parser[Sxpr] =
    yawn >> sxprP >>= (l => (((ws|yawn) >> listP) <+> happy(NIL)) map (r => Pair(l, r)))
  lazy val cellP: Parser[Pair] = yawn >>
    (sxprP <* (ws >> char('.') >> ws) map (l => Pair(l, _))) <*> sxprP
  lazy val consP:  Parser[Sxpr] = char('(') >> (cellP|listP) <* (ws|yawn) <* char(')')
  lazy val sxprP: Parser[Sxpr] = yawn >> qteP | litPosIntP | symP | consP <+> zilchP
  lazy val readP:  Parser[Sxpr] = (ws|yawn) >> sxprP <* (ws|yawn)

  case class ShowPrefs(renderConsList: Boolean = true)

  implicit def showSxpr(implicit prefs: ShowPrefs = ShowPrefs()): Show[Sxpr] = {
    case Lit(v)                                   => s"$v"
    case Sym(name)                                => show"""$name"""
    case NIL                                      => "()"
    case Qt(sxpr)                                 => show"""'$sxpr"""
    case Pair(car, NIL) if prefs.renderConsList   => show"""($car)"""
    case Pair(h, SxprSeq(t@_*)) if prefs.renderConsList =>
      (h +: t).map(_.show).mkString("(", " ", ")")
    case Pair(car, cdr)                           => show"""($car . $cdr)"""
  }
}
