package tut

import cats._, cats.data._, cats.syntax.all._

package object lisp2 { import pc2._
  type Err = String
  type Value = Sxpr

  sealed trait Sxpr
  sealed trait Atom extends Sxpr
  case class Lit[A](a: A) extends Atom
  case class Sym(s: String) extends Atom
  case object NIL extends Atom
  case class  Pair(l: Sxpr, r: Sxpr) extends Sxpr
  case class Qt(sxpr: Sxpr) extends Sxpr
  object SxprSeq {
    def unapplySeq(sxpr: Sxpr): Option[Seq[Sxpr]] = sxpr match {
      case Pair(h: Sxpr, SxprSeq(t@_*)) => Some(h +: t)
      case NIL => Some(Nil)
      case _ => None
    }
  }
  def typeErr(msg: String): Err = s"type-error: $msg"
  def typeErr[A](got: A, want: Class[_]): Either[Err, Value] =
    typeErr(s"${got.getClass.getName} != ${want.getName}").asLeft
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

  type Env = Map[Sym, Sxpr]

  trait Proc extends Sxpr {
    def apply(arge: List[Sxpr]): Either[String, Sxpr]
  }
  val ensureCallable: Value => Either[Err, Proc] = {
    case proc: Proc => proc.asRight
    case uncallable => typeErr(s"$uncallable is not callable").asLeft
  }
  case class Lambda(fargs: List[Sym], body: Sxpr, env: Env) extends Proc {
    def apply(args: List[Value]): Either[Err, Value] =
      if (fargs.size != args.size) show"arity mismatch: given ${args.length} for expected ${fargs.length}".asLeft
      else eval(body, env = env ++ (fargs zip args))
  }
  trait BuiltIn extends Proc
  def numOp(f: (Int,Int)=>Int, id: Int): BuiltIn =
    _.map { case Lit(a: Int) => Right(a); case nan => typeErr(s"not a number: $nan").asLeft }
      .sequence.map { case l@(_::_::_)=>l; case l => id::l }.map(_ reduce f).map(Lit(_))
  def numOp(f: (Int,Int)=>Boolean): BuiltIn = {
    case Lit(l:Int)::Lit(r:Int)::Nil => Right(Lit(f(l,r)))
    case args => Left("wrong arity (want 2)")
  }
  val builtInEnv: Map[Sym, BuiltIn] = Map(
    Sym("+")->numOp(_+_,0), Sym("*")->numOp(_*_,1), Sym("-")->numOp(_-_,0), Sym("/")->numOp(_/_,1),
    Sym(">")->numOp(_>_),   Sym("<")->numOp(_<_),   Sym("<=")->numOp(_<=_), Sym(">=")->numOp(_>=_),
    Sym("=")    -> { case l::r::Nil => Right(Lit(l==r));  case no => Left(s"(=)wrong arity (want 2), got: $no") },
    Sym("cons") -> { case l::r::Nil => Right(Pair(l, r)); case no => Left(s"(cons)wrong arity (want 2), got $no") },
    Sym("car")  -> { case Pair(car,_)::Nil => Right(car); case no => typeErr(no, classOf[Pair]) },
    Sym("cdr")  -> { case Pair(_,cdr)::Nil => Right(cdr); case no => typeErr(no, classOf[Pair]) },
  )

  def newLetBind(env: Env, kv: Sxpr): Either[String, Env] = kv match {
    case Pair(sym: Sym, Pair(e: Sxpr, NIL)) => eval(e, env).map(v => env ++ Map(sym -> v))
    case e => s"syntax error: expected name-value pair, got [$e]".asLeft
  }
  val validFarg: Sxpr => Either[Err, Sym] =
    { case s: Sym => s.asRight; case e => show"syntax error: bad arg [$e]".asLeft }

  def eval(e: Sxpr, env: Env = builtInEnv): Either[String, Value] = e match {
    case Qt(sxpr)    => Right(sxpr)
    case lit: Lit[_] => Right(lit)
    case sym: Sym    => env.get(sym).toRight(left = show"undefined variable: [$sym]")
    case NIL         => Right(NIL)
    case SxprSeq(Sym("let"), SxprSeq(bindings@_*), body: Sxpr) =>
      bindings.toList.foldM(env)(newLetBind) >>= (eval(body, _))
    case SxprSeq(Sym("if"), condE, thenE, elseE) =>
      eval(condE, env) >>= { case Lit(false) => eval(elseE, env); case _ => eval(thenE, env) }
    case SxprSeq(Sym("lambda"), SxprSeq(fargEs@_*), body: Sxpr) =>
      fargEs.toList.map(validFarg).sequence map (Lambda(_, body, env))
    case SxprSeq(fsxpr, argEs@_*) => for {
        f    <- eval(fsxpr, env) >>= ensureCallable
        args <- argEs.toList.map(eval(_, env)).sequence
        r    <- f apply args
      } yield r
    case Pair(fexpr, whatever) => s"syntax error: nonsense after $fexpr: $whatever".asLeft
    case proc: Proc => Right(proc)
  }

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
    case proc: Proc                               => s"#<procedure>$proc"
  }
}
