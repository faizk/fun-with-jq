package tut

import cats._, cats.data._, cats.syntax.all._

package object lisp { import pc._
  sealed trait Value {
    def apply(args: List[Value]): Either[Err, Value]
  }
  sealed trait Sexpr extends Value {
    override def apply(args: List[Value]) = show"""type: error: ${getClass().getTypeName()} not callable""".asLeft
  }
  case class Lit[A](a: A) extends Sexpr
  case class Sym(s: String) extends Sexpr
  case object Zilch extends Sexpr
  case class Cons(l: Value, r: Value) extends Sexpr
  object SexprSeq {
    def unapplySeq(sexpr: Sexpr): Option[Seq[Sexpr]] = sexpr match {
      case Cons(h: Sexpr, SexprSeq(t@_*)) => Some(h +: t)
      case Zilch => Some(Nil)
      case _ => None
    }
  }
  type Env = Map[Sym, Value]
  case class Lambda(fargs: List[Sym], body: Sexpr, env: Env) extends Value {
    def apply(args: List[Value]): Either[Err,Value] =
      if (fargs.size != args.size) show"arity mismatch: given ${args.length} for expected ${fargs.length}".asLeft
      else eval(body, env = env ++ (fargs zip args))
  }

  val wsChars: Set[Char] = " \t\n".toSet
  val ws: Parser[Unit] = wsChars.map(char).reduce(_ <+> _).repeated.void
  val litPosIntP: Parser[Lit[Int]] = posIntP map (Lit(_))
  val symCP: Parser[Char] = anyChar
    .reject { case c@(','|'`'|'\''|'('|')') => s"can't contain [$c]" }
    .reject { case c if wsChars contains c  => "can't contain whitespace" }
  val symP: Parser[Sym] = symCP
    .reject { case c if c >= '0' && c <= '9' => "can't start with number" }
    .reject { case '.' => "can't contain '.'" } // TODO: they actually can, just that `.` isn't a valid sym
    .map[String=>String](c => c +: _) <*> symCP.repeated.orEmpty.map(_.mkString) map (Sym(_))
  val zilchP: Parser[Zilch.type] = char('(') >> (ws|yawn) >> char(')') as Zilch
  lazy val listP: Parser[Sexpr] =
    yawn >> sexprP >>= (l => (((ws|yawn) >> listP) <+> happy(Zilch)) map (r => Cons(l, r)))
  // lazy val cellP: Parser[Cons] = yawn >> sexprP <* (ws >> char('.')) *>
  lazy val consP:  Parser[Sexpr] = char('(') >> listP <* (ws|yawn) <* char(')')
  lazy val sexprP: Parser[Sexpr] = yawn >> litPosIntP | symP | consP <+> zilchP <* (ws|yawn)
  lazy val readP:  Parser[Sexpr] = (ws|yawn) >> sexprP <* (ws|yawn)

  trait BuiltIn extends Value
  def numOp(f: (Int,Int)=>Int, id: Int): BuiltIn =
    _.map { case Lit(a: Int) => Right(a); case nan => Left(s"type-error: not a number: $nan") }
      .sequence.map { case l@(_::_::_)=>l; case l => id::l }.map(_ reduce f).map(Lit(_))
  def numOp(f: (Int,Int)=>Boolean): BuiltIn = {
    case Lit(l:Int)::Lit(r:Int)::Nil => Right(Lit(f(l,r)))
    case args => Left("wrong arity (want 2)")
  }
  val builtInEnv: Map[Sym, BuiltIn] = Map(
    Sym("+")->numOp(_+_,0), Sym("*")->numOp(_*_,1), Sym("-")->numOp(_-_,0), Sym("/")->numOp(_/_,1),
    Sym(">")->numOp(_>_),   Sym("<")->numOp(_<_),   Sym("<=")->numOp(_<=_), Sym(">=")->numOp(_>=_),
    Sym("=")    -> { case l::r::Nil => Right(Lit(l==r));  case no => Left(s"(=)wrong arity (want 2), got: $no") },
    Sym("cons") -> { case l::r::Nil => Right(Cons(l, r)); case no => Left(s"(cons)wrong arity (want 2), got $no") },
    Sym("car")  -> { case Cons(car,_)::Nil => Right(car); case no => Left(s"type error: not cons: $no") },
    Sym("cdr")  -> { case Cons(_,cdr)::Nil => Right(cdr); case no => Left(s"type error: not cons: $no") },
  )
  def newLetBind(env: Env, kv: Value): Either[Err, Env] = kv match {
    case Cons(sym: Sym, Cons(e: Sexpr, Zilch)) => eval(e, env).map(v => env ++ Map(sym -> v))
    case e => s"syntax error: expected name-value pair, got [$e]".asLeft
  }
  val validFarg: Sexpr => Either[Err, Sym] =
    { case s: Sym => s.asRight; case e => show"syntax error: bad arg [$e]".asLeft }

  def eval(e: Sexpr, env: Map[Sym, Value] = builtInEnv): Either[Err, Value] = e match {
    case lit: Lit[_] => Right(lit)
    case sym: Sym => env.get(sym).toRight(left = show"undefined variable: [$sym]")
    case Zilch => Right(Zilch)
    case SexprSeq(Sym("let"), SexprSeq(bindings@_*), body: Sexpr) =>
      bindings.toList.foldM(env)(newLetBind) >>= (eval(body, _))
    case SexprSeq(Sym("lambda"), SexprSeq(fargEs@_*), body: Sexpr) =>
      fargEs.toList.map(validFarg).sequence map (Lambda(_, body, env))
    case SexprSeq(fexpr: Sexpr, argEs@_*) => for {
        args  <- argEs.toList.map(eval(_, env = env)).sequence
        f     <- eval(fexpr, env = env)
        r     <- f(args)
      } yield r
    case Cons(fexpr, whatever) => s"syntax error: nonsense after $fexpr: $whatever".asLeft
  }
  def eval(s: String): Either[Err, Value] =
    readP(s) flatMap { case (e, Nil) => eval(e); case (e, leftOver) => s"trailing: $leftOver".asLeft } 

  implicit val showSexpr: Show[Sexpr] = {
    case Lit(v)                         => s"$v"
    case Sym(name)                      => show"""$name"""
    case Cons(car: Sexpr, Zilch)        => show"""($car)"""
    case Cons(car, Zilch)               => s"""($car)"""
    case Cons(h: Sexpr, SexprSeq(t@_*)) => (h +: t).map(_.show).mkString("(", " ", ")")
    case Cons(h, SexprSeq(t@_*))        => (h.toString +: t.map(_.show)).mkString("(", " ", ")")
    case Cons(car: Sexpr, cdr: Sexpr)   => show"""($car . $cdr)"""
    case Cons(car, cdr)                 => s"""($car . $cdr)"""
    case Zilch                          => "()"
  }
}
