
package tut

import cats._, cats.data._, cats.syntax.all._

import pc._

sealed trait Sexpr; object Sexpr {
  case class Num(i: Int) extends Sexpr
  case class Sym(s: String) extends Sexpr
  case class Lst(l: NonEmptyList[Sexpr]) extends Sexpr
  case object Zilch extends Sexpr
  case class Qt(e: Sexpr) extends Sexpr
  case class Qqt(e: Sexpr) extends Sexpr
  case class Uqt(e: Sexpr) extends Sexpr

  val wsChars: Set[Char] = " \t\n".toSet
  val ws: Parser[Unit] = wsChars.map(char).reduce(_ <+> _).repeated *> happy(())
  val ws0: Parser[Unit] = ws | happy(())  
  val numP: Parser[Num] = posIntP.map(Num(_))
  val symCP: Parser[Char] = anyChar
    .reject { case ','|'`'|'\''|'('|')' => "can't contain [,'`]" }
    .reject { case c if wsChars contains c => "can't contain whitespace" }
  val symP: Parser[Sym] = symCP
    .reject { case c if c >= '0' && c <= '9' => "can't start with number" }
    .map[String=>String](c => c +: _) <*> symCP.repeated.orEmpty.map(_.mkString) map (Sym(_))
  val zilchP: Parser[Sexpr] = char('\'') *> char('(') *> char(')') *> happy(Zilch)
  lazy val qP:  Parser[Qt]  = char('\'') *> sexprP.map(Qt(_))
  lazy val qqP: Parser[Qqt] = char('`')  *> sexprP.map(Qqt(_))
  lazy val uqP: Parser[Uqt] = char(',')  *> sexprP.map(Uqt(_))

  lazy val lstP: Parser[Lst] = char('(') *> sexprP.sepBy(ws).map(Lst(_)) <* char(')')
  lazy val sexprP: Parser[Sexpr] = yawn >>  qP|qqP|uqP | numP | symP | lstP | zilchP 
  lazy val parser: Parser[Sexpr] = ws0 >> sexprP <* ws0
}

object lispy { import Sexpr._
  sealed trait Value { def apply(args: List[Value]): Either[Err, Value] }
  trait UnCallable { self: Value => override final def apply(args: List[Value]) = Left(s"type error: ${getClass.getName} Not callable") }
  case object ZilchVal extends Value with UnCallable
  case class Lit[A](a: A) extends Value with UnCallable
  case class Lambda(fargs: List[Sym], body: Sexpr, env: Map[String, Value]) extends Value {
    override def apply(args: List[Value]): Either[Err, Value] =
      if (args.size != fargs.size) Left(s"wrong arity (want: ${fargs.size}), got: ${args.size})")
      else eval(body, env ++ (fargs.map(_.s) zip args)) 
  }
  case class Cons(car: Value, cdr: Value) extends Value with UnCallable
  trait BuiltIn extends Value
  def numOp(f: (Int,Int)=>Int, id: Int): BuiltIn =
    _.map(assumeNum).sequence.map{ case l@(_::_::_)=>l; case l => id::l }.map(_ reduce f).map(Lit(_))
  def numOp(f: (Int,Int)=>Boolean): BuiltIn = {
    case Lit(l:Int)::Lit(r:Int)::Nil => Right(Lit(f(l,r)))
    case args => Left("wrong arity (want 2)")
  }
  val assumeNum: Value=>Either[Err, Int] = {
    case Lit(a: Int) => Right(a); case whatever => Left(s"Not a number: $whatever")
  }   
  val defaultEnv: Map[String, BuiltIn] = Map(
    "+"->numOp(_+_,0), "*"->numOp(_*_,1), "-"->numOp(_-_,0), "/"->numOp(_/_,1),
    ">"->numOp(_>_),   "<"->numOp(_<_),   "<="->numOp(_<=_), ">="->numOp(_>=_),
    "="    -> { case l::r::Nil => Right(Lit(l==r));  case no => Left(s"(=)wrong arity (want 2), got: $no") },
    "cons" -> { case l::r::Nil => Right(Cons(l, r)); case no => Left(s"(cons)wrong arity (want 2), got $no") },
    "car"  -> { case Cons(car,_)::Nil => Right(car); case no => Left(s"type error: not cons: $no") },
    "cdr"  -> { case Cons(_,cdr)::Nil => Right(cdr); case no => Left(s"type error: not cons: $no") },
  )
  def letBinding(env: Map[String, Value], kv: Sexpr): Either[Err, Map[String, Value]] = kv match {
    case Lst(NonEmptyList(Sym(name), expr :: Nil)) => eval(expr, env).map(value => env ++ Map(name -> value))
    case e => Left(s"syntax error: expected name-value pair, got [$e]")
  }
  val fargName: Sexpr => Either[Err, Sym] = {
    case name: Sym => Right(name); case whatever => Left(s"syntax error: invalid formal arg: $whatever")
  }

  def eval(e: Sexpr, env: Map[String, Value] = defaultEnv): Either[Err, Value] = e match {
    case Num(n) => Right(Lit(n))
    case Sym(s) => env.get(s).toRight(left=s"undefined var: $s")
    case Zilch  => Right(ZilchVal)
    // "SYNTAX": (a.k.a special forms)
    case Lst(NonEmptyList(Sym("let"), Lst(bindings) :: bodyE :: Nil)) => // TODO: not supporting side-effects
      bindings.foldM(env)(letBinding) >>= { newEnv => eval(bodyE, env ++ newEnv) }
    case Lst(NonEmptyList(Sym("let"), _)) => Left("syntax error")
    case Lst(NonEmptyList(Sym("lambda"), Lst(fargs) :: body :: Nil)) =>
      fargs.map(fargName).sequence.map(_.toList).map(Lambda(_, body, env))
    case Lst(NonEmptyList(Sym("lambda"), _)) => Left("syntax error")
    case Lst(NonEmptyList(Sym("if"), condE :: thenE :: elseE :: Nil)) => for {
        cond <- eval(condE, env)
        r    <- eval(if (cond == Lit(true)) thenE else elseE, env)
      } yield r
    case Lst(NonEmptyList(maybeF, argExprs)) => for {
        f    <- eval(maybeF, env)
        args <- argExprs.map(eval(_, env)).sequence
        r    <- f(args)
      } yield r

    // TODO: Qt etc
  } 
  def eval(s: String): Either[Err, Value] = Sexpr.parser(s).map(_._1).flatMap(eval(_))
} 
