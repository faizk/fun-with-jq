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

  type Loc = Int
  type Mem = Map[Loc, Value]
  implicit class MemOps(m: Mem) {
    def nextLoc: Loc = m.size
    def alloc(v: Value): (Loc, Mem) =
      nextLoc -> (m + (nextLoc -> v))
    def fetch(loc: Loc): Either[Err, Value] =
      m.get(loc).toRight("NPE")
  }
  type Env = Map[Sym, Loc]

  trait Proc extends Sxpr {
    def apply(args: List[Sxpr])(implicit mem: Mem): Either[String, Sxpr]
    override def toString(): String = "<proc?>"
  }
  val ensureCallable: Value => Either[Err, Proc] = {
    case proc: Proc => proc.asRight
    case uncallable => typeErr(s"$uncallable is not callable").asLeft
  }
  case class Lambda(fargs: List[Sym], body: Sxpr, env: Env) extends Proc {
    def apply(args: List[Value])(implicit mem: Mem): Either[Err, Value] =
      if (fargs.size != args.size) show"arity mismatch: given ${args.length} for expected ${fargs.length}".asLeft
      else
        (fargs zip args).foldLeft(env -> mem) { case ((e, m), (fa, a)) =>
          m.alloc(a) match { case (loc, newMem) => (env + (fa -> loc), newMem) }
        } match { case (env, mem) => eval(body, env)(mem) }
  }

  trait BuiltIn extends Proc
  object BuiltIn {
    def apply(f: List[Sxpr] => Either[Err, Sxpr]): BuiltIn = new BuiltIn {
      def apply(args: List[Sxpr])(implicit mem: Mem): Either[String,Sxpr] = f(args)
    }
  }
  def numOp(f: (Int,Int)=>Int, id: Int): BuiltIn = BuiltIn {
    _.map { case Lit(a: Int) => Right(a); case nan => typeErr(s"not a number: $nan").asLeft }
      .sequence.map { case l@(_::_::_)=>l; case l => id::l }.map(_ reduce f).map(Lit(_))
  }
  def numOp(f: (Int,Int)=>Boolean): BuiltIn = BuiltIn {
    case Lit(l:Int)::Lit(r:Int)::Nil => Right(Lit(f(l,r)))
    case args => Left("wrong arity (want 2)")
  }
  val builtInEnv: Map[Sym, BuiltIn] = Map(
    Sym("+")->numOp(_+_,0), Sym("*")->numOp(_*_,1), Sym("-")->numOp(_-_,0), Sym("/")->numOp(_/_,1),
    Sym(">")->numOp(_>_),   Sym("<")->numOp(_<_),   Sym("<=")->numOp(_<=_), Sym(">=")->numOp(_>=_),
    Sym("=")    -> BuiltIn { case l::r::Nil => Right(Lit(l==r));  case no => Left(s"(=)wrong arity (want 2), got: $no") },
    Sym("cons") -> BuiltIn { case l::r::Nil => Right(Pair(l, r)); case no => Left(s"(cons)wrong arity (want 2), got $no") },
    Sym("car")  -> BuiltIn { case Pair(car,_)::Nil => Right(car); case no => typeErr(no, classOf[Pair]) },
    Sym("cdr")  -> BuiltIn { case Pair(_,cdr)::Nil => Right(cdr); case no => typeErr(no, classOf[Pair]) },
    Sym("empty?") -> BuiltIn { case NIL::Nil => Right(Lit(true)); case _ => Right(Lit(false)) },
  )

  val kvPair: Sxpr => Either[Err, (Sym, Sxpr)] = {
    case Pair(k: Sym, Pair(v: Sxpr, NIL)) => (k -> v).asRight
    case e => s"syntax error: expected name-value pair, got [$e]".asLeft
  }

  val validFarg: Sxpr => Either[Err, Sym] =
    { case s: Sym => s.asRight; case e => show"syntax error: bad arg [$e]".asLeft }

  def eval(e: Sxpr, env: Env)(implicit mem: Mem): Either[String, Value] = e match {
    case Qt(sxpr)    => Right(sxpr)
    case lit: Lit[_] => Right(lit)
    case sym: Sym    => env.get(sym).toRight(left = show"undefined variable: [$sym]") >>= mem.fetch
    case NIL         => Right(NIL)
    case SxprSeq(Sym("let"), SxprSeq(bindings@_*), body: Sxpr) =>
      bindings.foldM(env->mem) { case ((env, mem), maybeKV) => kvPair(maybeKV) >>=
        { case (k, v) => eval(v, env)(mem) map mem.alloc map { case (loc, newMem) => (env + (k -> loc), newMem) } }
      } >>= (eval(body, _))
    case SxprSeq(Sym("letrec"), SxprSeq(bindings@_*), body: Sxpr) => for {
        kvs <- bindings.map(kvPair).sequence
        lEnv = kvs.map(_._1).zipWithIndex.map { case (sym, i) => sym -> (mem.nextLoc + i) }
        newEnv = env ++ lEnv
        newMem <- kvs.foldM(mem) { case (memSoFar, (k, sxpr)) =>
          eval(sxpr, newEnv)(memSoFar) map memSoFar.alloc map (_._2)
        }
        r <- eval(body, newEnv)(newMem)
      } yield r
    case SxprSeq(Sym("if"), condE, thenE, elseE) =>
      eval(condE, env) >>= { case Lit(false) => eval(elseE, env); case _ => eval(thenE, env) }
    case SxprSeq(Sym("lambda"), SxprSeq(fargEs@_*), body: Sxpr) =>
      fargEs.toList.map(validFarg).sequence map (Lambda(_, body, env))
    case x@SxprSeq(fsxpr, argEs@_*) => for {
        f    <- eval(fsxpr, env) >>= ensureCallable
        args <- argEs.toList.map(eval(_, env)).sequence
        r    <- f apply args
      } yield r
    case Pair(fexpr, whatever) => s"syntax error: nonsense after $fexpr: $whatever".asLeft
    case proc: Proc => Right(proc)
  }
  def eval(e: Sxpr, envMem: (Env, Mem)): Either[Err, Value] =
    envMem match { case (env, mem) => eval(e, env)(mem) }
  def eval(e: Sxpr): Either[Err, Value] =
    builtInEnv.foldLeft[(Env, Mem)](Map.empty -> Map.empty) { case ((env, mem), (sym, bi)) =>
      mem.alloc(bi) match { case (loc, newMem) => (env + (sym -> loc), mem + (loc -> bi)) }
    } match { case (env, mem) => eval(e, (env, mem)) }

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
