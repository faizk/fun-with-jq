package tut

import cats._, cats.data._, cats.syntax.all._

object pc2 {

  type Pos[A] = (Int, A)
  type Err = NonEmptyChain[Pos[String]]
  type Parser[A] = Pos[Seq[Char]] => Either[Err, (A, Pos[Seq[Char]])]

  implicit val pMonad: MonadError[Parser, Err] = new MonadError[Parser, Err] {
    def pure[A](x: A): Parser[A] = s => Right(x -> s)
    def flatMap[A, B](fa: Parser[A])(f: A=>Parser[B]): Parser[B] =
      s => fa(s) flatMap { case (a, rest) =>  f(a)(rest) }
    def tailRecM[A, B](a: A)(f: A => Parser[Either[A,B]]): Parser[B] =
      flatMap(f(a)) { case Right(b) => pure(b); case Left(a) => tailRecM(a)(f) }
    def handleErrorWith[A](fa: Parser[A])(f: Err => Parser[A]): Parser[A] =
      s => fa(s).left.flatMap(e => f(e)(s))
    def raiseError[A](e: Err): Parser[A] = { case (i, _) => e.map(_.leftMap(_ + i)).asLeft }
  }
  implicit val pMonoidK: MonoidK[Parser] = new MonoidK[Parser] {
    def empty[A]: Parser[A] = { case (pos, _) => (pos -> "EOF").pure[NonEmptyChain].asLeft }
    def combineK[A](x: Parser[A], y: Parser[A]): Parser[A] =
      s => x(s) handleErrorWith { xe => y(s).adaptError(xe <+> _).adaptError(_.sorted.distinct) }
  }
  def happy[A](a: A): Parser[A] = a.pure[Parser]
  def sad[A](e: Err): Parser[A] = e.raiseError[Parser, A]
  def err(e: String): Err = (0 -> e).pure[NonEmptyChain]
  def sad[A](e: String): Parser[A] = err(e).raiseError[Parser, A]
  val yawn: Parser[Unit] = happy(())

  val anyChar: Parser[Char] = {
    case (i, c +: rest) => (c -> (i+1, rest)).asRight
    case (i, Nil)       => (i -> "EOF").pure[NonEmptyChain].asLeft
  }
  def char(c: Char): Parser[Char] =
    anyChar >>= { case `c` => happy(c); case x => sad(s"want [$c]; got [$x]") }
  def char(f: Char => Boolean): Parser[Char] =
    anyChar reject { case c if !f(c) => err(s"unexpected [$c]") }

  def chars(cs: Seq[Char]): Parser[Seq[Char]] = cs.map(char).sequence
  def word(s: String): Parser[Unit] = chars(s).void

  def oneOrMore[A](p: Parser[A]): Parser[List[A]] =
    p >>= (a => zeroOrMore(p) map (a +: _))
  def zeroOrMore[A](p: Parser[A]): Parser[List[A]] =
    oneOrMore(p) <+> happy(Nil)

  val digit: Parser[Int] = anyChar >>= {
    case d if d >= '0' && d <= '9' => (d-'0').pure[Parser]
    case c => sad(s"Not a digit: [$c]")
  }
  val posIntP: Parser[Int] = oneOrMore(digit) map {
    _.reverse.zipWithIndex.map { case (d,p) => d * math.pow(10,p).toInt }.sum
  }
  val intP: Parser[Int] = (char('-') *> posIntP map (-_)) <+> posIntP

  implicit class ParserOps[+A](l: Parser[A]) {
    def |[B >: A](r: => Parser[B]): Parser[B] = s => l(s) orElse r(s)
    def &[B, A1 >: A](r: Parser[B]): Parser[(A1,B)] = (l,r).tupled
    def orNone[B >: A]: Parser[Option[B]] =
      l.map(_.some) <+> none.pure[Parser]
    def orEmpty[B](implicit ev: A <:< NonEmptyList[B]): Parser[List[B]] =
      l.map(_.toList) <+> happy(Nil)
  }
  implicit class MoreParserOps[A](p: Parser[A]) {
    def repeated:  Parser[NonEmptyList[A]] =
      p flatMap (a => zeroOrMore(p) map(a.pure[NonEmptyList] ++ _))
    def sepBy(delimP: Parser[_]): Parser[NonEmptyList[A]] =
      for { a <- p; as <- (delimP *> p).repeated.orEmpty } yield NonEmptyList(a, as)
    // pc (v1) compat
    def apply(in: Seq[Char]): Either[Err, (A, Seq[Char])] = p(0 -> in) map (_.map(_._2))
    def apply(in: String): Either[Err, (A, Seq[Char])] = apply(in.toSeq)
  }
}

object pc2utils { import pc2._

  def rowAndCol(lines: Vector[(Range, String)], pos: Int): Option[(Int, Int)] =
    lines.zipWithIndex.find { case ((r, _), _) => r contains pos }
      .map { case ((r, l), i)  => (i+1, pos - r.min+1) }

  def lines(in: Seq[Char]): Vector[(Range, String)] =
    in.mkString.linesWithSeparators.toSeq.map(l => l.stripLineEnd -> l.length)
      .foldLeft[List[(Range, String)]](Nil) {
        case (Nil, (l, len))                => List((1 to len, l))
        case (soFar@((hr,_)::_), (l, len))  => ((hr.max+1 to hr.max+len), l) :: soFar
      }
      .reverse.toVector

  def report(in: Seq[Char], err: Err): Vector[String] = {
    val ls = lines(in)
    err.toList.flatMap { case (pos, msg) =>
      rowAndCol(ls, pos).toList.flatMap { case (row, col) =>
        ls(row-1)._2 :: ((" " * (col-1) :+ '^') ++ s" (line:$row, col:$col): $msg") :: "" :: Nil
      }
    }
    .toVector
  }

  implicit class FriendlyParserOps[A](p: Parser[A]) {
    def full: Seq[Char] => Either[Err, A] = p compose[Seq[Char]] (0 -> _) andThenF {
      case (a, (_, Nil)) => a.asRight
      case (a, (i, leftOver)) => (i -> s"[ERROR] trailing input: $leftOver").pure[NonEmptyChain].asLeft
    }

    def friendly: Seq[Char] => Either[String, A] =
      s => full(s) leftMap {
        report(s, _).mkString("## ERRORS: (start)\n", "\n", "## (fin)")
      }

  }
}
