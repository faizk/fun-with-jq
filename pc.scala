//> using scala 2.13
//> using dep io.monix::monix-reactive:3.4.1

package tut

import cats._, cats.data._, cats.syntax.all._

object pc {

  type Err = String
  type Parser[A] = Seq[Char] => Either[Err, (A, Seq[Char])]
  
  implicit val pMonad: MonadError[Parser, Err] = new MonadError[Parser, Err] {
    def pure[A](x: A): Parser[A] = s => Right(x -> s)
    def flatMap[A, B](fa: Parser[A])(f: A=>Parser[B]): Parser[B] =
      s => fa(s) flatMap { case (a, rest) =>  f(a)(rest) }
    def tailRecM[A, B](a: A)(f: A => Parser[Either[A,B]]): Parser[B] =
      flatMap(f(a)) { case Right(b) => pure(b); case Left(a) => tailRecM(a)(f) }
    def handleErrorWith[A](fa: Parser[A])(f: Err => Parser[A]): Parser[A] =
      s => fa(s).left.flatMap(e => f(e)(s))
    def raiseError[A](e: Err): Parser[A] = _ => Left(e)
  }
  implicit val pMonoidK: MonoidK[Parser] = new MonoidK[Parser] {
    def empty[A]: Parser[A] = "EOF".raiseError[Parser, A]
    def combineK[A](x: Parser[A], y: Parser[A]): Parser[A] =
      s => x(s) orElse y(s)
  }
  
  def happy[A](a: A): Parser[A] = a.pure[Parser]
  def sad[A](e: Err): Parser[A] = e.raiseError[Parser, A]
  val yawn: Parser[Unit] = happy(())
  
  val anyChar: Parser[Char] =
    { case c +: rest => Right(c -> rest); case Nil => Left("EOF") } 
  def char(c: Char): Parser[Char] =
    anyChar >>= { case `c` => happy(c); case x => sad(s"want [$c]; got [$x]") }
  def char(f: Char => Boolean): Parser[Char] =
    anyChar reject { case c if !f(c) => s"unexpected [$c]" }
  
  def chars(cs: Seq[Char]): Parser[Seq[Char]] = cs.map(char).sequence
  def word(s: String): Parser[Unit] = chars(s).void
  
  def oneOrMore[A](p: Parser[A]): Parser[List[A]] =
    p >>= (a => zeroOrMore(p) map (a +: _))
  def zeroOrMore[A](p: Parser[A]): Parser[List[A]] =
    oneOrMore(p) <+> happy(Nil)
  
  val digit: Parser[Int] = anyChar >>= {
    case d if d >= '0' && d <= '9' => (d-'0').pure[Parser]
    case c => s"Not a digit: $c".raiseError[Parser, Int]
  }
  val posIntP: Parser[Int] = oneOrMore(digit) map {
    _.reverse.zipWithIndex.map { case (d,p) => d * math.pow(10,p).toInt }.sum
  }
  
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
  }
}
