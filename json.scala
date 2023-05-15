package tut

import cats._, cats.data._, cats.syntax.all._
import pc._

object json {
  sealed trait JsVal
    case object JsNull extends JsVal
    sealed trait JsBool extends JsVal
      case object JsTrue  extends JsBool
      case object JsFalse extends JsBool
    case class JsInt(i: Int) extends JsVal
    case class JsStr(s: String) extends JsVal
    case class JsArr(a: Vector[JsVal]) extends JsVal
    case class JsObj(o: Map[String, JsVal]) extends JsVal

  val ws: Parser[Unit] = " \t\n".map(char).reduce(_ >> _).repeated.orEmpty.void

  val jsNullP: Parser[JsNull.type] = word("null") >> happy(JsNull)
  val jsBoolP: Parser[JsBool] = (word("true")  >> happy(JsTrue)) |
                                (word("false") >> happy(JsFalse))

  val jsIntP: Parser[JsInt] = posIntP map (JsInt(_))
  val jsStrP: Parser[JsStr] =
    (char('"') *> zeroOrMore(char(_!= '"')) <* char('"'))
      .map(_.mkString).map(JsStr(_))

  implicit val jsonP: Parser[JsVal] = ws >> {
      jsNullP | jsBoolP | jsIntP | jsStrP
    } <* ws
}

