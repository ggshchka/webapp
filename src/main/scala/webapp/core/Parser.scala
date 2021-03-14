package webapp.core

import scala.util.parsing.combinator._

object Parser extends RegexParsers with PackratParsers {
  lazy val variable: PackratParser[Var[Nothing]] = Predef.augmentString("""[a-zA-Z]""").r ^^ Var[Nothing]

  lazy val noAppl: PackratParser[Term[_]] =
    abstraction | variable | "(" ~> term <~ ")"

  lazy val abstraction: PackratParser[Abstraction[_]] =
    (Predef.augmentString("""λ|lambda|\\|/\\""").r ~> variable <~ ".") ~ term ^^ {
      case v ~ body => Abstraction(v, body)
    }

  lazy val term: PackratParser[Term[_]] = {
    rep1(noAppl) ^^ {
      case x :: xs =>
        xs.foldLeft(x) { (func, arg) => Application(func, arg)
        }
    } | failure("illegal start of term")
  }

  def apply(input: String): Option[Term[_]] = parseAll(term, input) match {
    case Success(result, _) => Some(result)
    case NoSuccess(_, _) => None
  }
}
