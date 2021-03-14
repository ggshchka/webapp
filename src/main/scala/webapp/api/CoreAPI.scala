package webapp.api

import webapp.core.{ContextTerm, Generator, Parser, Semantics}

object CoreAPI {

  def evalTerm(t: String, redStrat: String): String =
    Parser.apply(t) match {
      case Some(i) =>
        redStrat match {
          case "normal order" =>
            Semantics.evaluate[Nothing](i, Semantics.normReduction).toString
          case "application order" =>
            Semantics.evaluate[Nothing](i, Semantics.applReduction).toString
          case "call-by-name" =>
            Semantics.evaluate[Nothing](i, Semantics.cbnReduction).toString
          case "call-by-value" =>
            Semantics.evaluate[Nothing](i, Semantics.cbvReduction).toString
        }
      case None => "Error: non-parseable"
    }

  def evalTermWithSteps(t: String, redStrat: String): List[String] =
    Parser.apply(t) match {
      case Some(i) =>
        redStrat match {
          case "normal order" =>
            Semantics.getEvaluateList[Nothing](i, Semantics.normReduction).map(_.toString)
          case "application order" =>
            Semantics.getEvaluateList[Nothing](i, Semantics.applReduction).map(_.toString)
          case "call-by-name" =>
            Semantics.getEvaluateList[Nothing](i, Semantics.cbnReduction).map(_.toString)
          case "call-by-value" =>
            Semantics.getEvaluateList[Nothing](i, Semantics.cbvReduction).map(_.toString)
        }
      case None => List("Error: non-parseable")
    }

  def getContextTerm: (String, String) = {
    val t = Generator.genByCountOfSteps(4,5,exactly = false,3)
    val res = Semantics.evaluate[Nothing](t,Semantics.normReduction)
    val contextTerm = ContextTerm.getContext(t)
    (contextTerm.toString, res.toString)
  }

}
