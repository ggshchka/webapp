package webapp.api

import webapp.core.{ContextTerm, Generator, Parser, Semantics, Term}

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

  def getContextTerm[A](comp: A) = {
    val t = Generator.genByCountOfSteps(5,7,exactly = false,3)
    val res = Semantics.evaluate[Nothing](t,Semantics.normReduction)
    val contextTerm = ContextTerm.getContext(t, "_None_")
    val termList = ContextTerm.getListContextTerm(t)
    val contextTermList = contextTerm._1.toString.split("_").toList
    (contextTermList.map(el => if (el == "None") comp else el),
      contextTerm._2, termList, contextTerm._3.map(_.toString), res.toString, contextTerm._1)
  }

  def checkRes(cntxTerm: Any, strList: List[Any], res: Any): Boolean = {
    val strTerm = ContextTerm.suspToContextTerm(cntxTerm, strList, "_None_").toString
    val term = Semantics.evaluate[Nothing](Parser.apply(strTerm).get, Semantics.normReduction)
    Parser.apply(res.toString).get == term
  }

}
