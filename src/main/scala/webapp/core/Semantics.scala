package webapp.core
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Semantics {

  var C = 0

  def getFreeVars[A](t: Term[A]): Set[Var[A]] = t match {
    case v: Var[A] => Set(v)
    case Abstraction(v:Var[A], body:Term[A]) => getFreeVars(body) - v
    case Application(t1:Term[A], t2:Term[A]) => {
      val t1Vars = getFreeVars(t1)
      t1Vars ++ (getFreeVars(t2) -- t1Vars)
    }
    case _ => Set()
  }

  def substitution[A](srcTerm: Term[A], x: Var[A], t: Term[A]): Term[A] = srcTerm match {
    case Var(y) =>
      if (x == Var(y)) t else Var(y)
    case Abstraction(v:Var[A], body:Term[A]) =>
      if (x == v)
        Abstraction(v, body)
      else
        if (!getFreeVars(t).contains(v))
          Abstraction(v, substitution(body, x, t))
        else
          Abstraction(v, body)
    case Application(t1:Term[A], t2:Term[A]) =>
      Application(substitution(t1, x, t), substitution(t2, x, t))
    case _ => srcTerm
  }

  def conversion[A](term: Term[A], x: Var[A], y: Var[A]): Term[A] = term match {
    case Var(_) => term
    case Abstraction(v:Var[A], body:Term[A]) =>
      if (x == v)
        Abstraction(y, substitution(body, x, y))
      else
        Abstraction(v, conversion(body, x, y))
    case Application(t1:Term[A], t2:Term[A]) =>
      Application(conversion(t1, x, y), conversion(t2, x, y))
    case _ => term
  }

  def isValue[A](t: Term[A]): Boolean = t match {
    case Var(_) => true
    case Abstraction(_, _) => true
    case _ => false
  }

  @tailrec
  def evaluate[A](
      term: Term[A],
      redStrat: Term[A] => Term[A],
      callback: (Int, Term[A]) => List[(Int, Term[A])],
      count: Int = 0
   ): Term[A] = {
    val beta = redStrat(term)
    if (beta != term) {
      callback(count, term)
      C = count + 1
      evaluate(beta, redStrat, callback, count+1)
    } else {
      callback(count, term)
      term
    }
  }

  def evaluate[A](term: Term[A], redStrat: Term[A] => Term[A]): Term[A] =
    evaluate(term, redStrat, (count: Int, e: Term[A]) => List())

  def getEvaluateList[A](
                    term: Term[A],
                    redStrat: Term[A] => Term[A],
                  ): List[Term[A]] = {
    val beta = redStrat(term)
    if (beta != term) {
      term :: getEvaluateList(beta, redStrat)
    } else {
      beta :: List[Term[A]]()
    }
  }

  def normReduction[A](term: Term[A]): Term[A] = term match {
    case Application(Abstraction(v:Var[A], body:Term[A]), t2:Term[A]) =>
      substitution(body, v, t2)
    case Application(t1:Term[A], t2:Term[A]) =>
      if (!isValue(t1))
        Application(normReduction(t1), t2)
      else
        Application(t1, normReduction(t2))
    case Abstraction(v:Var[A], body:Term[A]) =>
      Abstraction(v, normReduction(body))
    case Var(_) => term
    case _ => term
  }

  def cbnReduction[A](term: Term[A]): Term[A] = term match {
    case Application(Abstraction(v:Var[A], body:Term[A]), t2:Term[A]) =>
      substitution(body, v, t2)
    case Application(t1:Term[A], t2:Term[A]) =>
      if (!isValue(t1))
        Application(cbnReduction(t1), t2)
      else
        Application(t1, cbnReduction(t2))
    case Abstraction(v:Var[A], body:Term[A]) =>
      Abstraction(v, body)
    case Var(_) => term
    case _ => term
  }

  def applReduction[A](term: Term[A]): Term[A] = term match {
    case Var(_) => term
    case Abstraction(v:Var[A], body:Term[A]) =>
      Abstraction(v, applReduction(body))
    case Application(Abstraction(v:Var[A], body:Term[A]), t2:Term[A]) =>
      if (isValue(t2))
        substitution(body, v, t2)
      else
        Application(Abstraction(v, body), applReduction(t2))
    case Application(t1:Term[A], t2:Term[A]) =>
      Application(applReduction(t1), t2)
    case _ => term
  }

  def cbvReduction[A](term: Term[A]): Term[A] = term match {
    case Var(_) => term
    case Abstraction(v:Var[A], body:Term[A]) =>
      Abstraction(v, body)
    case Application(Abstraction(v:Var[A], body:Term[A]), t2:Term[A]) =>
      if (isValue(t2))
        substitution(body, v, t2)
      else
        Application(Abstraction(v, body), cbvReduction(t2))
    case Application(t1:Term[A], t2:Term[A]) =>
      Application(applReduction(t1), t2)
    case _ => term
  }
}
