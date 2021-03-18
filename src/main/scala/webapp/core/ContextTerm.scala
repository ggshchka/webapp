package webapp.core

object ContextTerm{

  def getCountOfElem[A](t: Term[A]): Int = t match {
    case Var(_) => 1
    case Abstraction(v:Var[A], body:Term[A]) =>
      getCountOfElem(v) + getCountOfElem(body)
    case Application(t1:Term[A], t2:Term[A]) =>
      getCountOfElem(t1) + getCountOfElem(t2)
  }

  def getCountOfContexts(countOfElem: Int): Int = countOfElem / 3

  def getContext[A](term: Term[A], comp: Any): (Any, Int, List[Any]) = {
    val r = new scala.util.Random
    val countOfElem = getCountOfElem(term)
    val boo = r.shuffle(1 until (countOfElem+1) toSet).take(getCountOfContexts(countOfElem))
    var c = 0
    var changedElemList: List[Any] = Nil
    def getRand(t: Term[A]): Any = t match {
      case Var(name) =>
        c = c + 1
        if (boo.contains(c)) {
          changedElemList = Var(name) :: changedElemList
            comp
        } else Var(name)
      case Abstraction(v:Var[A], body:Term[A]) =>
        Abstraction(getRand(v), getRand(body))
      case Application(t1:Term[A], t2:Term[A]) =>
        Application(getRand(t1), getRand(t2))
    }
    (getRand(term), getCountOfContexts(countOfElem), changedElemList)
  }

  def getListContextTerm[A](term: Any): List[Any] = term match {
    case Abstraction(v, body) =>
      v :: getListContextTerm(body)
    case Application(t1, t2) =>
      getListContextTerm(t1) ++ getListContextTerm(t2)
    case x => x :: List[Any]()
  }

  def suspToContextTerm[A](cntxTerm: Any, resList: List[Any], comp: Any): Any = {
    var idx = -1
    cntxTerm match {
      case Abstraction(v, body) =>
        Abstraction(suspToContextTerm(v, resList, comp), suspToContextTerm(body, resList, comp))
      case Application(t1, t2) =>
        Application(suspToContextTerm(t1, resList, comp), suspToContextTerm(t2, resList, comp))
      case x =>
        if (x == comp) {
          idx = idx + 1
          resList(idx)
        } else x
    }
  }


//  def main(args: Array[String]): Unit = {
//    val t = Generator.genByCountOfSteps(4,10,exactly = false,3)
//    val res = Semantics.evaluate[Nothing](t,Semantics.normReduction)
//    val contextTerm = ContextTerm.getContext(t, "_None_")
//    val contextTermList = contextTerm.toString.split("_").toList
//    val resTerm = suspToContextTerm[Nothing](contextTerm._1, Var("a")::Var("a")::Var("a")::Nil, "_None_")
//    println(res)
//    println(contextTerm)
//    println(resTerm)
////    println(t)
////    println(getListContextTerm(t))
////    println(getListContextTerm(contextTerm))
////    println(contextTermList)
//  }
}
