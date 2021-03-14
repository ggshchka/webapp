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

  def getContext[A](term: Term[A]): Any = {
    val r = new scala.util.Random
    val countOfElem = getCountOfElem(term)
    val boo = r.shuffle(1 until (countOfElem+1) toSet).take(getCountOfContexts(countOfElem))
    var c = 0
    def getRand(t: Term[A]): Any = t match {
      case Var(name) =>
        c = c + 1
        if (boo.contains(c)) None else Var(name)
      case Abstraction(v:Var[A], body:Term[A]) =>
        Abstraction(getRand(v), getRand(body))
      case Application(t1:Term[A], t2:Term[A]) =>
        Application(getRand(t1), getRand(t2))
    }
    getRand(term)
  }

}
