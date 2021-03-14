package webapp.core

case class  Application[A](t1: A, t2: A) extends Term[A]{
  override def toString: String = t1.toString + " " + t2.toString
}
