package webapp.core

case class  Abstraction[A](variable: A, body: A) extends Term[A]{
  override def toString: String = "(" + "\\" + variable.toString + "." + body + ")"
}
