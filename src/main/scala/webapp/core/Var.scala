package webapp.core

case class Var[A](name: String) extends Term[A] {
  override def toString: String = name
}
