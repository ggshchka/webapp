package webapp.js

import cats.effect.{IO, SyncIO}
import outwatch._
import outwatch.dsl._

final private class NavComponent {

  def listGroupItem(name: String, hrf: String): BasicVNode = a(
    cls := "list-group-item list-group-item-action",
    href := hrf,
    name,
  )

  val node: SyncIO[VNode] = SyncIO(
    div(
      cls := "border-right",
      id := "sidebar-wrapper",
      div(
        cls := "sidebar-heading",
        color := "aliceblue",
        backgroundColor := "#1E2229",
        "Start Bootstrap",
      ),
      div(
        cls := "list-group list-group-flush",
        listGroupItem("Home", "#"),
        listGroupItem("About", "#"),
        listGroupItem("Portfolio", "#"),
        listGroupItem("Contact", "#"),
      ),
    ),
  )
}
