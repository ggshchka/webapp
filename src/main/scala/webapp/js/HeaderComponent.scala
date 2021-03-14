package webapp.js

import cats.effect.{IO, SyncIO}
import outwatch._
import outwatch.dsl._
import outwatch.reactive.handler._

final case class HeaderComponent private (boolStream: Handler[Boolean]) {

  val valueStream: Handler[Boolean] = boolStream

  private def toggleSlider = button(
    cls := "btn btn-primary",
    id := "menu-toggle",
    onClick(boolStream.map(!_)) --> boolStream,
    span(cls := "navbar-toggler-icon"),
  )

  def node: VDomModifier = VDomModifier(
    div(
      cls := "nav navbar navbar-expand-lg navbar-light bg-light border-bottom",
      toggleSlider,
      button(
        cls := "navbar-toggler",
        `type` := "button",
        attr("data-toggle") := "collapse",
        attr("data-target") := "#navbarSupportedContent",
        attr("aria-controls") := "navbarSupportedContent",
        attr("aria-expanded") := "false",
        attr("aria-label") := "Toggle navigation",
        span(cls := "navbar-toggler-icon"),
      ),
      div(
        cls := "collapse navbar-collapse",
        idAttr := "navbarSupportedContent",
        ul(
          cls := "navbar-nav ml-auto mt-2 mt-lg-0",
          li(
            cls := "nav-item active",
            a(
              cls := "nav-link",
              href := "#",
              "Home",
              span(cls := "sr-only", "(current)"),
            ),
          ),
          li(
            cls := "nav-item ",
            a(cls := "nav-link", href := "#", "Link"),
          ),
          li(
            cls := "nav-item dropdown",
            a(
              cls := "nav-link dropdown-toggle",
              href := "#",
              idAttr := "navbarDropdown",
              role := "button",
              attr("data-toggle") := "dropdown",
              attr("aria-haspopup") := "true",
              attr("aria-expanded") := "false",
              "Dropdown",
            ),
            div(
              cls := "dropdown-menu dropdown-menu-right",
              attr("aria-labelledby") := "navbarDropdown",
              a(cls := "dropdown-item", href := "#", "Action"),
              a(
                cls := "dropdown-item",
                href := "#",
                "Another action",
              ),
              div(cls := "dropdown-divider"),
              a(
                cls := "dropdown-item",
                href := "#",
                "Something else here",
              ),
            ),
          ),
        ),
      ),
    ),
  )
}

object HeaderComponent {

  def init: SyncIO[HeaderComponent] =
    for {
      boolStream <- Handler.create[Boolean](true)
    } yield HeaderComponent(boolStream)
}
