package webapp.js

import cats.effect.SyncIO
import outwatch._
import outwatch.dsl._
import outwatch.reactive.handler._
import webapp.api.CoreAPI

final private class ReductionComponent {

  def stepNode(s: String) =
    div(
      cls := "card",
      maxWidth := "50rem",
      div(
        cls := "card-body",
        div(
          cls := "sw-selectable card-text h3",
          fontSize := "22px",
          fontFamily := "Arev Sans",
          color := "#FFFFFF",
          padding := "3px",
          textAlign := "left",
          s,
        ),
        backgroundColor := "#3F3750",
      ),
    )

  val node: SyncIO[VNode] = for {
    term     <- Handler.create[String]
    redStrat <- Handler.create[String]("normal order")
    submits  <- Handler.create[String]
  } yield
    div(
      cls := "d-flex flex-column col-9",
      header(
        marginBottom := "1.14rem",
        h1("Вычисление терма"),
      ),
      div(
        cls := "reduction-wrapper",
        background := "#fff",
        padding := "15px",
        borderRadius := "5px",
        margin := "5px 5px 40px",
        div(
          cls := "form-row align-items-center",
          div(
            cls := "form-group col-md-6",
            //label(cls := "sr-only", `for` := "searchInputId", "Some Text"),
            input(
              `type` := "text",
              cls := "form-control mb-3",
              idAttr := "searchInputId",
              placeholder := "\\x.x",
              onInput.value --> term,
            ),
          ),
          div(
            cls := "form-group col-md-3",
            //label(`for` := "inputState", "State"),
            select(
              cls := "form-control mb-3",
              idAttr := "inputState",
              option(selected, value := "normal order", "normal order"),
              option(value := "application order", "application order"),
              option(value := "call-by-name", "call-by-name"),
              option(value := "call-by-value", "call-by-value"),
              onChange.value --> redStrat,
            ),
          ),
          div(
            cls := "form-group col-md-3",
            button(
              "Вычислить",
              idAttr := "reductionButton",
              `type` := "submit",
              backgroundColor := "#AD6ECC",
              cls := "btn btn-primary mb-3",
              onClick(term) --> submits,
            ),

          ),
        ),
        div(
          cls := "row justify-content-md-left",
          div(
            cls := "pr-1 col-9",
            submits.map { t =>
              redStrat.map(s =>
                CoreAPI
                  .evalTermWithSteps(t, s).zipWithIndex.map(x =>
                  stepNode((x._2 + 1).toString + ". " + x._1),
                ),
              )
            },
          ),
        ),
      ),
    )
}
