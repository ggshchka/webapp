package webapp.js

import cats.effect.SyncIO
import colibri.Cancelable
import monix.reactive.Observable
import org.scalajs.dom
import outwatch.dsl._
import outwatch._
import outwatch.reactive.handler.Handler
import webapp.api.CoreAPI

import scala.collection.mutable.ListBuffer
import scala.scalajs.js.annotation.JSExport
import scala.util.control.Breaks.break


final case class ContextTermComponent private (boolStream: Handler[String]){
  @JSExport
  def cntxInputField(cntx: Handler[String], colorB: Handler[Boolean]) = input(
    cls := "term",
    `type` := "text",
    color := "",
    borderBottom := "",
    attr("animation") := "underliner_unsolved 2s steps(1) infinite",
    attr("transform") := "translateZ(0)",
    attr("will-change") := "transform, border-bottom",
    backgroundColor := "#fff",
    colorB.map(if (_) backgroundColor := "#228B22" else backgroundColor := "#b20000"),
    textAlign := "center",
    lineHeight := "11px",
    verticalAlign := "middle",
    padding := "2px 0px",
    marginLeft := "",
    marginRight := "",
    border := "0px solid #999",
    borderRadius := "4px",
    width := "20px",
    onInput.value --> cntx,
  )

  def getCntxWithInputFields(cntxs: List[Handler[String]], colorBList: List[Handler[Boolean]], contextTermList: List[Any]) = {
    var idx = -1
    contextTermList.map{
      el => if (el.isInstanceOf[String]) div(
        cls := "term",
        el.toString
      ) else {
        idx = idx + 1
        cntxInputField(cntxs(idx), colorBList(idx))
      }
    }
  }

  def createCntxHandlerList(n: Int): List[Any] = {
    if (n == 0) Nil
    else None :: createCntxHandlerList(n-1)
  }

  val node = for {
    submit <- Handler.create[Boolean]
  } yield {
    div(
      cls := "container-fluid h-100",
      div(
        cls := "d-flex flex-column col-9 min-vh-600",
        header(
          marginBottom := "1.14rem",
          h1("Заполнение пропусков"),
        ),
        button(
          "GO",
          idAttr := "reductionButton",
          `type` := "submit",
          backgroundColor := "#AD6ECC",
          cls := "btn btn-primary btn-lg",
          onClick(true) --> submit
        ),
        submit.map(if (_) {
          val contextTerm = CoreAPI.getContextTerm(None)
          val l: List[Handler[String]] = createCntxHandlerList(contextTerm._2).map(_ => Handler.create[String].unsafeRunSync())
          val lres: List[Handler[String]] = createCntxHandlerList(contextTerm._2).map(_ => Handler.create[String].unsafeRunSync())
          val t  = lres.lazyZip(contextTerm._4).map((x, y) => x.map(z => if (z == y) true else false))
          val booll: List[Handler[Boolean]] = createCntxHandlerList(contextTerm._2).map(_ => Handler.create[Boolean].unsafeRunSync())
          div(
            cls := "context-wrapper",
            background := "#a9b0c1",
            padding := "15px",
            borderRadius := "5px",
            margin := "5px 5px 40px",
            div(
              cls := "row d-flex justify-content-center",
              getCntxWithInputFields(l, booll, contextTerm._1),
            ),
            div(
              cls := "row d-flex justify-content-center divider py-1 bg-dark",
            ),
            div(
              cls := "row d-flex justify-content-center",
              div(
                cls := "term",
                contextTerm._5
              ),
            ),
            div(contextTerm._4.toString),
            div(
              cls := "d-flex flex-row-reverse p-1",
              button(
                "Проверить",
                idAttr := "reductionButton",
                `type` := "submit",
                backgroundColor := "#AD6ECC",
                cls := "btn btn-primary btn-lg",
                lres.lazyZip(l).map((x, y) => onClick(y.map(z => z)) --> x),
                booll.lazyZip(t).map((x, y) => onClick(y.map(z => z)) --> x),
              ),
            ),
          )
        } else div())
      ),
    )
  }
}


object ContextTermComponent {

  def init: SyncIO[ContextTermComponent] =
    for {
      boolStream <- Handler.create[String]("")
    } yield ContextTermComponent(boolStream)
}