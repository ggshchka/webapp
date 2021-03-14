package webapp.js

import cats.effect.SyncIO
import outwatch.dsl._
import outwatch._
import outwatch.reactive.handler.Handler
import webapp.api.CoreAPI

final case class ContextTermComponent private (boolStream: Handler[Boolean]){

  val node = div(
    cls := "container-fluid h-100",
    div(
      cls := "d-flex flex-column col-9 min-vh-600",
      header(
        marginBottom := "1.14rem",
        h1("Заполнение пропусков"),
      ),
      div(
        cls := "context-wrapper",
        background := "#fff",
        padding := "15px",
        borderRadius := "5px",
        margin := "5px 5px 40px",
        height := "500px",
        CoreAPI.getContextTerm._1,
        "     --------->     ",
        CoreAPI.getContextTerm._2,
        //CoreAPI.getContextTerm._1 + CoreAPI.getContextTerm._2,
      ),
    ),
  )
}


object ContextTermComponent {

  def init: SyncIO[ContextTermComponent] =
    for {
      boolStream <- Handler.create[Boolean](true)
    } yield ContextTermComponent(boolStream)
}