package webapp.js

import cats.effect.IO
import outwatch._
import outwatch.dsl._

final case class TopComponent(
  headerComponent: HeaderComponent,
  contextTermComponent: ContextTermComponent,
  navComponent: VNode,
  reductionComponent: VNode,
) {

  def node =
      VDomModifier(
        div(
          cls := "d-flex",
          idAttr := "wrapper",
          headerComponent.valueStream.map(if (_) navComponent else div()),
          div(
            idAttr := "page-content-wrapper",
            headerComponent.node,
            //reductionComponent,
            contextTermComponent.node,
          ),
        ),
      )

}

object TopComponent {

  def init: IO[TopComponent] =(
    for {
      header                    <- HeaderComponent.init
      contextTermComponent      <- ContextTermComponent.init
      navComponent              <- new NavComponent().node
      reductionComponent        <- new ReductionComponent().node
    } yield
      TopComponent(
        header,
        contextTermComponent,
        navComponent,
        reductionComponent,
      )).toIO

}
