package webapp

import outwatch._
import outwatch.dsl._
import cats.effect.{ExitCode, IO, IOApp, SyncIO}
import colibri.Observer
import webapp.js.TopComponent

object Webapp extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    for {
      topComponent <- TopComponent.init
      _ <- OutWatch.renderReplace[IO]("#app", div(topComponent.node))
    } yield ExitCode.Success
  }
}

