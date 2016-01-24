package net.mcarolan.bob

import com.pi4j.io.gpio.GpioFactory
import scala.concurrent.duration._
import scala.io.StdIn
import scalaz.concurrent.Task
import scalaz.stream._
import org.http4s.server.blaze.BlazeBuilder
import org.http4s._
import org.http4s.dsl._
import org.http4s.server._

object BobMain extends App {

  import RaspberryPi._

  def cleanUp(controller: Controller): Task[Unit] =
    for {
      _ <- Task { println("Turning bob off...") }
      _ <- Halt(controller)
      _ <- controller.shutdown
      _ <- Task { println("Bob turned off") }
    }
      yield ()

  def bob(controller: => Controller): Sink[Task, Action] =
    Process.await(Task(controller)) { controller =>
      val interpreter: Sink[Task, Action] = Process repeatEval Task(interpret(controller)_)
      interpreter onComplete (Process eval_ cleanUp(controller))
    }

  def interpret(controller: Controller)(action: Action): Task[Unit] =
    for {
      _ <- Task { println(action) }
      _ <- action(controller)
    }
      yield ()

  val actions = async.boundedQueue[Action](10)

  def processActionRequest(action: Action): Task[Response] =
    actions.enqueueOne(action).flatMap(_ => Ok())

  val bobRoute = HttpService {

    case POST -> Root / "left" => processActionRequest(Left)

    case POST -> Root / "right" => processActionRequest(Right)

    case POST -> Root / "forward" => processActionRequest(Forward)

    case POST -> Root / "halt" => processActionRequest(Halt)

  }

  def sendResource(request: Request, path: String): Task[Response] =
    StaticFile.fromResource(path, Some(request)).fold(NotFound())(Task.now)

  val staticRoute = HttpService {

    case req @ (GET -> Root) =>
      sendResource(req, "/index.html")

    case req @ (GET -> path) =>
      sendResource(req, path.toString)

  }

  val bobServer: Process[Task, Server] = Process eval (BlazeBuilder.bindHttp(8080, "0.0.0.0")
    .mountService(staticRoute, "/")
    .mountService(bobRoute, "/api")
    .start)

  val actionReader: Process[Task, Action] = actions.dequeue

  val interpreter =  bob(PiController(GpioFactory.getInstance()))

  val runActions = (actionReader to interpreter)

  (bobServer merge runActions).run.run

}
