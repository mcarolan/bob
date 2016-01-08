package net.mcarolan.bob

import com.pi4j.io.gpio._

import scala.concurrent.duration._
import scala.io.StdIn
import scalaz.concurrent.Task
import scalaz.stream._
import org.http4s.server.blaze.BlazeBuilder
import org.http4s._
import org.http4s.dsl._
import org.http4s.server._

object RaspberryPi {

  sealed trait State
  case object High extends State
  case object Low extends State

  trait DigitalOutput {
    def enterState(state: State): Task[Unit]
  }

  trait Controller {
    val leftMotor: DigitalOutput
    val rightMotor: DigitalOutput
    def shutdown: Task[Unit]
    def resetMotors: Task[Unit] =
      for {
        _ <- leftMotor.enterState(Low)
        _ <- rightMotor.enterState(Low)
      }
        yield ()
  }

  case class PiDigitalOutput(pin: GpioPinDigitalOutput) extends DigitalOutput {

    override def enterState(state: State): Task[Unit] = Task {
      state match {
        case High => pin.high()
        case Low => pin.low()
      }
    }

  }

  case class PiController(controller: GpioController) extends Controller {

    val leftMotor = PiDigitalOutput(controller.provisionDigitalOutputPin(RaspiPin.GPIO_01, PinState.LOW))
    val rightMotor = PiDigitalOutput(controller.provisionDigitalOutputPin(RaspiPin.GPIO_07, PinState.LOW))

    override def shutdown: Task[Unit] = Task {
      controller.shutdown()
    }

  }

  case class StubController() extends Controller {

    var leftMotorSignals: Seq[State] = Seq.empty
    var rightMotorSignals: Seq[State] = Seq.empty
    var shutdownCalled = false

    override val leftMotor: DigitalOutput = new DigitalOutput {

      override def enterState(state: State): Task[Unit] = Task { leftMotorSignals = leftMotorSignals :+ state }

    }

    override val rightMotor: DigitalOutput = new DigitalOutput {

      override def enterState(state: State): Task[Unit] = Task { rightMotorSignals = rightMotorSignals :+ state }

    }

    override def shutdown: Task[Unit] = Task {
      shutdownCalled = true
    }

  }

}

object BobMain extends App {

  import RaspberryPi._

  case class Action(leftMotorState: State, rightMotorState: State) extends ((Controller) => Task[Unit]) {
    def apply(controller: Controller): Task[Unit] =
      for {
        _ <- controller.leftMotor enterState leftMotorState
        _ <- controller.rightMotor enterState rightMotorState
      }
        yield ()

      override def toString =
        s"left motor = $leftMotorState, right motor = $rightMotorState"
  }

  object Forward extends Action(High, High)
  object Left extends Action(High, Low)
  object Right extends Action(Low, High)
  object Halt extends Action(Low, Low)

  def cleanUp(controller: Controller): Task[Unit] =
    for {
      _ <- Task { println("Turning bob off...") }
      _ <- controller.resetMotors
      _ <- controller.shutdown
      _ <- Task { println("Bob turned off") }
    }
      yield ()

  def bob(controller: => Controller): Sink[Task, Action] =
    Process.await(Task(controller)) { controller =>
      val interpreter = Process repeatEval Task(interpret(controller)_)
      interpreter onComplete (Process eval_ cleanUp(controller))
    }

  def interpret(controller: Controller)(action: Action): Task[Unit] =
    for {
      _ <- Task { println(action) }
      _ <- action(controller)
    }
      yield ()

  val commands = async.boundedQueue[Action](10)

  val bobRoute = HttpService {

    case POST -> Root / "left" =>
      commands.enqueueOne(Left).flatMap(_ => Ok("Going left"))

    case POST -> Root / "right" =>
      commands.enqueueOne(Right).flatMap(_ => Ok("All right"))

    case POST -> Root / "forward" =>
      commands.enqueueOne(Forward).flatMap(_ => Ok("Straight up"))

    case POST -> Root / "halt" =>
      commands.enqueueOne(Halt).flatMap(_ => Ok("Stop right there!"))

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

  val runCommands = (commands.dequeue to bob(PiController(GpioFactory.getInstance())))

  (bobServer merge runCommands).run.run

}
