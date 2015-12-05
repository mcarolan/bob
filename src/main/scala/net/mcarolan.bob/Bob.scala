package net.mcarolan.bob

import java.util.concurrent.TimeoutException

import com.pi4j.io.gpio._

import scala.concurrent.duration.Duration
import scalaz.concurrent.Task
import scalaz.stream._
import scala.concurrent.duration._

object BobMain extends App {

  sealed trait DigitalOutput {
    def high: Task[Unit]
    def low: Task[Unit]
  }

  sealed trait Controller {
    val leftMotor: DigitalOutput
    val rightMotor: DigitalOutput
    def shutdown: Task[Unit]
    def resetMotors: Task[Unit] =
      for {
        _ <- leftMotor.low
        _ <- rightMotor.low
      }
        yield ()
  }

  case class StubDigitalOutput(pin: Pin) extends DigitalOutput {

    println(s"**** USING STUB MOTOR for ${pin.getName}****")

    def high: Task[Unit] = Task {
      println(s"high ${pin.getName}")
    }

    def low: Task[Unit] = Task {
      println(s"low ${pin.getName}")
    }

  }

  case class PiDigitalOutput(pin: GpioPinDigitalOutput) extends DigitalOutput {

    override def high: Task[Unit] = Task {
      pin.high()
    }

    override def low: Task[Unit] = Task {
      pin.low()
    }

  }

  case class PiController(controller: GpioController) extends Controller {

    val leftMotor = PiDigitalOutput(controller.provisionDigitalOutputPin(RaspiPin.GPIO_01, PinState.LOW))
    val rightMotor = PiDigitalOutput(controller.provisionDigitalOutputPin(RaspiPin.GPIO_07, PinState.LOW))

    override def shutdown: Task[Unit] = Task {
      controller.shutdown()
    }

  }

  case object StubController extends Controller {
    println("**** USING STUB CONTROLLER ****")

    val leftMotor = StubDigitalOutput(RaspiPin.GPIO_01)
    val rightMotor = StubDigitalOutput(RaspiPin.GPIO_07)

    override def shutdown: Task[Unit] = Task {
      println("shutting down controller")
    }
  }

  case class Bob(controller: Controller)

  sealed trait Action extends ((Bob) => Task[Unit])

  object Forward extends Action {
    override def apply(bob: Bob): Task[Unit] =
      for {
        _ <- bob.controller.leftMotor.high
        _ <- bob.controller.rightMotor.high
      }
        yield ()
  }

  object Left extends Action {
    override def apply(bob: Bob): Task[Unit] =
      bob.controller.leftMotor.high.map(_=>())
  }

  object Right extends Action {
    override def apply(bob: Bob): Task[Unit] =
      bob.controller.rightMotor.high.map(_=>())
  }

  case class Command(action: Action, duration: Duration)

  val cleanUp: Bob => Task[Unit] = { bob =>
    for {
      _ <- Task { println("Turning bob off...") }
      _ <- bob.controller.resetMotors
      _ <- bob.controller.shutdown
      _ <- Task { println("Bob turned off") }
    }
      yield ()
  }

  val createBob: Task[Bob] =
    for {
      _ <- Task { println("Creating bob...") }
      controller = StubController
    }
      yield Bob(controller)

  val commands: Process[Task, Command] = Process(Command(Forward, 10 seconds), Command(Forward, 5 seconds))

  val bob: Sink[Task, Command] = Process.await(createBob) { bob =>
    Process repeatEval Task { command: Command =>
      interpret(bob, command)
    } onComplete (Process eval_ cleanUp(bob))
  }

  def interpret(bob: Bob, command: Command): Task[Unit] =
       for {
         _ <- bob.controller.resetMotors
         _ <- command.action(bob)
         _ <- Task { Thread.sleep(command.duration.toMillis) }
         _ <- bob.controller.resetMotors
       }
         yield ()

  (commands to bob).run.run

}
