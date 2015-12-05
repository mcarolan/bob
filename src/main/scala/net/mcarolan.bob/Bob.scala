package net.mcarolan.bob

import com.pi4j.io.gpio._

import scala.concurrent.duration.Duration
import scalaz.concurrent.Task
import scalaz.stream._

object RaspberryPi {

  trait DigitalOutput {
    def high: Task[Unit]
    def low: Task[Unit]
  }

  trait Controller {
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

}

object BobMain extends App {

  import RaspberryPi._

  sealed trait Action extends ((Controller) => Task[Unit])

  object Forward extends Action {
    override def apply(controller: Controller): Task[Unit] =
      for {
        _ <- controller.leftMotor.high
        _ <- controller.rightMotor.high
      }
        yield ()
  }

  object Left extends Action {
    override def apply(controller: Controller): Task[Unit] =
      for {
        _ <- controller.leftMotor.high
        _ <- controller.rightMotor.low
      }
        yield ()
  }

  object Right extends Action {
    override def apply(controller: Controller): Task[Unit] =
      for {
        _ <- controller.leftMotor.low
        _ <- controller.leftMotor.high
      }
        yield ()
  }

  case class Command(action: Action, duration: Duration)

  val cleanUp: Controller => Task[Unit] = { controller =>
    for {
      _ <- Task { println("Turning bob off...") }
      _ <- controller.resetMotors
      _ <- controller.shutdown
      _ <- Task { println("Bob turned off") }
    }
      yield ()
  }

  def bob(controller: Controller): Sink[Task, Command] = {
    Process.await(Task(controller)) { controller =>
      Process repeatEval Task { command: Command =>
        interpret(controller, command)
      } onComplete (Process eval_ cleanUp(controller))
    }
  }

  def interpret(controller: Controller, command: Command): Task[Unit] =
       for {
         _ <- command.action(controller)
         _ <- Task { Thread.sleep(command.duration.toMillis) }
         _ <- controller.resetMotors
       }
         yield ()

}
