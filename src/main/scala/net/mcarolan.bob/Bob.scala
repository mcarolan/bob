package net.mcarolan.bob

import com.pi4j.io.gpio._

import scala.concurrent.duration.Duration
import scalaz.concurrent.Task
import scalaz.stream._

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

  case class Command(action: Action, duration: Duration)

  def cleanUp(controller: Controller): Task[Unit] =
    for {
      _ <- Task { println("Turning bob off...") }
      _ <- controller.resetMotors
      _ <- controller.shutdown
      _ <- Task { println("Bob turned off") }
    }
      yield ()

  def bob(controller: => Controller): Sink[Task, Command] =
    Process.await(Task(controller)) { controller =>
      val interpreter = Process repeatEval Task(interpret(controller)_)
      interpreter onComplete (Process eval_ cleanUp(controller))
    }

  def interpret(controller: Controller)(command: Command): Task[Unit] =
    for {
      _ <- Task { println(command) }
      _ <- command.action(controller)
      _ <- Task { Thread.sleep(command.duration.toMillis) }
      _ <- controller.resetMotors
    }
      yield ()

}
