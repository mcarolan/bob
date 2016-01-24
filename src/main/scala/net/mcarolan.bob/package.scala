package net.mcarolan

import scalaz.concurrent.Task

package object bob {

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
  }

  class Action(leftMotorState: State, rightMotorState: State) extends ((Controller) => Task[Unit]) {
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
  object Left extends Action(Low, High)
  object Right extends Action(High, Low)
  object Halt extends Action(Low, Low)

}
