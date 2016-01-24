package net.mcarolan.bob

import scalaz.concurrent.Task

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
