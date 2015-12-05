package net.mcarolan.bob

import net.mcarolan.bob.BobMain.{Forward, Left, Command}
import net.mcarolan.bob.RaspberryPi.{DigitalOutput, Controller}
import org.scalatest.{Matchers, FunSuite}

import scalaz.concurrent.Task
import scalaz.stream._
import scala.concurrent.duration._

case class StubController() extends Controller {

  var leftMotorSignals: Seq[Boolean] = Seq.empty
  var rightMotorSignals: Seq[Boolean] = Seq.empty
  var shutdownCalled = false

  override val leftMotor: DigitalOutput = new DigitalOutput {

    override def high: Task[Unit] = Task { leftMotorSignals = leftMotorSignals :+ true }

    override def low: Task[Unit] = Task { leftMotorSignals = leftMotorSignals :+ false }

  }

  override val rightMotor: DigitalOutput = new DigitalOutput {

    override def high: Task[Unit] = Task { rightMotorSignals = rightMotorSignals :+ true }

    override def low: Task[Unit] = Task { rightMotorSignals = rightMotorSignals :+ false }

  }

  override def shutdown: Task[Unit] = Task {
    shutdownCalled = true
  }

}

class BobTest extends FunSuite with Matchers {

  test("Bob can interpret a single Forward command") {
    val controller = StubController()
    val bob = BobMain.bob(controller)

    val commands: Process[Task, Command] = Process(Command(Forward, 1 second))

    (commands to bob).run.run

    controller.leftMotorSignals shouldBe Seq(true, false, false)
    controller.rightMotorSignals shouldBe Seq(true, false, false)

    controller.shutdownCalled shouldBe true
  }

  test("Bob can interpret multiple commands: a Forward followed by a Left") {
    val controller = StubController()
    val bob = BobMain.bob(controller)

    val commands: Process[Task, Command] = Process(Command(Forward, 1 second), Command(Left, 1 second))

    (commands to bob).run.run

    controller.leftMotorSignals shouldBe Seq(true, false, true, false, false)
    controller.rightMotorSignals shouldBe Seq(true, false, false, false, false)

    controller.shutdownCalled shouldBe true
  }

}
