package net.mcarolan.bob

import net.mcarolan.bob.BobMain._
import net.mcarolan.bob.RaspberryPi.{DigitalOutput, Controller}
import org.scalatest.{Matchers, FunSuite}

import scalaz.concurrent.Task
import scalaz.stream._
import scala.concurrent.duration._

class BobTest extends FunSuite with Matchers {

  import RaspberryPi.{State, High, Low}

  test("Bob can interpret a single Forward command") {
    val controller = RaspberryPi.StubController()
    val bob = BobMain.bob(controller)

    val commands: Process[Task, Command] = Process(Command(Forward, 1 second))

    (commands to bob).run.run

    controller.leftMotorSignals shouldBe Seq(High, Low, Low)
    controller.rightMotorSignals shouldBe Seq(High, Low, Low)

    controller.shutdownCalled shouldBe true
  }

  test("Bob can interpret multiple commands: a Forward followed by a Left") {
    val controller = RaspberryPi.StubController()
    val bob = BobMain.bob(controller)

    val commands: Process[Task, Command] = Process(Command(Forward, 1 second), Command(Left, 1 second))

    (commands to bob).run.run

    controller.leftMotorSignals shouldBe Seq(High, Low, High, Low, Low)
    controller.rightMotorSignals shouldBe Seq(High, Low, Low, Low, Low)

    controller.shutdownCalled shouldBe true
  }

}
