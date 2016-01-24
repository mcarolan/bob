package net.mcarolan.bob

import net.mcarolan.bob.BobMain._
import org.scalatest.{Matchers, FunSuite}

import scalaz.concurrent.Task
import scalaz.stream._
import scala.concurrent.duration._

class BobTest extends FunSuite with Matchers {

  test("Bob can interpret a single Forward command") {
    val controller = StubController()
    val bob = BobMain.bob(controller)

    val commands: Process[Task, Action] = Process(Forward, Halt)

    (commands to bob).run.run

    withClue("left motor signals") { controller.leftMotorSignals shouldBe Seq(High, Low, Low) }
    withClue("right motor signals") { controller.rightMotorSignals shouldBe Seq(High, Low, Low) }

    controller.shutdownCalled shouldBe true
  }

  test("Bob can interpret multiple commands: a Forward followed by a Left") {
    val controller = StubController()
    val bob = BobMain.bob(controller)

    val commands: Process[Task, Action] = Process(Forward, Halt, Left, Halt)

    (commands to bob).run.run

    withClue("left motor signals") { controller.leftMotorSignals shouldBe Seq(High, Low, Low, Low, Low) }
    withClue("right moror signals") { controller.rightMotorSignals shouldBe Seq(High, Low, High, Low, Low) }

    controller.shutdownCalled shouldBe true
  }

}
