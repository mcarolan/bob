package net.mcarolan.bob

import com.pi4j.io.gpio._
import scalaz.concurrent.Task

object RaspberryPi {

  case class PiDigitalOutput(pin: GpioPinDigitalOutput) extends DigitalOutput {

    override def enterState(state: State): Task[Unit] = Task {
      state match {
        case High => pin.high()
        case Low => pin.low()
      }
    }

  }

  case class PiController(controller: GpioController) extends Controller {

    val leftMotor = PiDigitalOutput(controller.provisionDigitalOutputPin(RaspiPin.GPIO_07, PinState.LOW))
    val rightMotor = PiDigitalOutput(controller.provisionDigitalOutputPin(RaspiPin.GPIO_01, PinState.LOW))

    override def shutdown: Task[Unit] = Task {
      controller.shutdown()
    }

  }

}
