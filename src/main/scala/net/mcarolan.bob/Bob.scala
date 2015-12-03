package net.mcarolan.bob

import com.pi4j.io.gpio.{PinState, RaspiPin, GpioFactory}

object BobMain extends App {


  val gpio = GpioFactory.getInstance()


  val Drive1 = gpio.provisionDigitalOutputPin(RaspiPin.GPIO_07, PinState.LOW)
  val Drive2 = gpio.provisionDigitalOutputPin(RaspiPin.GPIO_01, PinState.LOW)


  Drive1.high()
  Drive2.high()

  Thread.sleep(1000)

  Drive1.low()
  Drive2.low()

  println("I am bob")
}
