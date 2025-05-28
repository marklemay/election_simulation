
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Random

import optimus.optimization._
import optimus.optimization.enums.SolverLib
import optimus.optimization.model.MPFloatVar
import optimus.algebra.Int2Const

def scratchaergser(): Unit = {
  implicit val model: MPModel = MPModel(SolverLib.oJSolver)

  val x = MPFloatVar("x", 100, 200)
  val y = MPFloatVar("y", 80, 170)

  maximize(-2 * x + 5 * y)
  add(y >:= -x + 200)

  start()

  println(s"objective: $objectiveValue")
  println(s"x = ${x.value} y = ${y.value}")

  release()
}

@main
def byHand(): Unit = {
  import ch.qos.logback.classic.{Level, Logger}
  import org.slf4j.LoggerFactory

  LoggerFactory
    .getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME)
    .asInstanceOf[Logger]
    .setLevel(Level.OFF)

  implicit val model: MPModel = MPModel(SolverLib.oJSolver)
//  SolverLib.
//  Fig 1

  //A    0   20 - 20 100
  //B  -20    0   40 100
  //C   20  -40    0 100
  //D -100 -100 -100   0

  val w = 101

  val a = MPFloatVar("a")
  val b = MPFloatVar("b")
  val c = MPFloatVar("c")
  val d = MPFloatVar("d")

//  minimize(a + b + c + d)

  minimize(a * a + b * b + c *c + d *d)
  add(w *(a + b + c + d) := 1)

  add(a >:= 0)
  add(b >:= 0)
  add(c >:= 0)
  add(d >:= 0)

  // need to negate
  add(a * (   0+w) + b * ( -20+w) + c * (  20+w) + d * (-100+w) >:= 1)
  add(a * (  20+w) + b * (   0+w) + c * ( -40+w) + d * (-100+w) >:= 1)
  add(a * ( -20+w) + b * (  40+w) + c * (   0+w) + d * (-100+w) >:= 1)
  add(a * ( 100+w) + b * ( 100+w) + c * ( 100+w) + d * (   0+w) >:= 1)


  start()

  release()
  println(s"objective: $objectiveValue")
  println(s"x = ${a.value.get * w} y = ${b.value.get * w},  ${c.value.get * w},  ${d.value.get * w}")

}