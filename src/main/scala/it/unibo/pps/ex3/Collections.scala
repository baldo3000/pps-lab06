package it.unibo.pps.ex3

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.FiniteDuration

object PerformanceUtils:
  case class MeasurementResults[T](result: T, duration: FiniteDuration) extends Ordered[MeasurementResults[_]]:
    override def compare(that: MeasurementResults[_]): Int = duration.toNanos.compareTo(that.duration.toNanos)

  def measure[T](msg: String)(expr: => T): MeasurementResults[T] =
    val startTime = System.nanoTime()
    val res = expr
    val duration = FiniteDuration(System.nanoTime() - startTime, TimeUnit.NANOSECONDS)
    if (msg.nonEmpty) println(msg + " -- " + duration.toNanos + " nanos; " + duration.toMillis + "ms")
    MeasurementResults(res, duration)

  def measure[T](expr: => T): MeasurementResults[T] = measure("")(expr)

@main def checkPerformance(): Unit =

  /* Linear sequences: List, ListBuffer */

  /* Indexed sequences: Vector, Array, ArrayBuffer */

  /* Sets */

  /* Maps */

  /* Comparison */
  import PerformanceUtils.*
  val lst = (1 to 10000000).toList
  val vec = (1 to 10000000).toVector
  val arr = (1 to 10000000).toArray
  measure("list last")(lst.last)
  measure("vec last")(vec.last)
  measure("arr last")(arr.last)
  measure("list middle")(lst(5000000))
  measure("vec middle")(vec(5000000))
  measure("arr middle")(arr(5000000))
  measure("list middle")(lst.find(_ == 5000000))
  measure("vec find middle")(vec.find(_ == 5000000))
  measure("arr find middle")(arr.find(_ == 5000000))
