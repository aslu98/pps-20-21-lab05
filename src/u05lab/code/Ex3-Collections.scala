package u05lab.code

import u05lab.code.PerformanceUtils.{calculatePerformance, measure}

import java.util.concurrent.TimeUnit
import scala.collection.immutable.HashMap
import scala.collection.{mutable, _}
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.FiniteDuration

object PerformanceUtils {
  case class MeasurementResults[T](result: T, duration: FiniteDuration) extends Ordered[MeasurementResults[_]] {
    override def compare(that: MeasurementResults[_]): Int = duration.toNanos.compareTo(that.duration.toNanos)
  }

  def measure[T](msg: String)(expr: => T): MeasurementResults[T] = {
    val startTime = System.nanoTime()
    val res = expr
    val duration = FiniteDuration(System.nanoTime()-startTime, TimeUnit.NANOSECONDS)
    if(!msg.isEmpty) println(msg + " -- " + duration.toNanos + " nanos; " + duration.toMillis + "ms")
    MeasurementResults(res, duration)
  }

  def measure[T](expr: => T): MeasurementResults[T] = measure("")(expr)

  def calculatePerformance[A](seqs: Map[String, Iterable[A]], numberOfElems: Int): Unit ={
    println()
    println("LAST")
    for ((name, seq) <- seqs) measure(name + " last"){seq.last}
    println()
    println("SIZE")
    for ((name, seq) <- seqs) measure(name + " size"){ seq.size }
    println()
    println("UPDATE")
    for ((name, seq) <- seqs) measure(name + " update"){ seq.map(_+ "up") }
    println()
    println("DELETE")
    for ((name, seq) <- seqs) measure(name + " delete"){ seq.dropRight(numberOfElems) }
  }
}

object CollectionsTest extends App {

  private val numberOfElems: Int = 1000000
  private var iterables: mutable.LinkedHashMap[String, Iterable[Int]] = mutable.LinkedHashMap()
  println("CREATE")

  /* Linear sequences: List, ListBuffer */

  measure("IMM list create") {scala.collection.immutable.List.range(1, numberOfElems)}
  iterables += ("IMM list" -> scala.collection.immutable.List.range(1, numberOfElems))
  measure("MUT bufferList create") {mutable.ListBuffer.range(1, numberOfElems)}
  iterables += ("MUT bufferList" -> mutable.ListBuffer.range(1, numberOfElems))

  /* Indexed sequences: Vector, Array, ArrayBuffer */
  measure("IMM vector create") {Vector.range(1, numberOfElems)}
  iterables += ("IMM vector" -> Vector.range(1, numberOfElems))
  measure("PARMUT array create") {Array.range(1, numberOfElems)}
  iterables += ("PARMUT array" -> Array.range(1, numberOfElems))
  measure("MUT arrayBuffer create") {mutable.ArrayBuffer.range(1, numberOfElems)}
  iterables += ("MUT arrayBuffer" -> mutable.ArrayBuffer.range(1, numberOfElems))

  /* Sets */
  measure("IMM hashset create") {immutable.HashSet.range(1,numberOfElems)}
  iterables += ("IMM hashset" -> (immutable.HashSet.range(1,numberOfElems)))
  measure("MUT treeset create") {mutable.TreeSet.from(1 to numberOfElems)}
  iterables += ("MUT treeset" -> (mutable.TreeSet.from(1 to numberOfElems)))

  /* Maps */
  measure("IMM hashmap create") {immutable.HashMap((1,numberOfElems) -> (numberOfElems, numberOfElems*2))}
  iterables += ("IMM hashmap" -> immutable.HashMap((1,numberOfElems) -> (numberOfElems, numberOfElems*2)).asInstanceOf[Iterable[Int]])
  measure("MUT treemap create") {mutable.TreeMap((1,numberOfElems) -> (1,numberOfElems))}
  iterables += ("MUT treemap" -> mutable.TreeMap((1,numberOfElems) -> (1,numberOfElems)).asInstanceOf[Iterable[Int]])

  import PerformanceUtils._
  calculatePerformance[Int](iterables, numberOfElems)
  /* Comparison */
}