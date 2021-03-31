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
  private var seqs: mutable.HashMap[String, Iterable[Int]] = mutable.HashMap()
  println("CREATE")

  /* Linear sequences: List, ListBuffer */

  measure("list create") {scala.collection.immutable.List.range(1, numberOfElems)}
  seqs += ("list" -> scala.collection.immutable.List.range(1, numberOfElems))
  measure("bufferList create") {mutable.ListBuffer.range(1, numberOfElems)}
  seqs += ("bufferList" -> mutable.ListBuffer.range(1, numberOfElems))

  /* Indexed sequences: Vector, Array, ArrayBuffer */
  measure("vector create") {Vector.range(1, numberOfElems)}
  seqs += ("vector" -> Vector.range(1, numberOfElems))
  measure("array create") {Array.range(1, numberOfElems)}
  seqs += ("array" -> Array.range(1, numberOfElems))
  measure("arrayBuffer create") {mutable.ArrayBuffer.range(1, numberOfElems)}
  seqs += ("arrayBuffer" -> mutable.ArrayBuffer.range(1, numberOfElems))

  /* Sets */
  measure("hashset create") {immutable.HashSet.range(1,numberOfElems)}
  seqs += ("hashset" -> (immutable.HashSet.range(1,numberOfElems)))
  measure("treeset create") {mutable.TreeSet.from(1 to numberOfElems)}
  seqs += ("treeset" -> (mutable.TreeSet.from(1 to numberOfElems)))

  /* Maps */
  measure("hashmap create") {immutable.HashMap((1,numberOfElems) -> (numberOfElems, numberOfElems*2))}
  seqs += ("hashmap" -> immutable.HashMap((1,numberOfElems) -> (numberOfElems, numberOfElems*2)).asInstanceOf[Iterable[Int]])
  measure("treemap create") {mutable.TreeMap((1,numberOfElems) -> (1,numberOfElems))}
  seqs += ("treemap" -> mutable.TreeMap((1,numberOfElems) -> (1,numberOfElems)).asInstanceOf[Iterable[Int]])

  import PerformanceUtils._
  calculatePerformance[Int](seqs, numberOfElems)
  /* Comparison */
}