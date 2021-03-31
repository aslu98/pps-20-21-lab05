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
    println("DELETE")
    for ((name, seq) <- seqs) measure(name + " delete"){ seq.dropRight(numberOfElems) }
  }
}

object CollectionsTest extends App {

  private val numberOfElems: Int = 1000000
  private var iterables: mutable.LinkedHashMap[String, Iterable[Int]] = mutable.LinkedHashMap()
  println("CREATE")

  /* Linear sequences: List, ListBuffer */

  val l = mutable.ListBuffer.range(1, numberOfElems)
  l.update(3, 3)
  measure("IMM list create") {immutable.List.range(1, numberOfElems)}
  iterables += ("IMM list" -> immutable.List.range(1, numberOfElems))
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
  measure("MUT hashset create") {mutable.HashSet.range(1,numberOfElems)}
  iterables += ("MUT hashset" -> (mutable.HashSet.range(1,numberOfElems)))
  measure("IMM treeset create") {immutable.TreeSet.from(1 to numberOfElems)}
  iterables += ("IMM treeset" -> (immutable.TreeSet.from(1 to numberOfElems)))
  measure("MUT treeset create") {mutable.TreeSet.from(1 to numberOfElems)}
  iterables += ("MUT treeset" -> (mutable.TreeSet.from(1 to numberOfElems)))

  /* Maps */
  var immHashMap = immutable.HashMap[Int, Int](); for (i <- 1 to numberOfElems) immHashMap = immHashMap.updated(i, i+1)
  measure("IMM hashmap create") {var m = immutable.HashMap[Int, Int](); for (i <- 1 to numberOfElems) m = m.updated(i, i+1)}
  iterables += ("IMM hashmap" -> immHashMap.asInstanceOf[Iterable[Int]] )
  var mutHashMap = mutable.HashMap[Int, Int](); for (i <- 1 to numberOfElems) mutHashMap.update(i, i+1)
  measure("MUT hashmap create") {val m = mutable.HashMap[Int, Int](); for (i <- 1 to numberOfElems) m.update(i, i+1)}
  iterables += ("MUT hashmap" -> mutHashMap.asInstanceOf[Iterable[Int]])
  var immTreeMap = immutable.TreeMap[Int, Int](); for (i <- 1 to numberOfElems) immTreeMap = immTreeMap.updated(i, i+1)
  measure("IMM treemap create") {var immTreeMap = immutable.TreeMap[Int, Int](); for (i <- 1 to numberOfElems) immTreeMap = immTreeMap.updated(i, i+1)}
  iterables += ("IMM treemap" -> immTreeMap.asInstanceOf[Iterable[Int]]  )
  var mutTreeMap = mutable.TreeMap[Int, Int](); for (i <- 1 to numberOfElems) mutTreeMap.update(i, i+1)
  measure("MUT treemap create") {var mutTreeMap = mutable.TreeMap[Int, Int](); for (i <- 1 to numberOfElems) mutTreeMap.update(i, i+1)}
  iterables += ("MUT treemap" -> mutTreeMap.asInstanceOf[Iterable[Int]]  )

  import PerformanceUtils._
  calculatePerformance[Int](iterables, numberOfElems)
  /* Comparison */
}