package u05lab.code

import u05lab.code.PerformanceUtils.{calculatePerformance, measure}

import java.util.concurrent.TimeUnit
import scala.collection._
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
    for ((name, seq) <- seqs) measure(name + " last "){seq.last}
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
  private val numberOfUpdates: Int = 10000
  println("CREATE")

  /* Linear sequences: List, ListBuffer */
  measure("IMM list create") {immutable.List.range(1, numberOfElems)}
  measure("MUT bufferList create") {mutable.ListBuffer.range(1, numberOfElems)}

  /* Indexed sequences: Vector, Array, ArrayBuffer */
  measure("IMM vector create") {Vector.range(1, numberOfElems)}
  measure("PARMUT array create") {Array.range(1, numberOfElems)}
  measure("MUT arrayBuffer create") {mutable.ArrayBuffer.range(1, numberOfElems)}

  /* Sets */
  measure("IMM hashset create") {immutable.HashSet.range(1,numberOfElems)}
  measure("MUT hashset create") {mutable.HashSet.range(1,numberOfElems)}
  measure("IMM treeset create") {immutable.TreeSet.from(1 to numberOfElems)}
  measure("MUT treeset create") {mutable.TreeSet.from(1 to numberOfElems)}

  /* Maps */
  measure("IMM hashmap create") {var m = immutable.HashMap[Int, Int](); for (i <- 1 to numberOfElems) m = m.updated(i, i+1)}
  measure("MUT hashmap create") {val m = mutable.HashMap[Int, Int](); for (i <- 1 to numberOfElems) m.update(i, i+1)}
  measure("IMM treemap create") {var immTreeMap = immutable.TreeMap[Int, Int](); for (i <- 1 to numberOfElems) immTreeMap = immTreeMap.updated(i, i+1)}
  measure("MUT treemap create") {val mutTreeMap = mutable.TreeMap[Int, Int](); for (i <- 1 to numberOfElems) mutTreeMap.update(i, i+1)}

  var immList = immutable.List.range(1, numberOfElems)
  val mutList = mutable.ListBuffer.range(1, numberOfElems)
  var immVect = Vector.range(1, numberOfElems)
  val parmutArray = Array.range(1, numberOfElems)
  val mutArrayBuffer = mutable.ArrayBuffer.range(1, numberOfElems)
  var immHashSet = immutable.HashSet.range(1,numberOfElems)
  val mutHashSet = mutable.HashSet.range(1,numberOfElems)
  var immTreeSet = immutable.TreeSet.from(1 to numberOfElems)
  val mutTreeSet = mutable.TreeSet.from(1 to numberOfElems)
  var immHashMap = immutable.HashMap[Int, Int](); for (i <- 1 to numberOfElems) immHashMap = immHashMap.updated(i, i+1)
  val mutHashMap = mutable.HashMap[Int, Int](); for (i <- 1 to numberOfElems) mutHashMap.update(i, i+1)
  var immTreeMap = immutable.TreeMap[Int, Int](); for (i <- 1 to numberOfElems) immTreeMap = immTreeMap.updated(i, i+1)
  val mutTreeMap = mutable.TreeMap[Int, Int](); for (i <- 1 to numberOfElems) mutTreeMap.update(i, i+1)

  /* Comparison */
  private val iterables = Map[String, Iterable[Int]]("IMM list" -> immList, "MUT bufferList" -> mutList,
    "IMM vector" -> immVect, "PARMUT array" -> parmutArray, "MUT arrayBuffer" -> mutArrayBuffer,
    "IMM hashset" -> immHashSet, "MUT hashset" -> mutHashSet, "IMM treeset" -> immTreeSet, "MUT treeset" -> mutTreeSet,
    "IMM hashmap" -> immHashMap.asInstanceOf[Iterable[Int]], "MUT hashmap" -> mutHashMap.asInstanceOf[Iterable[Int]],
    "IMM treemap" -> immTreeMap.asInstanceOf[Iterable[Int]], "MUT treemap" -> mutTreeMap.asInstanceOf[Iterable[Int]])
  calculatePerformance[Int](iterables, numberOfElems)

  println()
  println("UPDATE")
  measure("IMM list update "){for (i <- 0 to numberOfUpdates) {immList = immList.updated(i, i+2)}}
  measure("MUT list update "){for (i <- 0 to numberOfUpdates) {mutList.update(i, i+2)}}
  measure("IMM vector update "){for (i <- 0 to numberOfUpdates) {immVect = immVect.updated(i, i+2)}}
  measure("PARMUT array update "){for (i <- 0 to numberOfUpdates) {parmutArray.update(i, i+2)}}
  measure("MUT arraybuffer update "){for (i <- 0 to numberOfUpdates) {mutArrayBuffer.update(i, i+2)}}
  measure("IMM hashset update "){for (i <- 0 to numberOfUpdates) {immHashSet = immHashSet + (i + numberOfElems)}}
  measure("MUT hashset update "){for (i <- 0 to numberOfUpdates) {mutHashSet.update(i, true)}}
  measure("IMM treeset update "){for (i <- 0 to numberOfUpdates) {immTreeSet = immTreeSet + (i + numberOfElems)}}
  measure("MUT treeset update "){for (i <- 0 to numberOfUpdates) {mutTreeSet.update(i, true)}}
  measure("IMM hashmap update "){for (i <- 0 to numberOfUpdates) {immHashMap = immHashMap.updated(i, i+2)}}
  measure("MUT hasmap update "){for (i <- 0 to numberOfUpdates) {mutHashMap.update(i, i+2)}}
  measure("IMM treemap update "){for (i <- 0 to numberOfUpdates) {immTreeMap = immTreeMap.updated(i, i+2)}}
  measure("MUT treemap update "){for (i <- 0 to numberOfUpdates) {mutTreeMap.update(i, i+2)}}
}