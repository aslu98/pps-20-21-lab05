package u05lab.code

import scala.collection.immutable._
import scala.collection.mutable

object ExamsManager extends App {

  trait Kind {
  }

  object Kind {
    case class Retired() extends Kind
    case class Failed() extends Kind
    case class Succeeded() extends Kind
  }

  trait ExamResult {
    def getKind: Kind
    def getEvaluation: Option[Int]
    def cumLaude: Boolean
  }

  trait ExamResultFactory{
    def failed: ExamResult
    def retired: ExamResult
    def succeededCumLaude: ExamResult
    def succeeded(evaluation: Int): ExamResult
  }

  case class ExamResultFactoryImpl() extends ExamResultFactory{
    private case class ExamResultImpl(kind: Kind, eval:Option[Int], laude: Boolean) extends ExamResult {
      if (eval.isDefined && (eval.get < 18 || eval.get > 30)) throw new IllegalArgumentException()
      override def getKind: Kind = kind
      override def getEvaluation: Option[Int] = eval
      override def cumLaude: Boolean = laude
      override def toString: String = kind.toString + (if (eval.isDefined) " - " + eval.get else "") + (if (laude) "L" else "")
    }

    override def failed: ExamResult = ExamResultImpl(Kind.Failed(), Option.empty, false)
    override def retired: ExamResult = ExamResultImpl(Kind.Retired(), Option.empty, false)
    override def succeededCumLaude: ExamResult = ExamResultImpl(Kind.Succeeded(), Option.apply(30), true)
    override def succeeded(eval: Int): ExamResult = ExamResultImpl(Kind.Succeeded(), Option.apply(eval), false)
  }

  trait ExamsManager {
    def createNewCall(call:String);
    def addStudentResult(call: String, student: String, result: ExamResult);
    def getAllStudentsFromCall(call: String): scala.collection.Set[String]
    def getEvaluationsMapFromCall(call: String): Map[String, Int]
    def getResultsMapFromStudent(student: String): Map[String, String];
    def getBestResultFromStudent(student: String): Option[Int]
  }

  case class ExamsManagerImpl() extends ExamsManager {
    private[this] val mainMap: mutable.Map[String, mutable.Map[String, ExamResult]] = mutable.HashMap()

    override def createNewCall(call: String): Unit =
      if (mainMap.contains(call)) throw new IllegalArgumentException else mainMap += (call -> mutable.HashMap())

    override def addStudentResult(call: String, student: String, result: ExamResult): Unit =
      if (mainMap(call).contains(student)) throw new IllegalArgumentException else mainMap(call) += (student -> result)

    override def getAllStudentsFromCall(call: String): scala.collection.Set[String] =
      mainMap(call).keySet

    override def getEvaluationsMapFromCall(call: String): Map[String, Int] =
     mainMap(call) collect { case (stud:String, res:ExamResult) if res.getEvaluation.isDefined => (stud, res.getEvaluation.get)} toMap

    override def getResultsMapFromStudent(student: String): Map[String, String] = {
      var studMap: Map[String, String] = HashMap()
      for ((call, map) <- mainMap){
        for ((stud, res) <- map){
          if (stud == student) studMap += (call ->  res.toString)
        }
      }
      studMap
    }

    override def getBestResultFromStudent(student: String): Option[Int] = {
      var results:List[Int] = List();
      for ((_, map) <- mainMap){
        for ((stud, res) <- map){
          if (stud == student && res.getEvaluation.isDefined) {
            results = res.getEvaluation.get :: results
          }
        }
      }
      Option.when(results.size > 0)(results.reduce(Math.max))
    }
  }

}