package u05lab

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test
import u05lab.code.ExamsManager._

class ExamsManagerTests {

  val erf: ExamsResultFactoryImpl = ExamsResultFactoryImpl()
  val em: ExamsManager = ExamsManagerImpl();

  @Test
  def testExamResultsBasicBehaviour() {
    // esame fallito, non c'è voto
    assertEquals(erf.failed.getKind, Kind.Failed())
    assertFalse(erf.failed.getEvaluation.isDefined)
    assertFalse(erf.failed.cumLaude)
    assertEquals("Failed()", erf.failed.toString())

    // lo studente si è ritirato, non c'è voto
    assertEquals(erf.retired.getKind, Kind.Retired())
    assertFalse(erf.retired.getEvaluation.isDefined)
    assertFalse(erf.retired.cumLaude)
    assertEquals("Retired()", erf.retired.toString())

    // 30L
    assertEquals(Kind.Succeeded(), erf.succeededCumLaude.getKind)
    assertEquals(Option.apply(30), erf.succeededCumLaude.getEvaluation)
    assertTrue(erf.succeededCumLaude.cumLaude)
    assertEquals("Succeeded() - 30L", erf.succeededCumLaude.toString)

    // esame superato, ma non con lode
    assertEquals(Kind.Succeeded(), erf.succeeded(28).getKind)
    assertEquals(Option.apply(28), erf.succeeded(28).getEvaluation)
    assertFalse(erf.succeeded(28).cumLaude)
    assertEquals("Succeeded() - 28", erf.succeeded(28).toString)
  }

  @Test def optionalTestEvaluationCantBeGreaterThan30(): Unit = {
    assertThrows(classOf[IllegalArgumentException], () => erf.succeeded(32))
  }

  @Test def optionalTestEvaluationCantBeSmallerThan17(): Unit = {
    assertThrows(classOf[IllegalArgumentException], () => erf.succeeded(17))
  }

  def prepareExams {
    em.createNewCall("gennaio");
    em.createNewCall("febbraio");
    em.createNewCall("marzo");

    em.addStudentResult("gennaio", "rossi", erf.failed) // rossi -> fallito
    em.addStudentResult("gennaio", "bianchi", erf.retired) // bianchi -> ritirato
    em.addStudentResult("gennaio", "verdi", erf.succeeded(28)) // verdi -> 28
    em.addStudentResult("gennaio", "neri", erf.succeededCumLaude) // neri -> 30L

    em.addStudentResult("febbraio", "rossi", erf.failed) // etc..
    em.addStudentResult("febbraio", "bianchi", erf.succeeded(20))
    em.addStudentResult("febbraio", "verdi", erf.succeeded(30))

    em.addStudentResult("marzo", "rossi", erf.succeeded(25))
    em.addStudentResult("marzo", "bianchi", erf.succeeded(25))
    em.addStudentResult("marzo", "viola", erf.failed)
  }

  @Test
  def testExamsManagement() {
    this.prepareExams

    // partecipanti agli appelli di gennaio e marzo
    assertEquals(Set("rossi","bianchi","verdi","neri"), em.getAllStudentsFromCall("gennaio"))
    assertEquals(Set("rossi","bianchi","viola"), em.getAllStudentsFromCall("marzo"))

    // promossi di gennaio con voto
    assertEquals(2, em.getEvaluationsMapFromCall("gennaio").size)
    assertEquals(28, em.getEvaluationsMapFromCall("gennaio").get("verdi").get)
    assertEquals(30, em.getEvaluationsMapFromCall("gennaio").get("neri").get)

    // promossi di febbraio con voto
    assertEquals(2, em.getEvaluationsMapFromCall("febbraio").size)
    assertEquals(20, em.getEvaluationsMapFromCall("febbraio").get("bianchi").get)
    assertEquals(30, em.getEvaluationsMapFromCall("febbraio").get("verdi").get)

    // tutti i risultati di rossi (attenzione ai toString!!)
    assertEquals(3, em.getResultsMapFromStudent("rossi").size)
    assertEquals("Failed()", em.getResultsMapFromStudent("rossi").get("gennaio").get)
    assertEquals("Failed()", em.getResultsMapFromStudent("rossi").get("febbraio").get)
    assertEquals("Succeeded() - 25", em.getResultsMapFromStudent("rossi").get("marzo").get)

    // tutti i risultati di bianchi
    assertEquals(3, em.getResultsMapFromStudent("bianchi").size)
    assertEquals("Retired()", em.getResultsMapFromStudent("bianchi").get("gennaio").get)
    assertEquals("Succeeded() - 20",em.getResultsMapFromStudent("bianchi").get("febbraio").get)
    assertEquals("Succeeded() - 25",em.getResultsMapFromStudent("bianchi").get("marzo").get)

    // tutti i risultati di neri
    assertEquals(1, em.getResultsMapFromStudent("neri").size)
    assertEquals("Succeeded() - 30L",em.getResultsMapFromStudent("neri").get("gennaio").get)

  }

  @Test def optionalTestExamsManagement() {
    this.prepareExams;
    assertEquals(Option.apply(25), em.getBestResultFromStudent("rossi"))
    assertEquals(Option.apply(25), em.getBestResultFromStudent("bianchi"))
    assertEquals(Option.apply(30), em.getBestResultFromStudent("neri"))
    assertEquals(Option.empty, em.getBestResultFromStudent("viola"))
  }

  @Test def optionalTestCantCreateACallTwice() {
    this.prepareExams;
    assertThrows(classOf[IllegalArgumentException], () => em.createNewCall("marzo"))
  }

  @Test def optionalTestCantRegisterAnEvaluationTwice() {
    this.prepareExams
    assertThrows(classOf[IllegalArgumentException], () => em.addStudentResult("gennaio", "verdi", erf.failed))
  }
}