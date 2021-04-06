package u05lab

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._
import u05lab.code._

class ListsTests {

  @Test
  def testZipRight(): Unit ={
    val l = List("a", "b", "c");

    assertEquals(List.nil, List.nil.zipRight)
    assertEquals(List(("a",0), ("b",1), ("c",2)), l.zipRight)
  }

  @Test
  def testPartition(): Unit ={
    val l = List(1, 2, 3, 5, 6);
    assertEquals((l, List.nil), l.partition(_>0))
    assertEquals((List(2, 6), List(1, 3, 5)), l.partition(_%2==0))
  }

  @Test
  def testSpan(): Unit ={
    val l = List(1, 2, 3, 5, 6);
    assertEquals((l, List.nil), l.span(_>0))
    assertEquals((List(1, 2), List(3, 5, 6)), l.span(_<3))
  }

  @Test
  def testReduce(): Unit ={
    val l = List(1, 2, 3, 5, 6);
    assertEquals(17, l.reduce(_+_))
    assertThrows(classOf[UnsupportedOperationException], () => List[Int]().reduce(_+_))
  }

  @Test
  def testTakeRight(): Unit ={
    val l = List(1, 2, 3, 5, 6);
    assertEquals(List(3, 5, 6), l.takeRight(3))
    assertEquals(List.nil, l.takeRight(0))
    assertEquals(l, l.takeRight(50))
  }

  @Test
  def testCollect(): Unit ={
    val l = List(1, 2, 3, 5, 6);
    assertEquals(List(0, 1, 5), l.collect{case x if x < 3 || x > 5 => x-1})
    assertEquals(List.nil, l.collect{case x if x < 1 || x > 10 => x-1})
  }

  @Test
  def testOptSequence(): Unit ={
    import u05lab.code.OptSequence.sequence
    assertEquals(Some(List(1, 2, 3)), sequence[Int](List(Some(1),Some(2),Some(3))))
    assertEquals(None, sequence[Int](List(Some(1),None,Some(3))))
  }
}