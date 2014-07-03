package com.tetraconcepts.transformers

import com.tetraconcepts.utilities.SequenceMethods.SequenceDB
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/** Simple tests for Frequent Sequences
  *
 * Created by jthalbert on 7/2/14.
 */
@RunWith(classOf[JUnitRunner])
class FrequentSequencesTest extends FunSuite {
  trait TestDB {
    val sdb: SequenceDB =List(
      (10, List(List("a"),List("a","b","c"),List("a","c"),List("d"),List("c","f"))),
      (20, List(List("a","d"), List("c"), List("b","c"), List("a","e"))),
      (30, List(List("e","f"), List("a","b"), List("d","f"),List("c"),List("b"))),
      (40, List(List("e"), List("g"), List("a","f"), List("c"), List("b"), List("c"))))

  }
  test("projectedDB works as expected") {
    new TestDB {
      val fs = new FrequentSequences
      val fp1 = fs.findLength1FrequentPatterns(sdb.map(_._2),2)
      assert(fs.projectedDB((List(List("a")),4), sdb.map(_._2),fp1) sameElements
        List(
          List(List(), List("a", "b", "c"), List("a", "c"), List("d"), List("c", "f")),
          List(List("d"), List("c"), List("b", "c"), List("a", "e")),
          List(List("b"), List("d", "f"), List("c"), List("b")),
          List(List("f"), List("c"), List("b"), List("c"))))
      assert(fs.projectedDB((List(List("b")),4), sdb.map(_._2),fp1) sameElements
        List(
          List(List("c"), List("a", "c"), List("d"), List("c", "f")),
          List(List("c"), List("a", "e")),
          List(List(), List("d", "f"), List("c"), List("b")),
          List(List(), List("c"))))
      assert(fs.projectedDB((List(List("c")),4), sdb.map(_._2),fp1) sameElements
        List(
          List(List(), List("a", "c"), List("d"), List("c", "f")),
          List(List(), List("b", "c"), List("a", "e")), List(List(), List("b")),
          List(List(), List("b"), List("c"))))
      assert(fs.projectedDB((List(List("d")),3), sdb.map(_._2),fp1) sameElements
        List(List(List(), List("c", "f")),
          List(List(), List("c"), List("b", "c"), List("a", "e")),
          List(List("f"), List("c"), List("b"))))
      assert(fs.projectedDB((List(List("e")),2), sdb.map(_._2),fp1) sameElements
        List(
          List(List("f"), List("a", "b"), List("d", "f"), List("c"), List("b")),
          List(List(), List("a", "f"), List("c"), List("b"), List("c"))))
      assert(fs.projectedDB((List(List("f")),2), sdb.map(_._2),fp1) sameElements
        List(List(List(), List("a", "b"), List("d", "f"), List("c"), List("b")),
          List(List(), List("c"), List("b"), List("c"))))
    }
  }
}
