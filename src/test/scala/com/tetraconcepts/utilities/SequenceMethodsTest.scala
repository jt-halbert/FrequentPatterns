package com.tetraconcepts.utilities

import com.tetraconcepts.adt.FPTree.ItemSet
import com.tetraconcepts.transformers.FrequentSequences
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import com.tetraconcepts.utilities.SequenceMethods._

/** Simple tests for sequence methods
  *
 * Created by jthalbert on 6/29/14.
 */
@RunWith(classOf[JUnitRunner])
class SequenceMethodsTest extends FunSuite {
  trait TestSequence {

    val sdb: SequenceDB =List(
      (10, List(List("a"),List("a","b","c"),List("a","c"),List("d"),List("c","f"))),
      (20, List(List("a","d"), List("c"), List("b","c"), List("a","e"))),
      (30, List(List("e","f"), List("a","b"), List("d","f"),List("c"),List("b"))),
      (40, List(List("e"), List("g"), List("a","f"), List("c"), List("b"), List("c"))))

    val alpha: List[ItemSet] = List(List("a"),List("b","c"),List("d"),List("f"))

    val beta: List[ItemSet] = List(List("a"),List("a","b","c"),List("a","c"),List("d"),List("c","f"))
  }

  test("subsequenceOf works as expected") {
    new TestSequence {
      assert(alpha.subsequenceOf(beta))
    }
  }

  test("prefixOf works as expected") {
    new TestSequence {
      //yes
      val a = List(List("a"))
      val aa = List(List("a"),List("a"))
      val aab = List(List("a"), List("a","b"))
      val aabc = List(List("a"), List("a","b","c"))
      assert(a.prefixOf(sdb(0)._2))
      assert(aa.prefixOf(sdb(0)._2))
      assert(aab.prefixOf(sdb(0)._2))
      assert(aabc.prefixOf(sdb(0)._2))
      //no
      val ab = List(List("a"),List("b"))
      val abc = List(List("a"), List("b","c"))
      assert(!ab.prefixOf(sdb(0)._2))
      assert(!abc.prefixOf(sdb(0)._2))

    }
  }

  test("suffixGiven works as expected") {
    new TestSequence {
      val s = sdb(0)._2
      val a = List(List("a"))
      val aa = List(List("a"), List("a"))
      val aab = List(List("a"), List("a","b"))
      assert(s.suffixGiven(a) sameElements List(List(),List("a","b","c"), List("a","c"),List("d"), List("c","f")))
      assert(s.suffixGiven(aa) sameElements List(List("b","c"),List("a","c"),List("d"),List("c","f")))
      assert(s.suffixGiven(aab) sameElements List(List("c"), List("a","c"), List("d"), List("c","f")))
    }
  }

  test("suffixAfter works as expected") {
    new TestSequence {
      val s1 = sdb(0)._2
      val s3 = sdb(2)._2
      val a = List(List("a"))
      val b = List(List("b"))
      assert(s1.suffixAfter(b) sameElements List(List("c"), List("a","c"), List("d"), List("c","f")))
      assert(s3.suffixAfter(a) sameElements List(List("b"), List("d","f"), List("c"), List("b")))
    }
  }
}
