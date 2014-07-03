package com.tetraconcepts.utilities

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

/** simple tests for list methods
  *
 * Created by jthalbert on 6/23/14.
 */
@RunWith(classOf[JUnitRunner])
class ListMethodsTest extends FunSuite {
  trait TestList {
    val list = List("a","b","c","d")
  }
  test("orderedSubsequences emits correct sequences") {
    new TestList {
      val expected1seq = List(List("a"), List("b"), List("c"), List("d"))
      val expected2seq = List(List("a", "b"), List("a", "c"), List("a", "d"),
        List("b", "c"), List("b", "d"), List("c", "d"))
      val expected3seq = List(List("a", "b", "c"), List("a", "b", "d"),
        List("a","c","d"), List("b", "c", "d"))
      val expected4seq = List(List("a", "b", "c", "d"))
      //TODO: rewrite to ensure check does not depend on order
      assert(ListMethods.orderedSubsequences(list, 0) sameElements List(List()))
      assert(ListMethods.orderedSubsequences(list, 1) sameElements expected1seq)
      assert(ListMethods.orderedSubsequences(list, 2) sameElements expected2seq)
      assert(ListMethods.orderedSubsequences(list, 3) sameElements expected3seq)
      assert(ListMethods.orderedSubsequences(list, 4) sameElements expected4seq)
    }
  }

  test("orderedSubsequences should throw exception on negative length") {
    new TestList {
      intercept[IllegalArgumentException] {
        ListMethods.orderedSubsequences(list,-1)
      }
      assert(true)
    }
  }
}
