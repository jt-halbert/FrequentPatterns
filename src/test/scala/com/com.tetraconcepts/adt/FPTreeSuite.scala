package com.tetraconcepts.adt

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
 * Tests for the FPTree implementation
 *
 * Created by jthalbert on 6/15/14.
 */
@RunWith(classOf[JUnitRunner])
class FPTreeSuite extends FunSuite {

  trait TestTransactions {
    val l1 = List(
      List("f", "a", "c", "d", "g", "i", "m", "p"),
      List("a", "b", "c", "f", "l", "m", "o"),
      List("b", "f", "h", "j", "o"),
      List("b", "c", "k", "s", "p"),
      List("a", "f", "c", "e", "l", "p", "m", "n"))

    val fp1 = FPTree(l1)
    val fp2 = FPTree(l1,2)

  }

  test("FPTree for simple transaction database") {
    new TestTransactions {
      def printTree(fpt: FPTree): String = {
        def recur(n: Node, depth: Int): String = {
          "\n" + "\t" * depth + n + n.children.map(recur(_, depth + 1)).mkString("\n")
        }
        recur(fpt.root, 0)
      }
      println(printTree(fp1))
    }
  }

  test("FPTree.contains works") {
    new TestTransactions {
      assert(fp2.contains(List("c","f","p","m","a")))
      assert(!fp2.contains(List("c","f","p","m","b")))
      assert(!fp2.contains(List("c","f","p","x","a")))
      assert(!fp2.contains(List("c","f","y","x","a")))
    }
  }

  test("The complete set of frequent item projections can be derived") {
    new TestTransactions {
      val frequentItemProjections = l1.map(fp1.frequentItemProjection)
      assert(frequentItemProjections.forall(fp1.contains))
    }
  }

}
