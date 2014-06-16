package adt

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import adt._

/**
 * Tests for the FPTree implementation
 *
 * Created by jthalbert on 6/15/14.
 */
@RunWith(classOf[JUnitRunner])
class FPTreeSuite extends FunSuite {

  trait TestTransactions {
    val l1 = List(List("f", "a", "c", "d", "g", "i", "m", "p"),
      List("a", "b", "c", "f", "l", "m", "o"),
      List("b", "f", "h", "j", "o"),
      List("b", "c", "k", "s", "p"),
      List("a", "f", "c", "e", "l", "p", "m", "n"))

  }

  test("") {
    new TestTransactions {
      val fp = FPTree(l1)
      def printTree(fpt: FPTree): String = {
        def recur(n: Node, depth: Int): String = {
          "\n" + "\t" * depth + n + n.children.map(recur(_, depth + 1)).mkString("\n")
        }
        recur(fpt.root, 0)
      }

      println(printTree(fp))
    }
  }

}
