package com.tetraconcepts.transformers


import com.tetraconcepts.adt.FPTree.{Pattern, TransactionDB}
import com.tetraconcepts.utilities.ListMethods
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
 * Tests for the Frequent pattern mining
 *
 * Created by jthalbert on 6/21/14.
 */
@RunWith(classOf[JUnitRunner])
class FrequentPatternsSuite extends FunSuite {

  trait TestTransactions {
    val l1 = List(
      List("f", "a", "c", "d", "g", "i", "m", "p"),
      List("a", "b", "c", "f", "l", "m", "o"),
      List("b", "f", "h", "j", "o"),
      List("b", "c", "k", "s", "p"),
      List("a", "f", "c", "e", "l", "p", "m", "n"))

    val supportThresh = 2

    val allPatterns = FrequentPatterns(l1)
    val freqPatterns = FrequentPatterns(l1, supportThresh)

    def bruteForceFrequentPatterns(transactions: TransactionDB, supportThreshold: Int = 0): List[Pattern] = {
      val sorted = transactions.flatten
        .groupBy(w => w)
        .mapValues(_.size).toList
        .sortBy(_._2).map(_._1).reverse
      transactions.map(p => {
        (1 to p.length)
          .map(ListMethods.orderedSubsequences(p.sortBy(sorted.indexOf(_)), _)).toList.flatten
      }).flatten
        .groupBy(w => w)
        .mapValues(_.size).toList
    }

    val bruteForceAllPatterns = bruteForceFrequentPatterns(l1)
    val bruteForceFreqPatterns = bruteForceFrequentPatterns(l1,2)
  }


  test("FPGrowth finds same answer as brute force") {
    new TestTransactions {
      assert(bruteForceAllPatterns.filter(!allPatterns.contains(_)).isEmpty)
      assert(allPatterns.filter(!bruteForceAllPatterns.contains(_)).isEmpty)
      assert(bruteForceAllPatterns.filter(!allPatterns.contains(_)).isEmpty)
      assert(allPatterns.filter(!bruteForceAllPatterns.contains(_)).isEmpty)
    }
  }


}
