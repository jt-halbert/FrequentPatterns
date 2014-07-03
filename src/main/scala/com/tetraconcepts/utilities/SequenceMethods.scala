package com.tetraconcepts.utilities

import com.tetraconcepts.adt.FPTree.{Count, ItemSet}

import scala.annotation.tailrec

/**
 * This is created as a learning exercise on reading the paper cited below.
 *
 * Created by jthalbert on 6/29/14.
 */
object SequenceMethods {

  type SequencePattern = (Sequence, Count)
  type Sequence = List[ItemSet]
  type SequenceDB = List[(SequenceID, Sequence)]
  type SequenceID = Int

  /** This is an implementation of the notion of Sequence from
    *
    * Pei, Jian, et al. "Mining sequential patterns by pattern-growth: The prefixspan approach."
    * Knowledge and Data Engineering, IEEE Transactions on 16.11 (2004): 1424-1440.
    *
    * It includes the notions of subSequence, prefix, suffix defined there.  See tests for examples.
    *
    * Created by jthalbert on 6/29/14.
    *
    * Note: This is using the implicit class stuff from scala 2.10.
    *       I learned about it from http://alvinalexander.com/scala/scala-2.10-implicit-class-example
    *
    *
    */
  implicit class ItemSetSequence(val target: List[ItemSet]) {

    /**
     * The following methods will be available to any List[ItemSet] if this is imported
     */
    def subsequenceOf(seq: List[ItemSet]): Boolean = {
      //TODO: investigate performance of this: should it use ordered nature of ItemSet?
      def containedIn(a: ItemSet, b: ItemSet): Boolean = a.toSet.subsetOf(b.toSet)
      @tailrec
      def recur(alpha: List[ItemSet], beta: List[ItemSet]): Boolean = (alpha, beta) match {
        case (Nil, bs) => true
        case (as, Nil) => false
        case (a :: as, bs) => {
          val bjs = bs.dropWhile(!containedIn(a,_))
          if (bjs.isEmpty) false
          else {
            recur(as, bjs.tail)
          }
        }
      }
      recur(target,seq)
    }

    //every itemset is expected to be ordered.  This gives meaning to prefix/suffix
    def isSorted: Boolean = target.forall(is => is.sorted.sameElements(is))

    def prefixOf(seq: List[ItemSet]): Boolean = (target, seq) match {
      case (Nil, ss) => true
      case (ts, Nil) => false
      case (ts, ss) => {
        val m = ts.length
        ts.init.sameElements(ss.take(m-1)) && ss(m-1).startsWith(ts.last)
      }
    }

    def seqLength: Int = target.flatten.length

    def suffixGiven(prefix: List[ItemSet]): List[ItemSet] = {
      require(prefix.prefixOf(target), "Can only find a suffix from a prefix")
      (prefix, target) match {
        case (Nil, ts) => ts
        case (ps, ts) => {
          val m = ps.length
          val tail = ts.drop(m)
          val head = {
            val n = ps.last.length
            ts(m-1).drop(n)
          }

          /** note that head might be empty.  This is by design.
            *
            * In the paper Pei, et. al. use a marker _ to signify that the
            * last element in the prefix together with the first in the suffix
            * join up into a single element.
            *
            * Here I choose not to do that.  I will leave an empty list in the first
            * element of the suffix and assume joining suffixes to prefixes just merges
            * the last element of the prefix with the first of the suffix.  The empty
            * list at the head of the suffix will ensure the joining is correct.  And,
            * I don't have to carry around some special signifier.
            */

          head :: tail
        }
      }
    }


    /**
     * This is for the actual usage of the prefix notion in the algorithm.
     * We find the first occurrence of a frequent element and get the suffix after it.
     * @param prefix usually will be a single element
     * @return
     */
    //TODO: Make this method reflect more clearly the fact that the prefix is single-element
    def suffixAfter(prefix: List[ItemSet]) : List[ItemSet] = {
      require(prefix.subsequenceOf(target), "require prefix to be contained")
      def containedIn(a: ItemSet, b: ItemSet): Boolean = a.toSet.subsetOf(b.toSet)
      (prefix, target) match {
        case (Nil, ts) => ts
        case (ps, ts) => {
          val m = ts.indexWhere(s => containedIn(ps.head,s))+1
          val tail = ts.drop(m)
          val head = {
            val n = ts(m-1).indexWhere(i => i == ps.head(0)) + 1
            ts(m-1).drop(n)
          }
          if (tail.nonEmpty) head :: tail
          else {
            throw new Exception("prefix in last position")
          }
        }
      }
    }

    /**
     * This function eliminates infrequent items from a sequence
     * @param freq1Patterns a list of patterns that occur above minsupp times (as in output
     *                      of FrequentSequences.findLength1FrequentPatterns
     * @return
     */
    def frequentProjection(freq1Patterns: List[SequencePattern]): Sequence = {
      val freqElements = freq1Patterns.map(_._1(0)(0))
      // the following val might have empty lists in the middle, have to clean them
      //TODO: do this in one pass instead of two
      val dirtyProj = target.map(s => s.filter(e => freqElements.contains(e)))
      dirtyProj.head :: dirtyProj.tail.filter(_.nonEmpty) //the head might be empty and that is ok
    }


    //TODO: figure out how to do this
    /**override def toString: String = {
      "<"+ target.map("("+_.mkString(",") + ")").mkString(";")+">"
    }
      */

  }

}

