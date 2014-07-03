package com.tetraconcepts.transformers

import com.tetraconcepts.adt.FPTree.{Count, ItemSet}
import com.tetraconcepts.utilities.SequenceMethods._

/** This is an implementation for personal learning drawn almost exclusively from
  *
  * Pei, Jian, et al. "Mining sequential patterns by pattern-growth: The prefixspan approach."
  * Knowledge and Data Engineering, IEEE Transactions on 16.11 (2004): 1424-1440.
  *
  * Created by jthalbert on 6/29/14.
  */
object FrequentSequences {


  /**
   * Take in a SequenceDB and recursively build the list of
   * frequent sequences.
   */

  def apply(sequences: SequenceDB,
             supportThreshold: Int = 0): List[SequencePattern] = {
    val fs = new FrequentSequences
    fs.prefixScan((List(),Int.MaxValue), 0, sequences)
  }

}

class FrequentSequences {

  def prefixScan(alpha: SequencePattern, l: Int, pS: SequenceDB): List[SequencePattern] = {

    ???
  }

  /**
   * Find the single item histogram.
   * @param sequences a sequence DB with no IDs
   * @param minSupp the user specified minimum support
   * @return List[(frequent 1-seq, support of it, true if it should be joined in itemset with prefix
   *         The underscore from the paper.  True = underscore mark
   */
  def findLength1FrequentPatterns(sequences: List[List[ItemSet]], minSupp: Int): List[(Sequence, Count, Boolean)] = {
    sequences.flatMap(_.flatten.groupBy(w=>w).mapValues(_ => 1)) // gather like and output 1
      .groupBy(t=>t._1) // gather like ones
      .mapValues(l => l.map(_._2).size) // count them up
      .toList.filter(_._2 > minSupp)  //only take frequent ones
      .map(t => (List(List(t._1)), t._2)) //form into a SequencePattern
  }

  /**
   * Note that I am emitting a Sequence DB that does not carry IDs.  This is a choice.  I could keep them
   * around but I would have to make some decision about how to accumulate them.  It does not make sense
   * to carry them around.
   *
   * @param prefixPattern expected to be a length-1 frequent pattern
   * @param sequences a DB stripped of IDs (compromise to be used in the recursion)
   * @param freq1pats the frequent 1-patterns, these are required to project out the infrequent elements
   * @return
   */
  def projectedDB(prefixPattern: SequencePattern, sequences: List[List[ItemSet]], freq1pats: List[SequencePattern]): List[List[ItemSet]] = {
    def optionSuffixAfter(prefix: Sequence, sequence: Sequence): Option[Sequence] = {
      try {
        Some(sequence.suffixAfter(prefix))
      } catch {
        case e: Exception => None
      }
    }
    sequences.filter(prefixPattern._1.subsequenceOf) //only sequences containing should be considered
      .flatMap(s => optionSuffixAfter(prefixPattern._1, s.frequentProjection(freq1pats)))

  }

  def joinPatterns(prefix: SequencePattern, suffix: SequencePattern): SequencePattern = {
    ???
  }
}
