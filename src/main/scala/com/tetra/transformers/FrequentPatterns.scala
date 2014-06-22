package com.tetra.transformers

import com.tetra.adt.FPTree
import com.tetra.adt.FPTree.{Transaction, TransactionDB, ItemSet}

/**
 * This is the overarching application that transforms transaction DBs
 * into a collection of frequent patterns.  It is backed by an implementation
 * of the so-called FPTree.
 *
 * Created by jthalbert on 6/16/14.
 */

object FrequentPatterns {

  // a Pattern is an item set together with how often it occurs in the DB
  type Pattern = (ItemSet, Int)

  /**
   * Take in a list of Transactions and create an FPTree
   */
  def apply(transactions: TransactionDB,
             supportThreshold: Int = 0,
             select: (Transaction => ItemSet) = x => x): List[Pattern] = {
    val fp = FPTree(transactions, supportThreshold, select)
    fp.FPGrowth
  }

}

