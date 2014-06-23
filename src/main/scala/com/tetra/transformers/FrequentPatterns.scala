package com.tetra.transformers

import com.tetra.adt.FPTree
import com.tetra.adt.FPTree.{Pattern, Transaction, TransactionDB, ItemSet}

/**
 * This is the overarching application that transforms transaction DBs
 * into a collection of frequent patterns.  It is backed by an implementation
 * of the so-called FPTree.
 *
 * Created by jthalbert on 6/16/14.
 */

object FrequentPatterns {

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

