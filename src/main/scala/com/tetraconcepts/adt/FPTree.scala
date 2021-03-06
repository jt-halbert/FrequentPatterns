package com.tetraconcepts.adt

import com.tetraconcepts.adt.FPTree._
import com.tetraconcepts.utilities.ListMethods

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer


/**
 * Created by jthalbert on 6/14/14.
 *
 * This is an implementation of the FPTree as described in
 *
 * Han, Jiawei, et al. "Mining frequent patterns without candidate generation:
 * A frequent-pattern tree approach." Data mining and
 * knowledge discovery 8.1 (2004): 53-87.
 *
 */


object FPTree {

  /**
   * Data:
   *
   *  For this method we have the following fundamental data types.
   *
   *  An "TransactionDB" contains "Transactions" that can be transformed
   *  into collections of "Item"s.  These "ItemSets" occur "Count" number of
   *  times in the TransactionDB.
   *
   *  A so-called "Pattern" is a tuple of an ItemSet and it's Count.
   *
   */
  type Item = String
  type Count = Int
  type ItemSet = List[Item]
  type Pattern = (ItemSet, Count)
  type Transaction = List[String]
  //note the distinction between this and ItemSet
  type TransactionDB = List[Transaction]

  type ItemTable = mutable.Map[Item, Node]

  def apply(transactions: TransactionDB,
            supportThreshold: Int = 0,
            select: (Transaction => ItemSet) = x => x): FPTree = {
    //TODO: Consider putting in the select function above to pull ItemSets out of a transaction
    // transit the DB once to collect the items and their frequency
    val freqList = transactions.flatten.groupBy(w => w).mapValues(_.size).
      toList.sortBy(_._2).reverse
    val emptyRoot = FPNode("", null, ListBuffer())

    // only proceed with items that exceed the supportThreshold
    val frequentItems = freqList.takeWhile(_._2 > supportThreshold).map(_._1)
    val start = new FPTree(emptyRoot, supportThreshold, frequentItems)
    transactions.foldLeft(start)(_ insert _)
  }
}

abstract class Node {

  def itemName: String

  def children: ListBuffer[Node]

  def parent: Node

  var nodeLink: Node = null

  var count = 0

  def insert(freqItems: List[Item], frequentItemHeader: ItemTable): Node = {

    /**
     * If root has a child n such that n.itemName == items.head, then increment n's count by 1;
     * else create a new child node with count initialized to 1, its nodeLink linked to the
     * nodes with the same itemName.  If items.tail is non-empty then
     * call insertTree(items.tail,n)
     *
     * @param itemSet these are the items pulled from a transaction; in a more general case
     *              they could have data accessed by item.itemName //TODO: implement that.
     * @param r a representation of a tree
     * @return
     */
    //TODO: refactor pull-up this and insertPatternBase
    def insertTree(itemSet: ItemSet, r: Node): Unit = {
      if (itemSet.nonEmpty) {
        val i = r.children.indexWhere(_.itemName == itemSet.head)
        if (i > -1) {
          r.children(i).count += 1
          insertTree(itemSet.tail, r.children(i))
        } else {
          var newNode = FPNode(itemSet.head, r, ListBuffer())
          newNode.count += 1
          r.children += newNode
          if (frequentItemHeader.contains(itemSet.head)) {
            //add new nodeLink in the chain
            frequentItemHeader(itemSet.head).lastNodeLinked.nodeLink = newNode
          } else {
            frequentItemHeader(itemSet.head) = newNode
          }
          insertTree(itemSet.tail, newNode)
        }
      }
    }
    insertTree(freqItems, this)
    this
  }

  def lastNodeLinked: Node = {
    if (nodeLink == null) this else nodeLink.lastNodeLinked
  }

  def nodeLinkList: List[Node] = {
    if (nodeLink == null) List(this) else this :: nodeLink.nodeLinkList
  }

}

case class FPNode(itemName: String, parent: Node, children: ListBuffer[Node]) extends Node {
  override def toString: String = itemName + ":" + this.count
}

/**
 * A Frequent Pattern Tree is a tree structure defined as follows.
 *
 * 1) It consists of a root node, a set of item-prefix subtrees as the
 * children of root, and a frequentItemHeader table.
 *
 * 2) Each node in the item-prefix subtree consists of three fields:
 * itemName, count, and nodeLink where itemName registers which item this
 * node represents, count registers the number of transactions represented
 * by the portion of the path reaching this node, and nodeLink links to
 * the next node in the FPTree carrying the same itemName, or null if
 * there is none. For this implementation I have decided to use mutable
 * lists and pop the pointer to the tree node into this ListBuffer in the
 * frequentItemHeader.
 *
 * 3) Each entry in the frequentItemHeader table consists of two fields:
 * (a) itemName and
 * (b) ListBuffer of Nodes that all have the itemName
 *
 * Note: we put the val objects in to the call parameters that we will
 * have when we construct it.
 */
case class FPTree(root: Node, supportThreshold: Int, frequentItems: List[Item]) {


  var frequentItemHeader = mutable.Map.empty[Item, Node]

  def preOrderWalk(visit: Node => Any) {
    def recur(n: Node) {
      visit(n)
      n.children.foreach(recur)
    }
    recur(root)
  }

  def toList: List[(String, Int)] = {
    def recur(n: Node): List[(String, Int)] = {
      if (n.itemName.nonEmpty) {
        (n.itemName, n.count) :: n.children.map(recur).toList.flatten
      } else {
        n.children.map(recur).toList.flatten
      }
    }
    recur(root)
  }


  def postOrderWalk(visit: Node => Any) {
    def recur(n: Node) {
      n.children.foreach(recur)
      visit(n)
    }
    recur(root)
  }


  /**
   * For each transaction we select the frequent items in it and sort them
   * according to the global order induced by orderedItems.  We then recursively
   * call insertTree to turn the frequent items into a subtree.
   *
   * @param transaction A generic transaction
   * @return
   */
  def insert(transaction: Transaction): FPTree = {
    root.insert(frequentItemProjection(transaction), frequentItemHeader)
    this
  }


  /**
   * Pattern bases are produced in the form (List(c,f,m,a),3), ...
   * This function will more efficiently insert into a Node.  The central difference
   * is the fact that counts are set from the second element of the Tuple versus
   * being grown over time.
   *
   * //TODO: add ability to use optimizations like the Single Path FP-tree
   * @param patternBase in form (List(c,f,m,a): Prefix,3: count)
   * @return
   */
  def insertPatternBase(patternBase: Pattern): FPTree = {
    val (pattern, count) = patternBase
    def recur(pattern: List[String], n: Node): Unit = {
      if (pattern.nonEmpty) {
        n.children.find(_.itemName == pattern.head) match {
          case Some(node) => //update child
            node.count += count
            recur(pattern.tail, node)
          case None => //add child
            val newNode = new FPNode(pattern.head, n, ListBuffer())
            newNode.count = count
            n.children += newNode
            if (frequentItemHeader.contains(pattern.head)) {
              //add new nodeLink in the chain
              frequentItemHeader(pattern.head)
                .lastNodeLinked.nodeLink = newNode
            } else {
              //add to the table
              frequentItemHeader(pattern.head) = newNode
            }
            recur(pattern.tail, newNode)
        }
      }
    }
    // before inserting we remove infrequent patterns (i.e. project to frequent items)
    recur(frequentItemProjection(pattern), this.root)
    this
  }

  def conditionalFPTree(itemName: String): FPTree = {
    val emptyRoot = FPNode("", null, ListBuffer())
    // form the frequent items for this new smaller DB
    val conditionalDB = conditionalPatternDB(itemName)
    // one pass to take List[(List[String], Int)] to a histogram
    /**
     * we choose to represent the intermediate patterns as List[(List[String],Int)]
     * e.g.
     * List((List(c, f, b, m),1), (List(c, f, p, m),2))
     *
     * This needs to be transformed into a frequency-sorted and filtered histogram:
     * List[(String, Int)] e.g. List((c,3),(m,3),(f,3),(p,2),(b,1))
     *
     *
     */
    val freqItems = conditionalDB.flatMap(t => t._1.map((_,t._2))) // split into single item tuples
      .groupBy(_._1) //gather by itemName into a Map
      .mapValues(_.map(_._2).sum) //sum counts
      .toList.filter(_._2 > supportThreshold).map(_._1) // take the names of the frequent ones
      .sortBy(frequentItems.indexOf(_)) // sort into the node occurrence order
    val start = new FPTree(emptyRoot, supportThreshold, freqItems)
    conditionalPatternDB(itemName).foldLeft(start)(_ insertPatternBase _)
  }

  def isSinglePath: Boolean = {
    def recur(n: Node): Boolean = {
      if (n.children.length == 0) true
      else if (n.children.length > 1) false
      else recur(n.children(0))
    }
    recur(root)
  }

  /**
   * Given a transaction database and a support threshold we construct
   * the frequent items in that database.  For every transaction the
   * frequentItemProjections(trans) is the items in the order they appear in
   * the frequentItems list.
   *
   * @param trans a transaction direct from the database
   * @return a projection of the transaction: only the frequent items in
   *         decreasing frequency order.
   */
  def frequentItemProjection(trans: Transaction): List[String] = {
    trans.filter(frequentItems.contains(_)).sortBy(frequentItems.indexOf(_))
  }

  /**
   * takes a transaction and walks the tree to determine if it is contained
   * @param trans the transaction to be walked
   * @return
   */
  def contains(trans: Transaction): Boolean = {
    @tailrec
    def recur(n: Node, t: Transaction): Boolean = {
      if (t.isEmpty) true
      else {
        n.children.find(_.itemName == t.head) match {
          case Some(node) => recur(node, t.tail)
          case None => false
        }
      }
    }
    recur(root, trans)
  }

  /**
   * The set of prefix paths of a node n in the frequentItemHeader table forms a
   * small database of patterns that co-occur with n.  This function will retrieve
   * this so-called "conditional pattern-base | n"
   *
   * @param itemName an item name that occurs in the frequentItemHeader
   * @return a list of patterns annotated with the count for that pattern
   */
  def conditionalPatternDB(itemName: String): List[Pattern] = {
    require(frequentItemHeader.contains(itemName), "can't find conditional pattern-base")
    type Pattern = (List[String], Int)
    frequentItemHeader(itemName).nodeLinkList.foldLeft(List[Pattern]())({
      (l, n) => {
        @tailrec
        def walkUp(bn: Node, acc: List[String]): List[String] = {
          if (bn.parent != null) {
            val parentName = bn.parent.itemName
            walkUp(bn.parent, if (parentName.nonEmpty) parentName :: acc else acc)
          }
          else acc
        }
        (walkUp(n, List()), n.count) :: l
      }
    })
  }

  //TODO: move out to a utility class somewhere
  def combinations(base: Item,
                   singlePath: List[(Item, Int)]): List[Pattern] = {
    (1 to singlePath.length).map(n => {
      // gather all the subsequences of size n
      val orderedPrefixs = ListMethods.orderedSubsequences(singlePath, n)
      // for each one of these merge it with the base item and get count right
      orderedPrefixs.map(op => {
        val min = op.map(_._2).min //TODO: refactor to do this biz in one pass
        (op.map(_._1) ::: List(base), min)
      })
    }).toList.flatten
  }

  def singlePathCombos(singlePath: List[(Item, Int)]): List[Pattern] = {
    (1 to singlePath.length).map(n => {
      // gather all subsequences
      val subseqs = ListMethods.orderedSubsequences(singlePath, n)
      subseqs.map(op => {
        val min = op.map(_._2).min //TODO: refactor to use last
        (op.map(_._1), min)
      })
    }).toList.flatten
  }

  def frequent1Patterns: List[Pattern] = {
    // grab each frequent item and accumulate its counts across conditional tree.
    // TODO: figure a better way to do this
    this.frequentItems.map((item: Item) =>
          (List(item), this.frequentItemHeader(item).nodeLinkList.map(_.count).sum))
  }

  def FPGrowth: List[Pattern] = {
    def joinPatterns(p1: Pattern, p2: Pattern): Pattern = {
      (p1._1 ::: p2._1, if (p1._2 < p2._2) p1._2 else p2._2)
    }
    def recur(tree: FPTree, patternBase: Pattern): List[Pattern] = {
      tree.frequent1Patterns.flatMap((pattern: Pattern) => {
        val cfpt: FPTree = tree.conditionalFPTree(pattern._1(0))
        val firstPart: List[Pattern] = cfpt.frequent1Patterns.map(p => joinPatterns(joinPatterns(p,pattern), patternBase))
        firstPart ::: recur(tree.conditionalFPTree(pattern._1(0)), joinPatterns(pattern, patternBase))
      })
    }
    this.frequent1Patterns ::: recur(this, (List(), Int.MaxValue))
  }

  def noCountsFPGrowth: List[ItemSet] = {
    def recur(tree: FPTree, itemSet: ItemSet): List[ItemSet] = {
      tree.frequentItems.flatMap((item: Item) => {
        tree.conditionalFPTree(item).frequentItems.map(_ :: item :: itemSet) ::: recur(tree.conditionalFPTree(item), item :: itemSet)
      })
    }
    this.frequentItems.map(List(_)) ::: recur(this, List())
  }
}






