package adt

import adt.FPTree.Transaction

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
   * Construct an FPTree from a sequence of transactions
   */
  type Transaction = List[String]

  /**
   * Construct the global mutable objects that will be carried along
   */
  var frequentItemHeader = mutable.Map.empty[String, ListBuffer[FPNode]]
  var orderedItems: List[String] = List()

  /**
   * Construct an empty FPTree.
   */
  def empty: FPTree = new FPTree(FPNode("",null, ListBuffer()), frequentItemHeader)

  //TODO: create a richer Transaction class
  def apply(transactions: List[Transaction]): FPTree = {
    val freqList = transactions.flatten.groupBy(w => w).mapValues(_.size).
      toList.sortBy(_._2).reverse

    orderedItems = freqList.map(_._1) //TODO: implement a check here for freq threshold
    //orderedItems = List("f","c","a","b","m","p")
    transactions.foldLeft(FPTree.empty)(_ insert _)
  }
}

abstract class Node {
  def itemName: String
  def children: ListBuffer[Node]
  def parent: Node
  var count = 0

  def insert(freqItems: List[String]): Node = {

    /**
     * If root has a child n such that n.itemName == items.head, then increment n's count by 1;
     * else create a new child node with count initialized to 1, its nodeLink linked to the
     * nodes with the same itemName.  If items.tail is non-empty then
     * call insertTree(items.tail,n)
     *
     * @param items these are the items pulled from a transaction; in a more general case
     *              they could have data accessed by item.itemName //TODO: implement that.
     * @param r a representation of a tree
     * @return
     */
    def insertTree(items: List[String], r: Node): Unit = {
      if (items.nonEmpty) {
        val i = r.children.indexWhere(_.itemName == items.head)
        if (i > -1) {
          r.children(i).count += 1
          insertTree(items.tail,r.children(i))
        } else {
          var newNode = FPNode(items.head,r, ListBuffer())
          newNode.count += 1
          r.children += newNode
          if (FPTree.frequentItemHeader.contains(items.head)) {
            FPTree.frequentItemHeader(items.head) += newNode
          } else {
            FPTree.frequentItemHeader(items.head) = ListBuffer(newNode)
          }
          insertTree(items.tail, newNode)
        }
      }
    }
    insertTree(freqItems, this)
    this
  }
}

case class FPNode(itemName: String,parent: Node, children: ListBuffer[Node]) extends Node {
  override def toString: String = itemName+":"+this.count
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
 */
case class FPTree(root: Node,
                  frequentItemHeader: mutable.Map[String,ListBuffer[FPNode]]) {

  def preOrderWalk(visit: Node => Any) {
    def recur(n: Node) {
      visit(n)
      n.children.foreach(recur)
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
    val freqItems = transaction.filter(FPTree.orderedItems.contains(_)).
      sortBy(FPTree.orderedItems.indexOf(_))
    val r = root.insert(freqItems)
    FPTree(r, FPTree.frequentItemHeader)
  }

  def conditionalFPTree(itemName: String): FPTree = {
    conditionalPatternBase(itemName).foldLeft(new FPTree(FPNode("",null,ListBuffer()),
      mutable.Map.empty[String, ListBuffer[FPNode]]))({
      (fp,ps) => {
        ???
      }
    })

  }

  /**
   * The set of prefix paths of a node n in the frequentItemHeader table forms a
   * small database of patterns that co-occur with n.  This function will retrieve
   * this so-called "conditional pattern-base | n"
   *
   * @param itemName an item name that occurs in the frequentItemHeader
   * @return a list of patterns annotated with the count for that pattern
   */
  def conditionalPatternBase(itemName: String): List[(List[String], Int)] = {
    require(FPTree.frequentItemHeader.contains(itemName),"can't find conditional pattern-base")
    type Pattern = (List[String], Int)
    FPTree.frequentItemHeader(itemName).foldLeft(List[Pattern]())({
      (l,n) => {
        def walkUp(bn: Node, acc: List[String]): List[String] = {
          if (bn.parent!=null) {
            val parentName = bn.parent.itemName
            walkUp(bn.parent, if (parentName.nonEmpty) parentName :: acc else acc)
          }
          else acc
        }
        (walkUp(n,List()), n.count) :: l
      }
    })
  }
}





