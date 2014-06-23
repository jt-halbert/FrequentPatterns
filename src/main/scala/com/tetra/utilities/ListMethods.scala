package com.tetra.utilities

/**
 * Created by jthalbert on 6/21/14.
 */
trait ListMethods {

  /**
   * In certain circumstances patterns I need to be able to calculate all the
   * ordered distinct subsequences of a list.  e.g.
   *
   * input: (a,b,c,d)
   * output: orderedSubsequences(input, 1) = ((a), (b), (c), (d))
   *         orderedSubsequences(input, 2) = ((a,b), (a,c), (a,d), (b,c), (b,d), (c,d)
   *         orderedSubsequences(input, 3) = ((a,b,c), (a,b,d), (a,c,d), (b,c,d))
   *         orderedSubsequences(input, 4) = ((a,b,c,d))
   *
   * There is a recursive relationship to be exploited here: at level n I generate by
   * looking back to level n-1.  emit a joined run of everything greater than the last element
   * of the previous generation's pattern.
   *
   * @param list
   * @param length
   * @tparam T
   * @return
   */
  //TODO: memoize for performance improvement?
  def orderedSubsequences[T](list: List[T], length: Int): List[List[T]] = {
    require(length >= 0, "negative length lists not allowed")
    length match {
      case 0 => List(List())
      case 1 => list.map(List(_))
      case n if n < list.length =>
        orderedSubsequences(list,n-1).flatMap(l => { // for each of the prior sequences
          list.drop(list.indexOf(l.last)+1) // for each element of the list to the right
            .map(elem => l ::: List(elem)) //build a new list
        })
      case n if n >= list.length => List(list)
    }
  }

}

object ListMethods extends ListMethods
