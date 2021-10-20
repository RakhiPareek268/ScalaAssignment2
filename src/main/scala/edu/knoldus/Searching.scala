package edu.knoldus
import scala.annotation.tailrec
class Searching {
  def binarySearch(array: Array[Int], elem: Int): Boolean = {
    @tailrec
    def bs_helper(array: Array[Int], elem: Int, start: Int, end: Int): Int = {
      if (start > end) return -1
      val mid = start + (end - start) / 2
      array(mid) match {
        case i if (i == elem) => mid
        case i if (i > elem) => bs_helper(array, elem, start, mid - 1)
        case _ => bs_helper(array, elem, mid + 1, end)
      }
    }
    val search = bs_helper(array, elem, start = 0, end = array.length - 1)
    if (search == (-1)) false
    else
      true
  }
  def linearSearch(array: Array[Int], elem: Int): Boolean = {
    def linearSearch(elem: Int, array: Array[Int], index: Int = 0): Int = {
           if (array.head.equals(elem)) index
          else if (array.tail.isEmpty) -1
          else linearSearch(elem, array.tail, index + 1)
        }
    val search = linearSearch(elem, array, index = 0)
    if(search==(-1))
      false
    else
      true
  }
}