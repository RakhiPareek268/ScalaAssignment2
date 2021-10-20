package edu.knoldus
import scala.annotation.tailrec
class Sorting {
  def insertionSort(array: Array[Int]): Array[Int]= {
    val list: List[Int] = array.toList
    def insert(elem : Int, list : List[Int]) : List[Int] = {
      list match{
        case Nil => List(elem)
        case y :: list1=>
          if(y >= elem) elem:: list
          else y :: insert(elem, list1)
      }
    }
    def isort(list : List[Int]) : List[Int] = {
      list match{
        case Nil => Nil
        case x :: list1 => insert(x, isort(list1))
      }
    }
    isort(list).toArray
  }
  def selectionSort(array: Array[Int]): Array[Int] = {
    val list:List[Int]=array.toList
    @tailrec
    def selectSortHelper(list:List[Int],accumList:List[Int] = List[Int]()): List[Int] =
    {
      list match
      {
        case Nil => accumList
        case _ => {
          val min  = list.min
          val requiredList = list.filter(_ != min)
          selectSortHelper(requiredList, accumList ::: List.fill(list.length - requiredList.length)(min))
        }
      }
    }
    selectSortHelper(list).toArray
  }

def bubbleSort(array:Array[Int]): Array[Int] = {
    for (i <- 1 to array.length - 1) {
      for (j <- (i - 1) to 0 by -1) {
        if (array(j) > array(j + 1)) {
          val temp = array(j + 1)
          array(j + 1) = array(j)
          array(j) = temp
        }
      }
    }
  array
  }
}


