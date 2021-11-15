package sorting

import sorting.ArrayUtil.swap

/**
  * Simple sequential in-place QuickSort
  */
object SimpleQuickSort extends Sorting {

  // This is necessary to enable implicit `Ordering` on `Int`s
  // Do not remove this import!
  import Ordering.Implicits._

  /**
    * A standard sub-routine of QuickSort that partitions an array
    * into two parts with regard to the pivot 
    * (<= pivot and > picot, correspondingly ).
    */

  def partition[T: Ordering](arr: Array[T], lo: Int, hi: Int): Int = {
    val pivot = arr(hi - 1)
    var i = lo
    for (j <- lo until hi - 1){
      if (arr(j) <= pivot){
        ArrayUtil.swap(arr, i, j)
        i = i + 1
      }
    }
    ArrayUtil.swap(arr, i, hi-1)
    i
  }

  def sort[T: Ordering](arr: Array[T]): Unit = {

    val lo = 0
    val hi = arr.length
    quickSort(arr, lo, hi)

    def quickSort[T: Ordering](arr: Array[T], lo: Int, hi: Int): Unit = {
      if (hi - lo > 1) {
        val mid = partition(arr, lo, hi)
        quickSort(arr, lo, mid)
        quickSort(arr, mid+1, hi)
      }
    }
  }


  /**
    * Returns the name of the sorting method 
    */
  override def getName = "SimpleSort"
}