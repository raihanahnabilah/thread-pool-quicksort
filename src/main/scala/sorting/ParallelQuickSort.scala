package sorting

import sorting.SimpleQuickSort.partition
import util.ThreadID

/**
  * Parallel in-place QuickSort
  */
object ParallelQuickSort extends Sorting {

  /**
    * Returns the name of the sorting method 
    */
  override def getName = "ParallelSort"

  def sort[T: Ordering](arr: Array[T]): Unit = {
    val lo = 0
    val hi = arr.length
    quickSort(arr, lo, hi)
  }


  def quickSort[T:Ordering](arr: Array[T], lo: Int, hi: Int): Unit = {
    if (hi - lo > 1) {
      val mid = partition(arr, lo, hi)
      val thread_low = new PQuickSort(arr, lo, mid)
      val thread_high = new PQuickSort(arr, mid+1, hi)
      thread_low.start()
      thread_high.start()
      thread_low.join()
      thread_high.join()
    }
  }

  class PQuickSort[T:Ordering](arr:Array[T], lo:Int, hi:Int) extends Thread {
    override def run(): Unit = {
      quickSort(arr, lo, hi)
    }

  }
}
