package sorting

import pool.ThreadPool
import sorting.ParallelQuickSort.PQuickSort
import sorting.SimpleQuickSort.partition

/**
  * In-place QuickSort using Thread Pool
  */
object PooledQuickSort extends Sorting {

  def sort[T: Ordering](arr: Array[T]): Unit = {

    val pool = new ThreadPool(5)

    val lo = 0
    val hi = arr.length
    try {
      pool.startAndWait(_ => quickSort(arr, lo, hi))
    } finally {
      pool.shutdown()
    }

    def quickSort[T:Ordering](arr: Array[T], lo: Int, hi: Int): Unit = {
      if (hi - lo > 1) {
        val mid = partition(arr, lo, hi)
        pool.async(_ => quickSort(arr,lo,mid))
        pool.async(_ => quickSort(arr,mid+1,hi))
      }
    }
  }

  /**
    * Returns the name of the sorting method 
    */
  override def getName = "PooledSort"
}
