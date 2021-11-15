package sorting

import pool.ThreadPool
import sorting.SimpleQuickSort.partition

/**
  * Hybrid sorting. Unleash your creativity here!
  */
object HybridQuickSort extends Sorting {

  /**
    * Returns the name of the sorting method 
    */
  override def getName = "HybridSort"

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
        if (mid - lo > 4000){
          pool.async(_ => quickSort(arr,lo,mid))
        } else {
          quickSort(arr,lo,mid)
        }
        if(hi - (mid+1) > 4000){
          pool.async(_ => quickSort(arr,mid+1,hi))
        } else {
          quickSort(arr,mid+1,hi)
        }
      }
    }

  }


}
