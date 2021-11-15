package sorting

import scala.reflect.ClassTag
import scala.util.Random

/**
  * Utility functions for testing implementations with arrays
  */
object ArrayUtil {


  /**
    * Swaps elements with indices `i` and `j` in a n array `a`.
    *
    * Ensures that both `i` and `j` are in `a`'s range and throws an
    * exception otherwise.
    */
  def swap[T](a: Array[T], i: Int, j: Int): Unit = {
      val curr = a(i)
      a(i) = a(j)
      a(j) = curr
  }

  /**
    * The name says it all. Returns an array of a given size
    * populated with random integers. 
    */
  def generateRandomArrayOfInts(size: Int): Array[Int] = {
    Array.fill(size)(Random.nextInt)
  }

  /**
    * Generates a random integer. Both ends are inclusive. 
    */
  def generateRandomIntBetween(low: Int, high: Int): Int = {
    assert(low <= high, "Low end should be less then high end.")
    val r = math.random()
    val range = high - low
    (range * r).toInt + low
  }

  // This is necessary to enable implicit `Ordering` on `Int`s
  // Do not remove this import!
  import Ordering.Implicits._

  /**
    * Check if an array `a` is sorted 
    */
  def checkSorted[T: Ordering](a: Array[T]): Boolean = {
    var result: List[Boolean] = Nil
    for (i <- 0 until a.length - 1){
      if (a(i) <= a(i+1)){
        result = true :: result
      } else {
        result = false :: result
      }
    }
    result.forall(x => x == true)
  }

  /**
    * Check if an array `a` has the same elements as an array `b`
    */
  def checkSameElements[T: Ordering](a: Array[T], b: Array[T]): Boolean = {
    a.sameElements(b)
  }

  /**
    * Copies an array `a` into a new one and returns it
    */
  def arrayCopy[T: ClassTag](a: Array[T]): Array[T] = {
    val aCopy = new Array[T](a.length)
    a.copyToArray(aCopy)
    aCopy
  }


}
