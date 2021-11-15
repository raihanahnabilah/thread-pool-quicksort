package sorting

import org.scalatest.{FunSpec, Matchers}

/**
  * A generic trait for testing sorting implementations
  */
trait SortingTests extends FunSpec with Matchers {

  val sorter: Sorting
  
  val ARRAY_SIZE: Int = 5000

  describe(s"A Sorting implementation from ${this.getClass.getSimpleName}") {
    it("should sort an array correctly") {

      // Generate random array
      val a = ArrayUtil.generateRandomArrayOfInts(ARRAY_SIZE)

      // Copy the array to keep the original result
      val aCopy = ArrayUtil.arrayCopy(a)

//      println("Array a before:")
//      println(a.mkString("Array(", ", ", ")"))
      val t1: Long = System.currentTimeMillis()
      sorter.sort(a)
      val t2: Long = System.currentTimeMillis()

      val formatter = java.text.NumberFormat.getIntegerInstance
      println()
      println(s"[Array size $ARRAY_SIZE] ${sorter.getName} time: ${formatter.format(t2 - t1)} ms")

//      println()
//      println("Array a after sorted:")
//      println(a.mkString("Array(", ", ", ")"))

//      println()
//      println("Check if array a is sorted:")
//      println(ArrayUtil.checkSorted(a))
//
//      println()
//      println("Array Acopy:")
//      println(aCopy.mkString("Array(", ", ", ")"))
//
//      println()
//      println("Array Acopy after sorted:")
//      println(aCopy.sorted.mkString("Array(", ", ", ")"))
//
//      println()
//      println("Check if array aCopy is sorted:")
//      println(ArrayUtil.checkSorted(aCopy.sorted))

      assert(ArrayUtil.checkSorted(a) == ArrayUtil.checkSorted(aCopy.sorted))
      assert(ArrayUtil.checkSameElements(a, aCopy.sorted))
    }
  }

}
