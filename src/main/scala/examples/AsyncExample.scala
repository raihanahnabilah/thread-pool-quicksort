package examples

import pool.ThreadPool

/**
  * An example of pooled asynchronous execution
  * 
  * @author Ilya Sergey
  */
object AsyncExample {

  def main(args: Array[String]): Unit = {
    // Only three threads
    val pool = new ThreadPool(3)

    // Create ten tasks
    val tasks = for (i <- 1 to 10) yield {
      _: Unit => {
        println(s"Task $i")
      }
    }

    // Run the tasks asynchronously between the threads
    for (t <- tasks) {
      pool.async(t)
    }

//    println(s"Finished before executed.")

    // Wait for some time before shutting down the pool's threads
    Thread.sleep(1000)

    println("About to shut down the pool.")

    pool.shutdown()
  }
}
