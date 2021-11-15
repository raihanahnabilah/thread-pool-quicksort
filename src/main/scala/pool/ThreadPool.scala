package pool

import java.util.concurrent.locks.ReentrantLock
import scala.collection.mutable

/**
  * A simplistic thread pool implementation
  */
class ThreadPool(private val n: Int) {

  // A queue of pending tasks to be implemented
  private val taskQueue = new mutable.Queue[Unit => Unit]()

  // An array of worker threads that are allocated to execute tasks in parallel.
  private var workers: Array[Worker] = Array()

  private val workersHelper: Array[Worker] = {
    for (i <- 0 until n){
      workers = workers ++ Array(new Worker(i))
    }
    workers
  }

  // An array of flags indicating which threads are currently active
  private val workInProgress = Array.fill(n)(false)

  // A flag indicating that the thread-pool is no longer usable
  // It is annotated as @volatile. Make sure to use it as such.
  @volatile
  private var isShutDown = false

  private val lock = new ReentrantLock()
  private val isEmpty = lock.newCondition()
  private val onProgress = lock.newCondition()

  {
    for (i <- 0 until n){
      workers(i).start()
    }
  }



  private class Worker(val id: Int) extends Thread {
    override def run() = {
      // Next task
      var task: Unit => Unit = null

      try {
        while (true) {
          lock.lock()
          try {
            if (taskQueue.isEmpty && workInProgress.forall(x => x == false)){
              onProgress.signalAll()
            }
            while (taskQueue.isEmpty) {
              isEmpty.await()
            }
            task = taskQueue.dequeue()
          } finally {
            lock.unlock()
          }

          workInProgress(id) = true
          task()
          workInProgress(id) = false
        }
      } catch {
        case _: InterruptedException =>
//         println(s"Thread $id is now stopped.")
      }

    }
  }


  /**
    * Shuts down the thread pool by interrupting all its worker threads.
    * After this the thread pool is no longer usable.
    */
  def shutdown(): Unit = {
    if (isShutDown) throw ThreadPoolException("Thread pool is no longer active")
    lock.lock()
    try {
      for (i <- 0 until n){
        workers(i).interrupt()
      }
      isShutDown = true
    } finally {
      lock.unlock()
    }
  }


  /**
    * Schedule a new task for execution by some thread in the thread pool.
    * `async()` doe not block the caller, but neither does it guarantee that 
    * the task will be completed before `async()` returns.
    *
    * @param task a task to executre concurrently
    */
  def async(task: Unit => Unit): Unit = {
    if (!isShutDown) {
      lock.lock()
      try {
        taskQueue.enqueue(task)
        isEmpty.signalAll()
      } finally {
        lock.unlock()
      }
    } else {
      println("The thread is already shut down!")
    }
  }


  /**
    * Takes an initial tasks and blocks until all threads in the pool finish their work.
    * That is, the other threads may allocate future tasks. Yet, the thread that
    * invoked this method will be blocked until the process of creating tasks
    * and completing them reaches a "quiescent moment". Make sure to understand how
    * the "quiescence" for the thread pool is defined.
    *
    * @param task an initial task to executed by some thread in the pool
    */
  def startAndWait(task: Unit => Unit): Unit = {
    if (!isShutDown) {
      lock.lock()
      try {
        taskQueue.enqueue(task)
        isEmpty.signalAll()
        while (!taskQueue.isEmpty || !workInProgress.forall(x => x == false)) {
          onProgress.await()
        }
      } finally {
        lock.unlock()
      }
    } else {
      println("The thread is already shut down!")
    }
  }
}

/**
  * An exception used to indicate the invalid state of this thread pool 
  */
case class ThreadPoolException(msg: String) extends Exception(msg)
