package chapter2

import collection.mutable.PriorityQueue

// TODO make it work with guarded blocks
// TODO implement shutdown

opaque type PoolSize = Int
opaque type PrioLimit = Int
object PoolSize:
  def apply(i: Int): PoolSize = i

object PrioLimit:
  def apply(i: Int): PrioLimit = i
extension (x: PoolSize | PrioLimit)
  def toInt: Int = x
  def < (other: Int): Boolean = x < other
  def > (other: Int): Boolean = x > other

case class PriorityTaskPool(poolSize: PoolSize, prioLimit: PrioLimit) {
  val tasks = PriorityQueue[(Int, String => Unit)]()(Ordering.by(ordering))
  @volatile var paused = false
  var hasStarted: Boolean = false
  start()

  private def start(): Unit = {
    for (num <- 0 to poolSize.toInt) work("Worker" + num)
  }

  private def ordering(tuple2: Null | (Int, String => Unit)) = {
    if (tuple2 == null) -1 else -tuple2._1
  }

  def pause(): Unit = {
    paused = true
  }

  def resume(): Unit = tasks.synchronized {
    paused = false
    tasks.notifyAll()
  }

  def work(name: String): Thread = {
    val w = new Thread {
      def poll(): Option[String => Unit] = tasks.synchronized {
          while (paused || tasks.isEmpty) tasks.wait()
          Some(tasks.dequeue()._2)
      }

      override def run() = while (true) {
        poll() match {
          case Some(x) => x(name)
          case None =>
        }
      }
    }
    w.setName(name)
    w.setDaemon(true)
    w.start()
    w
  }

  def asynchronous(priority: Int)(task: String => Unit): Unit = tasks.synchronized {
    tasks.enqueue((priority, task))
    tasks.notifyAll()
  }

  def shutdown(): Unit = {
    
  }
}

