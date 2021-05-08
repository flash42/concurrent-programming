package chapter2

import collection.mutable.PriorityQueue


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
  val tasks = PriorityQueue[(Int, () => Unit)]()(Ordering.by(ordering))
  var paused = false
  var hasStarted: Boolean = false
  start()

  private def start(): Unit = {
    for (num <- 0 to poolSize.toInt) work("Worker" + num)
  }

  private def ordering(tuple2: Null | (Int, () => Unit)) = {
    if (tuple2 == null) -1 else -tuple2._1
  }

  def pause(): Unit = paused.synchronized {
    paused = true
  }

  def resume(): Unit = paused.synchronized {
    paused = false
  }

  def work(name: String): Thread = {
    val w = new Thread {
      def poll(): Option[() => Unit] = tasks.synchronized {
        paused.synchronized {
          if (!paused && tasks.nonEmpty) {
            val t = tasks.dequeue()
            Some(t._2)
          } else None
        }
      }

      override def run() = while (true) poll() match {
        case Some(task) => {
          task()
        }
        case None =>
      }
    }
    w.setName(name)
    w.setDaemon(true)
    w.start()
    w
  }

  def asynchronous(priority: Int)(task: => Unit): Unit = tasks.synchronized {
    tasks.enqueue((priority, () => task))
  }

  def shutdown(): Unit = {

  }
}

