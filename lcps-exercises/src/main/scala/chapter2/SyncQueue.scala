package chapter2

case class SyncQueue[T](n: Int) extends SyncStore[T] {
  var queue = List[T]()

  override def get: T = {
    queue match {
      case x :: xs =>
        queue = xs
        x
      case Nil => throw IllegalStateException("Queue should not be empty")
    }
  }

  override def put(x: T): Unit = {
    queue match {
      case _ if queue.size >= n => throw IllegalStateException("Queue is full")
      case xs => queue = x :: xs
    }
  }

  def isEmpty: Boolean = {
    queue.isEmpty
  }

  def nonEmpty: Boolean = {
    !isEmpty
  }
}
