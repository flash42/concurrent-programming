package chapter2

trait SyncStore[T] {
  def putWait(v: T): Unit = {
    while (nonEmpty) wait()
    put(v)
  }

  def getWait(): T = {
    while (isEmpty) wait()
    get
  }
  def get: T

  def put(x: T): Unit

  def isEmpty: Boolean

  def nonEmpty: Boolean
}
