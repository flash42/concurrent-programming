package chapter2

class SyncVar[T] {
  var value: T | Null = null

  def get(): T = {
    value match {
      case x: T => {
        value = null
        x
      }
      case null => throw UninitializedFieldError("Value field should be set")
    }
  }

  def put(x: T): Unit = {
    value match {
      case x: T => throw IllegalStateException("Value is already set")
      case null => value = x
    }
  }
}
