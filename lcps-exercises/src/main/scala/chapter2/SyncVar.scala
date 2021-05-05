package chapter2

case class SyncVar[T]() {
  var value: T | Null = null

  def get(): T = {
    value match {
      case x: T =>

        value = null
        x
      case null => throw UninitializedFieldError("Value field should be set")
    }
  }

  def put(x: T): Unit = {
      value match {
        case x: T => throw IllegalStateException("Value is already set")
        case null =>
          println("put" + x)
          value = x

    }
  }


  def isEmpty: Boolean = {
//    println("isEmpty" + value)
    value == null
  }

  def nonEmpty: Boolean = {
//    println("nonEmpty" + value)

    !isEmpty
  }

  override def toString: String = if value != null then value.toString else ""
}
