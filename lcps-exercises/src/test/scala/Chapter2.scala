import org.junit.Assert.assertEquals
import org.junit.Test
import book.*

case class Chapter2() {

  def delayLog(msg: String): Unit = {
    println("a")
    Thread.sleep(1000)
  }

  @Test def parallelWorks(): Unit = {
    assertEquals((1, 2), parallel(() => { 1 }, () => { 2 }))
  }
}
