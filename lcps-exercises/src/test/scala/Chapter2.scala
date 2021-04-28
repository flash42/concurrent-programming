import org.junit.Assert.assertEquals
import org.junit.Test
import book.*

import java.util.concurrent.atomic.AtomicInteger

case class Chapter2() {

  def delayLog(msg: String): Unit = {
    println("a")
    Thread.sleep(1000)
  }

  @Test def parallelWorks(): Unit = {
    assertEquals((1, 2), parallel(() => { 1 }, () => { 2 }))
  }

  @Test def periodicallyWorks(): Unit = {
    var cnt: Int = 0
    periodically(0)(() => cnt += 1)
    assertEquals(10, cnt)
  }
}
