import org.junit.Assert.assertEquals
import org.junit.Test
import org.scalatest.Assertions._
import book.*
import chapter2.*
import util.*

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

  @Test def syncVarWorks(): Unit = {
    val syncVar = SyncVar[String]()
    assertThrows[UninitializedFieldError] {syncVar.get}
    syncVar.put("1")
    assertThrows[IllegalStateException] {syncVar.put("2")}
    assertEquals("1", syncVar.get)
  }

  @Test def produceAndConsumeWorks(): Unit = {
    assertEquals(16, produceAndConsume(16))
  }

  @Test def produceAndConsume2Works(): Unit = {
    assertEquals(16, produceAndConsume2(16))
  }


  @Test def produceAndConsume3Works(): Unit = {
    assertEquals(16, produceAndConsume3(16))
  }

  @Test def syncQueueWorks(): Unit = {
    val syncQueue = SyncQueue[String](3)
    assertThrows[IllegalStateException] {syncQueue.get}
    syncQueue.put("1")
    syncQueue.put("2")
    syncQueue.put("3")
    assertThrows[IllegalStateException] {syncQueue.put("2")}
    assertEquals("3", syncQueue.get)
    assertEquals("2", syncQueue.get)
    assertEquals("1", syncQueue.get)
  }

  @Test def sendWorks(): Unit = {
    val a = new Account("Jack", 1000)
    val b = new Account("Jill", 2000)
    val t1 = thread {
      for (i <- 0 until 100) send(a, b, 1)
    }
    val t2 = thread {
      for (i <- 0 until 100) send(b, a, 1)
    }
    t1.join();
    t2.join()

    assertEquals(1000, a.money)
    assertEquals(2000, b.money)
  }
}
