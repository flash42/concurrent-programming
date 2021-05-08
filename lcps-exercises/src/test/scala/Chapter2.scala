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

  // TODO fix issue with sendAll. It is not a deadlock but at least we have something :)
  @Test def sendAllWorks(): Unit = {
    val r = scala.util.Random
    val from = (for (_ <- 0 to 100) yield Account("Jack", r.nextInt(100))).toSet
    val to = Account("Jill", 0)
    val from_sum = from.toList.map(_.money).sum
    val threads = for (_ <- 0 to 100) yield thread {
      Thread.sleep(r.nextLong(2))
      sendAll(from, to)
    }
    for (t <- threads) t.join()
    assertEquals(from_sum, to.money)
  }
  @Test def asyncWorks(): Unit = {
    val pool = PriorityTaskPool(PoolSize(1), PrioLimit(1))
    val r = scala.util.Random
    val syncQ = SyncQueue[String](8)
    val p1 = () => pool.asynchronous(1)({ println("A") })
    val p2 = () => pool.asynchronous(2)({ println("B") })
    val generators = List(p2, p1, p1, p2, p2, p2, p1, p1, p1)

    for (i <- 0 to 8) generators(i)()
    Thread.sleep(100)
  }

  @Test def asyncPoolWorks(): Unit = {
    val pool = PriorityTaskPool(PoolSize(8), PrioLimit(2))
    pool.pause()
    val r = scala.util.Random
    val syncQ = SyncQueue[String](8)
    val p1 = () => pool.asynchronous(1)({ println("A") })
    val p2 = () => pool.asynchronous(2)({ println("B") })
    val generators = List(p2, p1, p1, p2, p2, p2, p1, p1, p1)

    for (i <- 0 to 8) generators(i)()
    pool.resume()

    Thread.sleep(100)
  }
}
