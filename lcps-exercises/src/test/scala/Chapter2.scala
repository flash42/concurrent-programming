import org.junit.Assert.assertEquals
import org.junit.Test
import org.scalatest.Assertions._
import book.*
import chapter2.SyncVar

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
    val syncVar= SyncVar[String]()
    assertThrows[UninitializedFieldError] {syncVar.get()}
    syncVar.put("1")
    assertThrows[IllegalStateException] {syncVar.put("2")}
    assertEquals("1", syncVar.get())
  }

}
