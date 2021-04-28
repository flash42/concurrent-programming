
import org.junit.Test
import org.junit.Assert._
import book._

class Chapter1 {
  @Test def composeWorks(): Unit = {
    assertEquals(8, compose((a: Int) => a * 2, (a: Int) => a + 2)(2))
  }
  
  @Test def fuseSome(): Unit = {
    assertEquals(Some(1, 2), fuse(Some(1), Some(2)))
  }

  @Test def fuseNone(): Unit = {
    assertEquals(None, fuse(None, Some(2)))
    assertEquals(None, fuse(Some(1), None))
  }
  
  @Test def checkWorks(): Unit = {
    assertEquals(false, check(0 until 10)(40 / _ > 0))
    assertEquals(true, check(1 until 10)(40 / _ > 0))
  }

  @Test def permutationsWork(): Unit = {
    assertEquals(("a" :: "b" :: "c" :: Nil).permutations.toList.sorted.map(l => l.mkString("")), permutations("abc").sorted)
  }

  @Test def combinationsWork(): Unit = {
    assertEquals(List(List(3, 2, 1), List(4, 2, 1), List(4, 3, 1), List(4, 3, 2)), combinations(3, 1 :: 2 :: 3 :: 4 :: Nil).toList)
  }

  @Test def parallelWorks(): Unit = {
    assertEquals((1, 2), parallel(() => { println("a"); Thread.sleep(1000); 1 }, () => { println("b"); Thread.sleep(1000);2 }))
  }
}