import org.junit.Test
import org.junit.Assert.*

class MonoidTest:
  @Test def intAdditionWorks(): Unit =
    assertEquals(3, intAddition.op(1, 2))

  @Test def intMultiplicationWorks(): Unit =
    assertEquals(2, intMultiplication.op(1, 2))

  @Test def booleanAndWorks(): Unit =
    assertEquals(true, booleanAnd.op(true, booleanAnd.zero))
    assertEquals(false, booleanAnd.op(false, booleanAnd.zero))

  @Test def booleanOrWorks(): Unit =
    assertEquals(true, booleanOr.op(true, booleanOr.zero))
    assertEquals(false, booleanOr.op(false, booleanOr.zero))

  @Test def endoFunctionWorks(): Unit =
    val r = (a: Int) => a * 2
    val s = (a: Int) => a + 2
    val t = (a: Int) => a -1
    val em1 = endoMonoid.op(r, endoMonoid.op(s, t))
    val em2 = endoMonoid.op(endoMonoid.op(r, s), t)
    assertEquals(em1(5), em2(5))

  @Test def foldMapWorks(): Unit =
    assertEquals(18, foldMap(List(1.2, 3.4, 5.6), intMultiplication)((f: Double) => f.round.toInt))

  @Test def foldMapVWorks(): Unit =
    assertEquals(18, foldMapV(Vector(1.2, 3.4, 5.6), intMultiplication)((f: Double) => f.round.toInt))
