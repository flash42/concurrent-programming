trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

val intAddition = new Monoid[Integer] {
  def op(a1: Integer, a2: Integer) = a1 + a2
  def zero = 0
}

val intMultiplication = new Monoid[Integer] {
  def op(a1: Integer, a2: Integer) = a1 * a2
  def zero = 1
}

val booleanAnd = new Monoid[Boolean] {
  def op(a1: Boolean, a2: Boolean) = a1 && a2
  def zero = true
}

val booleanOr = new Monoid[Boolean] {
  def op(a1: Boolean, a2: Boolean) = a1 || a2
  def zero = false
}

def optionMonoid[A] = new Monoid[Option[A]] {
  def op(a1: Option[A], a2: Option[A]) = a1.orElse(a2)
  def zero = None
}

def endoMonoid[A] = new Monoid[A => A] {
  def op(a1: A => A, a2: A => A) = aVal => a1(a2(aVal))
  def zero = identity
}

@main def hello: Unit =
  println(intAddition.op(1, 2))
  println(intMultiplication.op(1, 2))
  println(booleanAnd.op(true, booleanAnd.zero))
  println(booleanAnd.op(false, booleanAnd.zero))
  println(booleanOr.op(false, booleanOr.zero))
  println(booleanOr.op(true, booleanOr.zero))
  val r = (a: Int) => a * 2
  val s = (a: Int) => a + 2
  val t = (a: Int) => a -1
  val em1 = endoMonoid.op(r, endoMonoid.op(s, t))
  val em2 = endoMonoid.op(endoMonoid.op(r, s), t)
  println(em1(5))
  println(em2(5))
  print(foldMap(List(1.2, 3.4, 5.6), intMultiplication)((f: Double) => f.round.toInt))

trait Gen[A]
trait Prop


def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
  // TODO implement property based testing
  null
}

def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)
def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
  as.foldLeft(m.zero)((a, b) => m.op(a, f(b)))
}

def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
  // TODO implement again without help :)
  foldMap(as, endoMonoid[B])(f.curried)(z)
}

