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

def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
      if (v.isEmpty)
        println("empty")
        m.zero
      else if(v.length == 1)
        m.op(m.zero, f(v(0)))
      else
        val (fh, lh) = v.splitAt(v.length / 2)
        m.op(foldMapV(fh, m)(f), foldMapV(lh, m)(f))
}