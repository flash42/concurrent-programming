package book
import scala.util.{Try, Success, Failure}

@main def main: Unit = {
    println(compose((a: Int) => a * 2, (a: Int) => a + 2)(2))
}

def compose[A, B, C] (g: B => C, f: A => B): A => C = {
    (a: A) => g(f(a))
}

def fuse[A, B] (a: Option[A], b: Option[B]): Option[(A, B)] = {
    for {
        aValue <- a
        bValue <- b
    } yield (aValue, bValue)
}

def check[T](xs: Seq[T])(pred: T => Boolean): Boolean = xs match {
    case Nil => true
    case (x +: xs) =>
        Try(pred(x)) match
            case Success(v) =>
                if (v) check[T](xs)(pred) 
                else false
            case Failure(e) =>
                false
  }

def permutations(x: String): Seq[String] = {
    Nil
}
