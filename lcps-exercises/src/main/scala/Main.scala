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
            case Success(v) => if (v) check[T](xs)(pred) else false
            case Failure(e) => false
  }


case class Parts(first: String, rest: String)

def gen_first_rest_combinations[A](coll: Seq[A]): Seq[Parts] = {
    var result = List[Tuple2[A, Seq[A]]]()
    for (idx <- 0 until coll.size)
        result = (coll(idx), coll.take(idx) ++ coll.drop(idx + 1)) :: result
    return result.map((char, others) => Parts(char.toString, others.mkString("")))
}

def permutations(x: String): Seq[String] = {
    if (x.size == 1) return x :: Nil
    var result = List[String]()
    for (parts <- gen_first_rest_combinations(x))
        result = result ++ (permutations(parts.rest) map (perm_result => parts.first + perm_result))
    result
}

def combinations(n: Int, xs: Seq[Int]): Iterator[Seq[Int]] = {
    def _combinations(n: Int, prefix: List[Int], xs: List[Int]): Seq[List[Int]] = (n, xs) match {
        case (0, _) =>
            prefix :: Nil
        case (_, x::xs) =>
            // We need to calculate the successively narrower list for the iterations
            val usedItems = scala.collection.mutable.Set[Int]()
            val a = (x::xs).flatMap(el => {
                usedItems += el
                _combinations(n - 1, el :: prefix, xs.filterNot(elInner => usedItems.contains(elInner)))
            })
            a
        case (_, Nil) =>
            // May it be
            Nil
    }
    _combinations(n, Nil, xs.toSet.toList).iterator
}
