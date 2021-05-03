package book
import util.*

import scala.util.{Failure, Success, Try}

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

def gen_first_rest_combinations[A](coll: Seq[A]): LazyList[Parts] = {
    return (0 until coll.size).to(LazyList) map (idx => Parts(coll(idx).toString, (coll.take(idx) ++ coll.drop(idx + 1)).mkString("")))
}

def permutations(x: String): LazyList[String] = {
    if (x.size == 1) return x #:: LazyList.empty
    var result = List[String]()
    return gen_first_rest_combinations(x) flatMap (parts => (permutations(parts.rest) map (perm_result => parts.first + perm_result)))
}

def combinations(n: Int, xs: Seq[Int]): Iterator[Seq[Int]] = {
    def _combinations(n: Int, prefix: LazyList[Int], xs: List[Int]): LazyList[LazyList[Int]] = (n, xs) match {
        case (0, _) =>
            prefix #:: LazyList.empty
        case (_, x::xs) =>
            // We need to calculate the successively narrower list for the iterations
            // TODO code smell: external data is used in flatMap
            val usedItems = scala.collection.mutable.Set[Int]()
            (x::xs).to(LazyList).flatMap(el => {
                usedItems += el
                _combinations(n - 1, el #:: prefix, xs.filterNot(elInner => usedItems.contains(elInner)))
            })
        case (_, Nil) =>
            // May it be
            LazyList.empty
    }
    _combinations(n, LazyList.empty, xs.toSet.toList).iterator
}


def parallel[A, B](a: () => A, b: () => B): (A, B) = {
    var aRes: Option[A] = None
    var bRes: Option[B] = None
    val t1 = thread{ aRes = Some(a()) }
    val t2 = thread{ bRes = Some(b()) }
    t1.join()
    t2.join()
    aRes.flatMap(a => bRes.map(b => (a, b))).get
}

def periodically(duration: Long)(b: () => Unit): Unit = {
    // TODO Read volatile local var: https://stackoverflow.com/questions/50107720/can-i-have-a-local-var-in-scala-as-volatile-since-in-java-it-is-not-possible
    var cnt: Int = 0
    val t = thread { while cnt < 10 do { b(); cnt += 1; Thread.sleep(duration) }}
    t.join()
}