@main def hello: Unit = {
    println(update(1 :: 2 :: 5 :: Nil, 0, 3))
    println(suffixes(1 :: 2 :: 5 :: Nil))
}

def update[A](lst: List[A], i: Int, y: A): List[A] = (lst, i, y) match {
    case (Nil, i, y) => throw new Error("update failed")
    case (x :: xs, 0, y) => y :: xs
    case (x :: xs, i, y) => x :: update(xs, i - 1, y)
}

// Exercise 2.1
def suffixes[A](lst: List[A]): List[List[A]] = lst match {
    case Nil => Nil :: Nil
    case (x :: xs) => (x :: xs) :: suffixes(xs) // the :: operation is constant time
}

def msg = "I was compiled by Scala 3. :)"
