package lu
package ian

object Chapter1:
    val uncons : [A] => LazyList[A] => Option[(A, LazyList[A])] = [A] => (l: LazyList[A]) => if l.isEmpty then None else Some((l.head, l.tail))

    val wrap: [A] => A => LazyList[A] = [A] => (a: A) => LazyList(a)

    val unwrap: [A] => LazyList[A] => A = [A] => (l: LazyList[A]) => l.head

    val single: [A] => LazyList[A] => Boolean = [A] => (l: LazyList[A]) => l match 
        case x #:: LazyList() => true
        case _ => false

    def reverse[A](l: List[A]): List[A] = l.foldLeft(List.empty)(_ prepended _)

    def map[A, B](f: A => B)(l: LazyList[A]): LazyList[B] = 
        val step: (A, LazyList[B]) => LazyList[B] = (a: A, acc: LazyList[B]) => f(a) #:: acc
        l.foldRight(LazyList.empty)(step)

    def filter[A](p: A => Boolean)(l: LazyList[A]): LazyList[A] =
        val step: (A, LazyList[A]) => LazyList[A] = (a, acc) => if p(a) then a #:: acc else acc
        l.foldRight(LazyList.empty)(step)


    /*
    foldr f e . filter p = foldr op e
    where 
      op x xs = if p x then f x xs else xs
    */

    def takeWhile[A](p: A => Boolean)(l: LazyList[A]): LazyList[A] = 
        val step: (A, LazyList[A]) => LazyList[A] = (a, acc) => if p(a) then a #:: acc else LazyList.empty
        l.foldRight(LazyList.empty)(step)

    def dropWhileEnd[A](p: A => Boolean)(l: LazyList[A]): LazyList[A] = 
        val step: (A, LazyList[A]) => LazyList[A] = (a, acc) => if p(a) && acc.isEmpty then LazyList.empty else a #:: acc
        l.foldRight(LazyList.empty)(step)

    /*
    foldl f e xs = if null xs then e else f (foldl f e (init xs)) (last xs) 
    this definition makes the time complexity become O(n^2)
    */

    /*
    when f is associative operation and e is the unit element of the associative operation, 
    foldl f e xs = foldr f e xs
    */

    val integer: List[Int] => Int = l =>
        val step = (acc: Int, i: Int) => acc * 10 + i
        l.foldLeft(0)(step)

    val fraction: List[Int] => Double = l =>
        val step = (i: Int, acc: Double) => acc / 10 + i.toDouble / 10
        l.foldRight(0.0)(step)

    /*
    map (foldl f e) . inits = scanl f e
    map (foldr f e) . tails = scanr f e
    */

    val apply: [A] => Int => (A => A) => A => A = [A] => (x: Int) => (f: A => A) => (a: A) => x match 
        case 0 => a
        case _ => apply(x - 1)(f)(f(a))

end Chapter1