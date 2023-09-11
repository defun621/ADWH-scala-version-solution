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

    /*
    inserts :: a -> [a] -> [[a]]
    inserts x [] = [[x]]
    inserts x (y:ys) = (x:y:ys): map (y:) (inserts x ys)
    */
    def inserts[A](x: A, l: List[A]): List[List[A]] = 
        val step = (a: A, acc: List[List[A]]) => 
            val ys = acc.head.tail
            val head = x :: a :: ys
            val tail = acc.map(a :: _)
            head :: tail
        end step
        l.foldRight(List(List(x)))(step)

    
    /*
    perms3 [] = [[]]
    perms3 xs = [x : ys | x <- xs, ys <- perms3(remove x xs)]
    in haskell, the remove should have the signature of Eq a => a -> [a] -> [a].
    therefore, one cannot generate the permutation of a list of functions using perms3
    since there is no way to define the instance of Eq of function
    */
    def remove[A](x: A, xs: List[A]) = 
        val p: A => Boolean = i => i != x
        filter(p)(LazyList.from(xs))

    def concat[A](xss: List[List[A]]): List[A] = 
        type LF = List[A] => List[A]
        val step: (LF, List[A]) => LF  = (acc: LF, a: List[A]) => l => acc(a ++ l)
        xss.foldLeft(identity[List[A]])(step).apply(List[A]())
    end concat

    val simpleSteep: List[Int] => Boolean = xs => xs match 
        case Nil => true
        case y :: ys => simpleSteep(ys) && y > ys.sum

    val fastSteep: List[Int] => Boolean = l => 
        type Sum = Int
        type Flag = Boolean
        type Info = (Sum, Flag)
        val step: (Int, Info) => Info = (x, info) => (x + info._1, info._2 && x > info._1)
        val finalInfo = l.foldRight((0, true))(step)
        finalInfo._2

end Chapter1