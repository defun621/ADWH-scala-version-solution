package lu
package ian

object Chapter3:

    /*
    (a, dcb)
    (ab,dc)
    (abc,d)
    */

    type SymList[A] = (List[A], List[A])
    val fromSL: [A] => SymList[A] => List[A] = [A] => (sl: SymList[A]) => sl match 
        case (xs, ys) => xs ++ (ys.reverse)

    val consSL: [A] => A => SymList[A] => SymList[A] = [A] => (a: A) => sl => sl match 
        case (Nil, ys) => (List(a), ys)
        case (xs, ys) => (a :: xs, ys)


    val snocSL: [A] => A => SymList[A] => SymList[A] = [A] => (a: A) => sl => sl match 
        case (xs, Nil) => (xs, List(a))
        case (xs, ys) => (xs, a :: ys)

    val headSL: [A] => SymList[A] => A = [A] => (sl: SymList[A]) => sl match 
        case (Nil, ys) => if ys.isEmpty then throw new UnsupportedOperationException() else ys.head
        case (xs, ys) => xs.head

    
    val tailSL: [A] => SymList[A] => SymList[A] = [A] => (sl: SymList[A]) => sl match 
        case (Nil, ys) => if ys.isEmpty then throw new UnsupportedOperationException() else (Nil, Nil)
        case (x :: Nil, ys) => 
            val (us, vs) = ys.splitAt(ys.size / 2)
            (vs.reverse, us)
        case (x :: xs, ys) => (xs, ys)

    val lastSL: [A] => SymList[A] => A = [A] => (sl: SymList[A]) => sl match 
        case (xs, Nil) => if xs.isEmpty then throw new UnsupportedOperationException() else xs.head
        case (xs, ys) => ys.head

    def nilSL[A]: SymList[A] = (List.empty[A], List.empty[A])

    val nullSL: [A] => SymList[A] => Boolean = [A] => (sl: SymList[A]) => sl._1.isEmpty && sl._2.isEmpty

    val singleSL: [A] => SymList[A] => Boolean = [A] => (sl: SymList[A]) => sl match 
        case (Nil, Nil) => false
        case (Nil, List(_)) => true
        case (List(_), Nil) => true
        case _ => false
    
    val lengthSL: [A] => SymList[A] => Int = [A] => (sl: SymList[A]) => sl._1.size + sl._2.size

    val initSL: [A] => SymList[A] => SymList[A] = [A] => (sl: SymList[A]) => sl match 
        case (Nil, Nil) => throw new UnsupportedOperationException()
        case (Nil, ys) => (Nil, Nil)
        case (xs, Nil) => (Nil, Nil)
        case (xs, y :: Nil) => 
            val (us, vs) = xs.splitAt(xs.size / 2)
            (us, vs.reverse)
        case (xs, ys) => (xs, ys.tail)

    val dropWhileSL: [A] => (A => Boolean) => SymList[A] => SymList[A] = [A] => (f: A => Boolean) => sl => sl match
        case _ if nullSL(sl) => sl
        case _ if f(headSL(sl)) => dropWhileSL(f)(tailSL(sl))
        case _ => sl
    
    /*
    inits . fromSL = map fromSL . fromSL . initsSL
    */
    val initsSL: [A] => SymList[A] => (SymList[SymList[A]]) = [A] => (sl: SymList[A]) => 
        if nullSL(sl) then
            snocSL(sl)(nilSL)
        else
            snocSL(sl)(initsSL(initSL(sl)))

    
end Chapter3 