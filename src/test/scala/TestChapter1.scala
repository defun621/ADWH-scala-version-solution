package lu
package ian

import org.scalacheck.*

object Chapter1Spec extends Properties("Chapter1") {
    
    import Prop.{forAll, propBoolean}

    property("integer") = forAll { (n: Int) =>
        (n > 0) ==> {
            def helper(x: Int, acc: List[Int]): List[Int] = x match 
                case 0 => if acc.isEmpty then List(0) else acc
                case _ => helper(x / 10, (x % 10) :: acc)
            end helper
            Chapter1.integer(helper(n, List.empty)) == n
        }
    }

    property("fraction") = forAll(Gen.pick(10, 0 to 9).map(_.toList)) { l =>
        val target = "0." ++ l.mkString
        val result = Chapter1.fraction(l)
        result == target.toDouble
    }

    property("inserts") = forAll { (l: List[Int]) =>
        def inserts(x: Int, xs: List[Int]): List[List[Int]] = xs match 
            case Nil => List(List(x))
            case y :: ys => (x :: y :: ys) :: inserts(x, ys).map(y :: _)
        end inserts

        def sig(l: List[List[Int]]): String = 
            l.map(_.mkString).sorted.mkString

        sig(Chapter1.inserts(0, l)) == sig(inserts(0, l))
    }

    import Gen._
    import Arbitrary.arbitrary

    property("concat") = forAll(Gen.containerOf[List, List[Int]](Gen.containerOf[List, Int](arbitrary[Int]))) { xss =>
        Chapter1.concat(xss) == xss.flatten
    }

    property("steep") = forAll { (l: List[Int]) =>
        Chapter1.simpleSteep(l) == Chapter1.fastSteep(l)
    }
}
