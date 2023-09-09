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
}
