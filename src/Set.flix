namespace Set1 {
  /*
    pub def count(f: a -> Bool & ef, s: Set[a]): Int32 & ef =
        if (reify ef)
            // parallelize
            let Set(rbTree) = s;
            let blackHeight = RedBlackTree.blackHeight(rbTree);
            let threshold = 2000;
            if (blackHeight > threshold)


        else
            Set.foldLeft((acc, x) -> if (f(x)) 1 + acc else acc, 0, xs) as & ef
*/

    ///
    /// Returns the set `s` as a `LazyList`.
    ///
    pub def toLazyList(s: Set[a]): LazyList[a] =
        Set.foldLeft((acc, x) -> ECons(x, acc), ENil, s)

    /*
    pub def tester(s: Set[a]): Unit = match (reifyType a) {
        case ReifiedInt32 => ???
    }
    */

    ///
    /// Returns an iterator over `s`.
    ///
    pub def toIterator(s: Set[a]): Iterator[a] & Impure =
        List1.toIterator(Set.toList(s))

}

namespace Set {

    ///
    /// Returns the sum of all elements in the set `s`.
    ///
    pub def sum(s: Set[Int32]): Int32 =
        sumWith(x -> x, s)

    ///
    /// Returns the sum of all elements in the set `s` according to the function `f`.
    ///
    pub def sumWith(f: a -> Int32 & ef, s: Set[a]): Int32 & ef =
        foldLeft((acc, x) -> f(x) + acc, 0, s)

}
