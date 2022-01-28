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
    /// Returns the concatenation of the string representation
    /// of each element in `s` according to `f` with `sep` inserted between each element.
    ///
    pub def joinWith(f: a -> String, sep: String, s: Set[a]): String =
        Foldable.joinWith(f, sep, s)

}
