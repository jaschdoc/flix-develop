namespace Option1 {

    ///
    /// Returns an iterator over `o` with 1 element or an empty iterator if `o` is `None`.
    ///
    pub def toIterator(o: Option[a]): Iterator[a] & Impure = match o {
        case None    => Iterator1.empty()
        case Some(x) => Iterator1.singleton(x)
    }
}

namespace Option {

    ///
    /// Returns `v` if `o` is `Some(v)` else `0`.
    ///
    pub def sum(o: Option[Int32]): Int32 =
        sumWith(v -> v, o)

    ///
    /// Returns `f(v)` if `o` is `Some(v)` else `0`.
    ///
    pub def sumWith(f: a -> Int32 & ef, o: Option[a]): Int32 & ef = match o {
        case None    => 0
        case Some(v) => f(v)
    }

}
