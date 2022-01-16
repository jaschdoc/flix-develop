namespace Option1 {

    ///
    /// Returns an iterator over `o` with 1 element or an empty iterator if `o` is `None`.
    ///
    pub def toIterator(o: Option[a]): Iterator[a] & Impure = match o {
        case None    => Iterator1.empty()
        case Some(x) => Iterator1.singleton(x)
    }
}
