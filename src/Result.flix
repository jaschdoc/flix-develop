namespace Result1 {

    ///
    /// Returns an iterator over `r` with 1 element or an empty iterator if `r` is `Err`.
    ///
    pub def toIterator(r: Result[t, e]): Iterator[t] & Impure = match r {
        case Err(_) => Iterator1.empty()
        case Ok(x)  => Iterator1.singleton(x)
    }

}
