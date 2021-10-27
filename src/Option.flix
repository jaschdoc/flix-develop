namespace Option1 {

    ///
    /// Returns an iterator over `o` with 1 element or an empty iterator if `o` is `None`.
    ///
    pub def toIterator(o: Option[a]): Iterator[a] & Impure = match o {
        case None    =>
            let done = () -> true;
            let next = () -> bug!("Empy Iterator!");
            Iterator(done, next)
        case Some(x) =>
            let d = ref false;
            let done = () -> deref d;
            let next = () -> {
                d := true;
                x
            };
            Iterator(done, next)
    }

}
