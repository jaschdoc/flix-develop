namespace Result1 {

    ///
    /// Returns an iterator over `r` with 1 element or an empty iterator if `r` is `Err`.
    ///
    pub def toIterator(r: Result[t, e]): Iterator[t] & Impure = match r {
        case Ok(x)  =>
            let d = ref false;
            let done = () -> deref d;
            let next = () -> {
                d := true;
                x
            };
            Iterator(done, next)
        case Err(_) =>
            let done = () -> true as & Impure;
            let next = () -> bug!("Empty Iterator!") as & Impure;
            Iterator(done, next)
    }

}
