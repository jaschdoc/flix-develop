namespace Array1 {

    ///
    /// Returns an iterator over `a`.
    ///
    pub def toIterator(a: Array[a]): Iterator[a] & Impure = {
        let len = Array.length(a);
        let copy = a[0 .. len];
        let i = ref 0;
        let done = () -> (deref i) >= len;
        let next = () -> {
            let j = deref i;
            let x = copy[j];
            i := j + 1;
            x
        };
        Iterator(done, next)
    }

}
