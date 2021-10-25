namespace Map1 {

    ///
    /// Returns an iterator over all key-value pairs in `m`.
    ///
    pub def toIterator(m: Map[a, b]): Iterator[(a, b)] & Impure =
        List1.toIterator(Map.query(_ -> EqualTo, m))

}
