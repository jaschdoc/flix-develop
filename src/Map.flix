namespace Map1 {

    ///
    /// Returns an iterator over all key-value pairs in `m`.
    ///
    pub def toIterator(m: Map[a, b]): Iterator[(a, b)] & Impure =
        List1.toIterator(Map.query(_ -> EqualTo, m))

    ///
    /// Returns a copy of `m` as a `DelayMap`.
    ///
    pub def toDelayMap(m: Map[k, v]): DelayMap[k, v] = {
        let Map(t) = m;
        DMap(RedBlackTree.mapWithKey((_, v) -> lazy v, t))
    }
}
