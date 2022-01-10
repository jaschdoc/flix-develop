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

namespace Map {

    ///
    /// Returns the sum of all keys in the map `m`.
    ///
    pub def sumKeys(m: Map[Int32, v]): Int32 =
        sumWith((k, _) -> k, m)

    ///
    /// Returns the sum of all values in the map `m`.
    ///
    pub def sumValues(m: Map[k, Int32]): Int32 =
        sumWith((_, v) -> v, m)

    ///
    /// Returns the sum of all key-value pairs `k => v` in the map `m` according to the function `f`.
    ///
    // TODO: Purity polymorphic
    pub def sumWith(f: (k, v) -> Int32 & ef, m: Map[k, v]): Int32 & ef =
        foldLeftWithKey((acc, k, v) -> f(k, v) + acc, 0, m)

}
