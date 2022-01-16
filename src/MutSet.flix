namespace MutSet1 {

    ///
    /// Returns the sum of all elements in the mutable set `s`.
    ///
    pub def sum(s: MutSet[Int32]): Int32 & Impure =
        let MutSet(ss) = s;
        Set.sum(deref ss)

    ///
    /// Returns the sum of all elements in the mutable set `s` according to the function `f`.
    ///
    pub def sumWith(f: a -> Int32 & ef, s: MutSet[a]): Int32 & Impure =
        let MutSet(ss) = s;
        Set.sumWith(f, deref ss)

}
