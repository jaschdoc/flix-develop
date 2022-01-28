namespace Chain {

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `c` according to `f` with `sep` inserted between each element.
    ///
    pub def joinWith(f: a -> String, sep: String, c: Chain[a]): String =
        Foldable.joinWith(f, sep, c)

}
