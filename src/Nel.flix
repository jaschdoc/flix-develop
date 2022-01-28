namespace Nel {

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `l` according to `f` with `sep` inserted between each element.
    ///
    pub def joinWith(f: a -> String, sep: String, l: Nel[a]): String =
        Foldable.joinWith(f, sep, l)

}
