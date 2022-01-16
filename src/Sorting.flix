namespace Sorting {
    // TODO: Add tests!!!

    ///
    /// Returns the rank of `x` in the **sorted** array `a`.
    /// The array `a` may contain duplicates
    ///
    pub def rank(x: a, a: Array[a]): Option[Int32] & Impure with Order[a] =
        def loop(lo, hi) =
            if (lo < hi) {
                let m = (lo + hi) / 2; // Automatically floored due to truncation
                if (a[m] < x)
                    loop(m + 1, hi)
                else
                    loop(lo, m)
            }
            else
                lo;
        let l = Array.length(a);
        if (l == 0)
            None
        else
            Some(loop(0, Array.length(a)))

    ///
    /// Optionally returns the position of the first occurrence of `x` in `a`
    /// searching from left to right.
    ///
    /// Returns `Some(i)` where `i` is the left-most index of `x` if `x` is in `a`. Returns `None` otherwise.
    ///
    /// The implementation uses binary search.
    ///
    pub def indexOfLeft(x: a, a: Array[a]): Option[Int32] & Impure with Order[a] = match Array.sort(a) |> rank(x) {
        case Some(i) if a[i] == x => Some(i)
        case _                    => None
    }

    ///
    /// Returns `Some(i)` where `i` is the right-most index of `x` if `x` is in `a`. Returns `None` otherwise.
    ///
    /// The implementation uses binary search.
    ///
    pub def indexOfRight(x: Int32, a: Array[Int32]): Option[Int32] & Impure =
        let a1 = Array.sort(a);
        ???

    // Add variants that use `Comparison` for comparing
    // https://en.wikipedia.org/wiki/Binary_search_algorithm
    // https://en.wikipedia.org/wiki/Merge_algorithm#Parallel_merge
    // https://en.wikipedia.org/wiki/Merge_sort

}
