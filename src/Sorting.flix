namespace Sorting {
    // TODO: Add tests!!!

    ///
    /// Returns the rank of `x` in the **sorted** array `a`.
    /// The array `a` may contain duplicates
    ///
    pub def rank(x: Int32, a: Array[Int32]): Int32 & Impure =
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
        loop(0, Array.length(a))

    pub def binarySearch(x: Int32, a: Array[Int32]): Int32 = ??? // & Impure

    // Add variants that use `Comparison` for comparing

}
