namespace Sorting {

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

    @Parallel
    def _sort(a: Array[a]): Array[a] & Impure with Order[a] =
        sortHelper(a, 0, Array.length(a))

    @Parallel
    def sortHelper(a: Array[a], lo: Int32, hi: Int32): Array[a] & Impure with Order[a] =
        let l = hi - lo;
        if (l == 0)
            ???
        else {
            let t = [$DEFAULT$; l];
            ???
        }

    type alias Indices = {
        i :: Int32,
        j :: Int32,
        k :: Int32,
        l :: Int32,
        p :: Int32,
        q :: Int32
    }

    @Parallel
    def _merge(a: Array[a], b: Array[a]): Array[a] & Impure with Order[a] =
        let al = Array.length(a);
        let bl = Array.length(b);
        let cl = al + bl;
        let c = Array.copyOfRange(0, cl, a);
        let idx = {i = 0, j = al, k = 0, l = bl, p = 0, q = cl};
        mergeHelper(idx, ref a, ref b, ref c);
        c

    @Parallel
    def mergeHelper(idx: Indices, a: Ref[Array[a]], b: Ref[Array[a]], c: Ref[Array[a]]): Unit & Impure with Order[a] =
        let m = idx.j - idx.i;
        let n = idx.l - idx.k;
        if (m < n)
            mergeHelper2(idx, n, b, a, c)
        else
            mergeHelper2(idx, m, a, b, c)

    @Parallel
    def mergeHelper2(idx: Indices, m: Int32, a: Ref[Array[a]], b: Ref[Array[a]], c: Ref[Array[a]]): Unit & Impure with Order[a] =
        if (m <= 0)
            ()
        else
            let r = (idx.i - idx.j) / 2;
            match rank((deref a)[r], deref b) {
                case Some(s) =>
                    let t = idx.p + (r - idx.i) + (s - idx.k);
                    (deref c)[t] = (deref a)[r];
                    let chanL = chan Unit 1;
                    let chanH = chan Unit 1;
                    spawn (chanL <- mergeHelper({ i = idx.i, j = r, k = idx.k, l = s, p = idx.p, q = t }, a, b, c));
                    spawn (chanL <- mergeHelper({ i = r + 1, j = idx.j, k = s, l = idx.l, p = t + 1, q = idx.q }, a, b, c));
                    <- chanL;
                    <- chanH
                case None => unreachable!()
            }


    // Add variants that use `Comparison` for comparing
    // https://en.wikipedia.org/wiki/Binary_search_algorithm
    // https://en.wikipedia.org/wiki/Merge_algorithm#Parallel_merge
    // https://en.wikipedia.org/wiki/Merge_sort

}
