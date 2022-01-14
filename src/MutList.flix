namespace MutList1 {

    ///
    /// Returns the sum of all elements in the MutList `v`.
    ///
    pub def sum(v: MutList[Int32]): Int32 & Impure =
        sumWith(x -> x, v)

    ///
    /// Returns the sum of all elements in the MutList `v` according to the function `f`.
    ///
    pub def sumWith(f: a -> Int32 & ef, v: MutList[a]): Int32 & Impure =
        let MutList(a0, l0) = v;
        let a = deref a0;
        let l = deref l0;
        def loop(i, acc) =
            if (i < l)
                loop(i + 1, f(a[i]) + acc)
            else
                acc;
        loop(0, 0)

}


namespace MutList {

    ///
    /// Returns the product of all elements in the mutable list `v`.
    ///
    pub def product(v: MutList[Int32]): Int32 & Impure =
        productWith(x -> x, v)

    ///
    /// Returns the product of all elements in the mutable list `v` according to the function `f`.
    ///
    pub def productWith(f: a -> Int32 & ef, v: MutList[a]): Int32 & Impure =
        if (isEmpty(v))
            0
        else {
            let MutList(a_, l_) = v;
            let a = deref a_;
            let l = deref l_;
            def loop(i, acc) =
                if (i < l)
                    loop(i + 1, f(a[i]) * acc)
                else
                    acc;
            loop(0, 1)
        }
}
