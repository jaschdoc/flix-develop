mod EffectExamples.Range {

    eff Emission {
        pub def emit(e: Int32): Unit
    }

    ///
    /// Iterates over the range between `lo` and `hi`.
    ///
    pub def range(lo: Int32, hi: Int32): Unit \ Emission =
        if (lo > hi) {
            ()
        }
        else {
            do Emission.emit(lo);
            range(lo + 1, hi)
        }

    ///
    /// Returns the sum from `0` to `n`.
    ///
    pub def sumTo(n: Int32): Int32 = region rc {
        let s = Ref.new(rc, 0);
        try {
            range(0, n);
            Ref.get(s)
        } with Emission {
            def emit(e, k) = {
                Ref.put(Ref.get(s) + e, s);
                k()
            }
        }
    }

}
