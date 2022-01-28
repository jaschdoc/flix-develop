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
    /// Returns the concatenation of the string representation
    /// of each element in `v` with `sep` inserted between each element.
    ///
    pub def join1(sep: String, v: MutList[a]): String & Impure with ToString[a] =
        joinWith(x -> ToString.toString(x), sep, v)


    ///
    /// Returns the concatenation of the string representation
    /// of each element in `v` according to `f` with `sep` inserted between each element.
    ///
    pub def joinWith(f: a -> String & ef, sep: String, v: MutList[a]): String & Impure =
        let lastSep = 1;
        let sb = StringBuilder.new();
        foldLeft((_, x) -> StringBuilder.append!(sb, "${f(x)}" + sep), (), v);
        StringBuilder.toString(sb) |> String.dropRight(lastSep)

}
