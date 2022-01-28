namespace Array1 {

    ///
    /// Returns an iterator over `a`.
    ///
    pub def toIterator(a: Array[a]): Iterator[a] & Impure = {
        let len = Array.length(a);
        let copy = a[0 .. len];
        let i = ref 0;
        let done = () -> (deref i) >= len;
        let next = () -> {
            let j = deref i;
            let x = copy[j];
            i := j + 1;
            x
        };
        Iterator(done, next)
    }

}

namespace Array {

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `a` with `sep` inserted between each element.
    ///
    pub def join1(sep: String, a: Array[a]): String & Impure with ToString[a] =
        joinWith(x -> ToString.toString(x), sep, a)


    ///
    /// Returns the concatenation of the string representation
    /// of each element in `a` according to `f` with `sep` inserted between each element.
    ///
    pub def joinWith(f: a -> String & ef, sep: String, a: Array[a]): String & Impure =
        let lastSep = 1;
        let sb = StringBuilder.new();
        foldLeft((_, x) -> StringBuilder.append!(sb, "${f(x)}" + sep), (), a);
        StringBuilder.toString(sb) |> String.dropRight(lastSep)

}
