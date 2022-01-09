namespace Chain {

    ///
    /// Returns the sum of all elements in the array `a`.
    ///
    pub def sum(c: Chain[Int32]): Int32 =
        sumWith(x -> x, c)

    ///
    /// Returns the sum of all elements in the chain `c` according to the function `f`.
    ///
    pub def sumWith(f: a -> Int32 & ef, c: Chain[a]): Int32 & ef =
        def loop(d, acc) = match viewLeft(d) {
            case NoneLeft        => acc
            case SomeLeft(x, xs) => loop(xs, f(x) + acc)
        };
        loop(c, 0)
}
