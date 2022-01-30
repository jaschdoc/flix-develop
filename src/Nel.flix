instance Reducible[Nel] {
    pub def reduceLeftTo(f: (b, a) -> b & ef1, g: a -> b & ef2, l: Nel[a]): b & (ef1 and ef2) = Nel.reduceLeftTo(f, g, l)
    pub def reduceRightTo(f: (a, b) -> b & ef1, g: a -> b & ef2, l: Nel[a]): b & (ef1 and ef2) = Nel.reduceRightTo(f, g, l)
}

instance SemiGroup[Nel[a]] {
    pub def combine(x: Nel[a], y: Nel[a]): Nel[a] = Nel.append(x, y)
}


namespace Nel {

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `l` according to `f` with `sep` inserted between each element.
    ///
    pub def joinWith(f: a -> String, sep: String, l: Nel[a]): String =
        Foldable.joinWith(f, sep, l)



    ///
    /// Left-associative reduction of a structure.
    /// Applies `g` to the initial element of `l` and combines it
    /// with the remainder of `l` using `f` going from left to right.
    ///
    pub def reduceLeftTo(f: (b, a) -> b & ef1, g: a -> b & ef2, l: Nel[a]): b & (ef1 and ef2) = match l {
        case Nel(x, xs) => List.foldLeft(f, g(x), xs)
    }

    ///
    /// Right-associative reduction of a structure.
    /// Applies `g` to the initial element of `l` and combines it
    /// with the remainder of `l` using `f` going from right to left.
    ///
    pub def reduceRightTo(f: (a, b) -> b & ef1, g: a -> b & ef2, l: Nel[a]): b & (ef1 and ef2) =
        def loop(ll) = match ll { // TODO: Tail recursive impl
            case x :: Nil => g(x)
            case x :: xs  => f(x, loop(xs))
            case _        => unreachable!()
        };
        let Nel(x, xs) = l;
        loop(x :: xs)


}

