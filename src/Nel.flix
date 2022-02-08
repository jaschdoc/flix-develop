instance Reducible[Nel] {
    pub def reduceLeftTo(f: (b, a) -> b & ef1, g: a -> b & ef2, l: Nel[a]): b & (ef1 and ef2) = Nel.reduceLeftTo(f, g, l)
    pub def reduceRightTo(f: (a, b) -> b & ef1, g: a -> b & ef2, l: Nel[a]): b & (ef1 and ef2) = Nel.reduceRightTo(f, g, l)
    override pub def head(l: Nel[a]): a = Nel.head(l)
    override pub def last(l: Nel[a]): a = Nel.last(l)
    override pub def init(l: Nel[a]): List[a] = Nel.init(l)
    override pub def tail(l: Nel[a]): List[a] = Nel.tail(l)
    // override pub def exists(f: a -> Bool & ef, l: Nel[a]): Bool & ef = Nel.exists(f, l)
    // override pub def forall(f: a -> Bool & ef, l: Nel[a]): Bool & ef = Nel.forall(f, l)
    // override pub def find(f: a -> Bool & ef, l: Nel[a]): Option[a] & ef = Nel.find(f, l)
    // override pub def findLeft(f: a -> Bool & ef, l: Nel[a]): Option[a] & ef = Nel.findLeft(f, l)
    // override pub def findRight(f: a -> Bool & ef, l: Nel[a]): Option[a] & ef = Nel.findRight(f, l)
    override pub def memberOf(a: a, l: Nel[a]): Bool with Eq[a] = Nel.memberOf(a, l)
    // override pub def dropWhile(f: a -> Bool & ef, l: Nel[a]): List[a] & ef = Nel.dropWhile(f, l)
    // override pub def takeWhile(f: a -> Bool & ef, l: Nel[a]): List[a] & ef = Nel.takeWhile(f, l)
    override pub def toArray(l: Nel[a]): Array[a] & Impure = Nel.toArray(l)
    override pub def toIterator(l: Nel[a]): Iterator[a] & Impure = Nel.toIterator(l)
    override pub def toList(l: Nel[a]): List[a] = Nel.toList(l)
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

