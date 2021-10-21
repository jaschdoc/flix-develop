/*
pub enum StreamListed[a, s] {
    case StreamL(s -> Step[a, s], s)
    case Listed(List[a])
}

namespace StreamListed {


    pub def empty(): StreamListed[a, s] =
        StreamL(_ -> Done, default)


    ///
    /// Returns true if and only if the step function of `s` evaluates to `Done`.
    ///
    pub def isEmpty(s: StreamListed[a, s]): Bool = match s {
        case StreamL(next0, s0) => match next0(s0) {
            case Done => true
            case _    => false
        }
        case Listed(l)          => l |> List.isEmpty
    }


    pub def toList(s: StreamListed[a, s]): List[a] = match s {
        case StreamL(next0, s0) => toListAcc(next0, s0, ks -> ks)
        case Listed(l)          => l
    }

    ///
    /// Helper function for `toList`.
    ///
    def toListAcc(next: s -> Step[a, s], s0: s, k: List[a] -> List[a]): List[a] = match next(s0) {
        case Done         => k(Nil)
        case Skip(s1)     => toListAcc(next, s1, k)
        case Yield(x, s1) => toListAcc(next, s1, ks -> k(x :: ks))
    }

    ///
    /// Returns a stream of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    pub def range(b: Int32, e: Int32): StreamListed[Int32, Int32] =
        let next = x -> if (x >= e) Done else Yield(x, x + 1);
        StreamL(next, b)




    // `filterL` has multiple return types which causes an issue.
    // a simple type cast breaks is unsound (breaks during runtime).


    ///
    /// Returns a stream of every element in `s` that satisfies the predicate `f`.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    ///
    /// - If `f` is pure then it is applied lazily.
    /// - If `f` is impure then it is applied eagerly.
    ///
    pub def filter(f: a -> Bool & ef, s: StreamListed[a, s]): StreamListed[a, s] & ef =
        if (reify ef)
            filterL(f as a -> Bool & Pure,   s)
        else
            filterE(f as a -> Bool & Impure, s) as & ef

    ///
    /// Returns a stream of every element in `s` that satisfies the predicate `f`.
    ///
    /// Applies `f` lazily.
    ///
    def filterL(f: a -> Bool, s: StreamListed[a, s]): StreamListed[a, s] = match s {
        case StreamL(next0, s0) =>
            let next1 = s1 -> match next0(s1) {
                case Done         => Done
                case Skip(s2)     => Skip(s2)
                case Yield(x, s2) => if (f(x)) Yield(x, s2) else Skip(s2)
            };
            StreamL(next1, s0)
        case Listed(l)          =>
            let next = (l1) -> match l1 {
                case Nil     => Done
                case x :: xs => if (f(x)) Yield(x, xs) else Skip(xs)
            };
            StreamL(next, l)
    }

    ///
    /// Returns a stream of every element in `s` that satisfies the predicate `f`.
    ///
    /// Applies `f` eagerly.
    ///
    def filterE(f: a -> Bool & Impure, s: StreamListed[a, s]): StreamListed[a, s] & Impure = match s {
        case StreamL(nexta, sa) => Listed(filterEAcc(f, nexta, sa, ks -> ks))
        case Listed(l)          => Listed(List.filter(f, l))
    }

    ///
    /// Helper function for `filterE`.
    ///
    def filterEAcc(f: a -> Bool & Impure, next: s -> Step[a, s], s0: s, k: List[a] -> List[a]): List[a] & Impure = match next(s0) {
        case Done         => k(Nil)
        case Skip(s1)     => filterEAcc(f, next, s1, k)
        case Yield(x, s1) => if (f(x)) filterEAcc(f, next, s1, ks -> k(x :: ks)) else filterEAcc(f, next, s1, k)
    }

}

*/
