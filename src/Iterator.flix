///
/// The type of the done function.
///
type alias RegionDone[ef: Region] = Unit -> Bool & ef

///
/// The type of the next function.
///
type alias RegionNext[a: Type, ef: Region] = Unit -> a & ef


pub enum RegionIterator[a: Type, r: Region] {
    case RegionIterator(RegionDone[r], RegionNext[a, r])
}

instance Newable[RegionIterator[a]] {
    pub def new(r: Region[r]): RegionIterator[a, r] \ { Read(r), Write(r) } = RegionIterator.new(r)
}

namespace RegionIterator {

    ///
    /// Returns an empty iterator.
    ///
    pub def new(_: Region[r]): RegionIterator[a, r] \ { Read(r), Write(r) } =
        let done = () -> true as \ Read(r);
        let next = () -> bug!("Empty Iterator!") as \ Read(r);
        RegionIterator(done, next) as \ { Read(r), Write(r) }


    ///
    /// Returns an iterator containing only a single element, `x`.
    ///
    pub def regSingleton(r: Region[r], x: a): RegionIterator[a, r] \ Write(r) =
        let d = ref false @ r;
        let done = () -> deref d;
        let next = () -> {
            if (done()) {
                bug!("Empty Iterator!")
            }
            else {
                d := true;
                x
            }
        };
        RegionIterator(done, next)

    ///
    /// Returns `Some(x)` if `iter` is not empty. Returns `None` otherwise.
    ///
    /// Consumes the head element of `iter`.
    ///
    pub def regNext(iter: RegionIterator[a, r]): Option[a] \ Read(r) =
        let RegionIterator(done, next) = iter;
        if (done())
            None
        else
            Some(next())

    ///
    /// Returns true if the iterator is empty.
    ///
    /// Does **not** consume any elements of the iterator.
    ///
    pub def regIsEmpty(iter: RegionIterator[a, r]): Bool \ Read(r) =
        let RegionIterator(done, _) = iter;
        done()

    ///
    /// Returns an iterator of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns an empty iterator if `b >= e`.
    ///
    @Lazy
    pub def regRange(r: Region[r], b: Int32, e: Int32): RegionIterator[Int32, r] \ Write(r) =
        let i = ref b @ r;
        let done = () -> deref i >= e;
        let next = () -> {
                let res = deref i;
                if (res < e) {
                    i := res + 1;
                    res
                }
                else {
                    bug!("Empty Iterator!")
                }
            };
        RegionIterator(done, next)

    ///
    /// Returns an iterator over an iterable with the element `x` repeated `n` times.
    ///
    /// Returns an empty iterator if `n < 0`.
    ///
    @Lazy
    pub def regRepeat(r: Region[r], n: Int32, x: a): RegionIterator[a, r] \ Write(r) =
        let i = ref 0 @ r;
        let done = () -> deref i >= n;
        let next = () -> {
              let j = deref i;
              if (j < n) {
                i := j + 1;
                x
              }
              else {
                bug!("Empty Iterator!")
              }
            };
        RegionIterator(done, next)

    ///
    /// Returns the sum of all elements in the iterator `iter`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def sum(iter: RegionIterator[Int32, r]): Int32 \ Read(r) =
        regFoldLeft((acc, x) -> acc + x, 0, iter)

    ///
    /// Returns the sum of all elements in the iterator `iter` according to the function `f`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def sumWith(f: a -> Int32 & ef, iter: RegionIterator[a, r]): Int32 \ { ef, Read(r) } =
        regFoldLeft((acc, x) -> acc + f(x), 0, iter)

    ///
    /// Returns the product of all elements in the iterator `iter`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def product(iter: RegionIterator[Int32, r]): Int32 \ Read(r) =
        if (regIsEmpty(iter))
            0
        else
            regFoldLeft((acc, x) -> acc * x, 1, iter)

    ///
    /// Returns the product of all elements in the iterator `iter` according to the function `f`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def productWith(f: a -> Int32 & ef, iter: RegionIterator[a, r]): Int32 \ { ef, Read(r) } =
        if (regIsEmpty(iter))
            0
        else
            regFoldLeft((acc, x) -> acc * f(x), 1, iter)

    ///
    /// Returns the contents of `iter` as an array.
    ///
    /// Consumes the entire iterator.
    ///
    pub def regToArray(r1: Region[r1], iter: RegionIterator[a, r2]): Array[a, r1] \ { Read(r2), Write(r1) } = region r0 {
        let m = new MutList(r0);
        regForeach(a -> MutList.push!(a, m), iter);
        MutList.toArray(m, r1)
    }

    ///
    /// Returns the contents of `iter` as a list.
    ///
    /// Consumes the entire iterator.
    ///
    pub def regToList(iter: RegionIterator[a, r]): List[a] \ Read(r) =
        regFoldRight((a, acc) -> a :: acc, Nil, iter)

    ///
    /// Returns the contents of `iter` as a map.
    ///
    /// Consumes the entire iterator.
    ///
    pub def regToMap(iter: RegionIterator[(a, b), r]): Map[a, b] \ Read(r) with Order[a] =
        regFoldLeft((acc, ab) -> Map.insert(fst(ab), snd(ab), acc), Map.empty(), iter)

    ///
    /// Applies `f` to every element of `iter`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def regForeach(f: a -> Unit & ef, iter: RegionIterator[a, r]): Unit \ { ef, Read(r) } =
        let RegionIterator(done, next) = iter;
        def loop() = {
            if (done())
                ()
            else {
                f(next());
                loop()
            }
        };
        loop()

    ///
    /// Applies `f` to a start value `s` and all elements in `iter` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def regFoldLeft(f: (b, a) -> b & ef, s: b, iter: RegionIterator[a, r]): b \ { ef, Read(r) } =
        let RegionIterator(done, next) = iter;
        def loop(acc) = {
            if (done())
                acc
            else
                loop(f(acc, next()))
        };
        loop(s)

    ///
    /// Applies `f` to a start value `s` and all elements in `iter` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def regFoldRight(f: (a, b) -> b & ef, s: b, iter: RegionIterator[a, r]): b \ { ef, Read(r) } =
        let RegionIterator(done, next) = iter;
        def loop(k) = {
            if (done())
                k(s)
            else {
                let x = next();
                loop(ks -> k(f(x, ks)))
            }
        };
        loop(ks -> ks as & ef) // NB: Cast required because the identity continuation is pure,
                               // but the intermediate continuations are effect polymorphic

}

namespace List {

    pub def toRegionIterator(r: Region[r], l: List[a]): RegionIterator[a, r] \ Write(r) =
        let cursor = ref l @ r;
        let done = () -> match (deref cursor) {
            case Nil => true
            case _   => false
        };
        let next = () -> match (deref cursor) {
            case Nil     => bug!("Empty Iterator!")
            case x :: xs =>
                cursor := xs;
                x
        };
        RegionIterator(done, next)

}


namespace Map {

    pub def toRegionIterator(r: Region[r], m: Map[k, v]): RegionIterator[(k, v), r] \ Write(r) =
        List.toRegionIterator(r, Map.query(_ -> EqualTo, m))

}
