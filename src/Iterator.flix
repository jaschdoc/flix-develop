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
    /// Returns the contents of `iter` as a `Some(Nel)` if `iter` is not empty. Returns None otherwise.
    ///
    /// Consumes the entire iterator.
    ///
    pub def regToNel(iter: RegionIterator[a, r]): Option[Nel[a]] \ Read(r) = match regToList(iter) {
        case Nil     => None
        case x :: xs => Some(Nel(x, xs))
    }

    ///
    /// Returns the contents of `iter` as a nec.
    ///
    /// Consumes the entire iterator.
    ///
    pub def regToNec(iter: RegionIterator[a, r]): Option[Nec[a]] \ Read(r) =
        let f = (x, acc) -> match acc {
            case None      => Some(Nec.singleton(x))
            case Some(nec) => Some(Nec.cons(x, nec))
        };
        regFoldRight(f, None, iter)

    ///
    /// Returns the contents of `iter` as a MutDeque.
    ///
    /// Consumes the entire iterator.
    ///
    pub def regToMutDeque(r1: Region[r1], iter: RegionIterator[a, r2]): MutDeque[a, r1] \ { Read(r2), Write(r1) }  =
        let d = new MutDeque(r1);
        regForeach(x -> MutDeque.pushBack(x, d), iter);
        d

    ///
    /// Returns the contents of `iter` as a set. Consumes the entire iterator.
    ///
    pub def regToSet(iter: RegionIterator[a, r]): Set[a] \ Read(r) with Order[a] =
        regFoldLeft((acc, a) -> Set.insert(a, acc), Set.empty(), iter)

    ///
    /// Returns a list of every element in `iter` that satisfies the predicate `f`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def regFilter(f: a -> Bool & ef, iter: RegionIterator[a, r]): List[a] \ { ef, Read(r) } =
        let RegionIterator(done, next) = iter;
        def loop(k) = {
            if (done())
                k(Nil)
            else
                let x = next();
                if (f(x)) loop(ks -> k(x :: ks)) else loop(k)
        };
        loop(identity)

    ///
    /// Returns a list with `f` applied to every element in `iter`.
    ///
    /// That is, the result is of the form: `f(x1) :: f(x2) :: ...`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def regMap(f: a -> b & ef, iter: RegionIterator[a, r]): List[b] \ { ef, Read(r) } =
        let RegionIterator(done, next) = iter;
        def loop(k) = {
            if (done())
                k(Nil)
            else
                let x = f(next());
                loop(ks -> k(x :: ks))
        };
        loop(identity)

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
    /// Returns an iterator with every element of the iterator `iter` that
    /// satisfies the predicate `f`.
    ///
    /// Does *not* consume any elements from the iterator.
    ///
    /// The original iterator `iter` should not be reused.
    ///
    @Lazy
    pub def regFilterL(f: a -> Bool, iter: RegionIterator[a, r]): RegionIterator[a, r] \ { Read(r), Write(r) } =
        let RegionIterator(done, next) = iter;

        // Allocate a reference to hold the next element.
        let cursor = ref None @ Scoped.regionOf(iter);

        let done1 = () -> match deref cursor {
            case None => match regForward(f, done, next) {
                case None    => true
                case Some(x) =>
                    // An element was found. Store it in the reference.
                    cursor := Some(x);
                    false
            }
            case Some(_) => false
        };

        let next1 = () -> match deref cursor {
            case None    => bug!("Empty iterator")
            case Some(x) =>
                // An element is in the reference. Use it.
                cursor := None;
                x
        };
        RegionIterator(done1, next1)

    ///
    /// Fast forwards the given iterator until the next element that satisfies
    /// the given predicate `f`. Returns it (if it exists).
    ///
    def regForward(f: a -> Bool & ef, done: RegionDone[r], next: RegionNext[a, r]): Option[a] \ { ef, Read(r) } =
        def loop() = {
            if (done())
                None
            else
                let e = next();
                if (f(e)) Some(e) else loop()
        };
        loop()

    ///
    /// Returns an iterator with every `f` lazily applied to each element in `iter`.
    ///
    /// Does *not* consume any elements from the iterator.
    ///
    @Lazy
    pub def regMapL(f: a -> b, iter: RegionIterator[a, r]): RegionIterator[b, r] =
        let RegionIterator(done, next) = iter;
        let next1 = () -> f(next());
        RegionIterator(done, next1)

    ///
    /// Returns an iterator with every occurrence of `from` replaced by `to` in `iter`.
    ///
    /// Does *not* consume any elements from the iterator.
    ///
    @Lazy
    pub def regReplace(from: {from :: a}, to: {to :: a}, iter: RegionIterator[a, r]): RegionIterator[a, r] with Eq[a] =
        let RegionIterator(done, next) = iter;
        let next1 = () -> {
            let el = next();
            if (el == from.from) to.to else el
        };
        RegionIterator(done, next1)

    ///
    /// Returns `iterB` appended to `iterA`.
    ///
    /// Does *not* consume any elements from either iterator.
    ///
    /// The original iterators `iterA` and `iterB` should *not* be reused.
    ///
    @Lazy
    pub def regAppend(iterA: RegionIterator[a, r1], iterB: RegionIterator[a, r2]): RegionIterator[a, r1 and r2] =
        let RegionIterator(doneA, nextA) = iterA;
        let RegionIterator(doneB, nextB) = iterB;
        let doneAB = () -> doneA() and doneB();
        let nextAB = () -> if (doneA()) nextB() else nextA();
        RegionIterator(doneAB, nextAB)

    ///
    /// Returns an iterator where the element at index `i` is `(a, b)` where
    /// `a` is the element at index `i` in `iterA` and `b` is the element at index `i` in `iterB`.
    ///
    /// Does *not* consume any elements from either iterator.
    ///
    /// If either `iterA` or `iterB` is depleted, then no further elements are added to the resulting iterator.
    ///
    /// The original iterators `iterA` and `iterB` should *not* be reused.
    ///
    /// An iterator should *never* be zipped with itself.
    ///
    @Lazy
    pub def regZip(iterA: RegionIterator[a, r1], iterB: RegionIterator[b, r2]): RegionIterator[(a, b), r1 and r2] =
        let RegionIterator(doneA, nextA) = iterA;
        let RegionIterator(doneB, nextB) = iterB;
        let doneAB = () -> doneA() or doneB();
        let nextAB = () -> (nextA(), nextB());
        RegionIterator(doneAB, nextAB)

    ///
    /// Returns an iterator where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `iterA` and `b` is the element at index `i` in `iterB`.
    ///
    /// Does *not* consume any elements from either iterator.
    ///
    /// If either `iterA` or `iterB` becomes depleted, then no further elements are added to the resulting list.
    ///
    /// The original iterators `iterA` and `iterB` should *not* be reused.
    ///
    /// An iterator should *never* be zipped with itself.
    ///
    @Lazy
    pub def regZipWithL(f: (a, b) -> c, iterA: RegionIterator[a, r1], iterB: RegionIterator[b, r2]): RegionIterator[c, r1 and r2] =
        let RegionIterator(done, next) = regZip(iterA, iterB);
        let next1 = () -> {
            let (x, y) = next();
            f(x, y)
        };
        RegionIterator(done, next1)

    ///
    /// Returns an iterator where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `iterA` and `b` is the element at index `i` in `iterB`.
    ///
    /// Consumes the entire iterator.
    ///
    /// If either `iterA` or `iterB` becomes depleted, then no further elements are added to the resulting list.
    ///
    /// The original iterators `iterA` and `iterB` should *not* be reused.
    ///
    /// An iterator should *never* be zipped with itself.
    ///
    pub def regZipWith(f: (a, b) -> c & ef, iterA: RegionIterator[a, r1], iterB: RegionIterator[b, r2]): List[c] \ { ef, Read(r1, r2) } =
        regZip(iterA, iterB) |> regMap(match (x, y) -> f(x, y))

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
    ///
    /// Applies `f` to a start value `z` and all elements in `iter` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, z))...)`.
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    /// Consumes the entire iterator.
    ///
    pub def regFoldRightWithCont(f: (a, Unit -> b \ { ef, Read(r) }) -> b & ef, z: b, iter: RegionIterator[a, r]): b \ { ef, Read(r) } =
        let RegionIterator(done, next) = iter;
        def loop() = {
            if (done())
                z
            else {
                let x = next();
                f(x, _ -> loop())
            }
        };
        loop()

    ///
    /// Returns the result of mapping each element and combining the results.
    ///
    pub def regFoldMap(f: a -> b & ef, iter: RegionIterator[a, r]): b \ { ef, Read(r) } with Monoid[b] =
        regFoldLeft((acc, x) -> Monoid.combine(acc, f(x)), Monoid.empty(), iter)

    ///
    /// Applies `f` to all elements in `iter` going from left to right until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(x1, x2), x3)..., xn))`
    ///
    /// Returns `None` if `iter` is empty.
    ///
    /// Consumes the entire iterator.
    ///
    pub def regReduceLeft(f: (a, a) -> a & ef, iter: RegionIterator[a, r]): Option[a] \ { ef, Read(r) } =
        let RegionIterator(done, next) = iter;
        if (done())
            None
        else
            Some(regFoldLeft(f, next(), iter))

    ///
    /// Returns the longest prefix of `iter` that satisfies the predicate `f`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def regTakeWhile(f: a -> Bool & ef, iter: RegionIterator[a, r]): List[a] \ { ef, Read(r) } =
        let RegionIterator(done, next) = iter;
        def loop(k) = {
            if (done())
                k(Nil)
            else
                let x = next();
                if (f(x)) loop(ks -> k(x :: ks)) else k(Nil)
        };
        loop(identity)

    ///
    /// Returns `iter` without the longest prefix that satisfies the predicate `f`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def regDropWhile(f: a -> Bool & ef, iter: RegionIterator[a, r]): List[a] \ { ef, Read(r) } =
        let RegionIterator(done, next) = iter;
        match regForward(x -> not f(x), done, next) {
            case None    => Nil
            case Some(i) => i :: regToList(iter)
        }

    ///
    /// Returns the result of applying `f` to every element in `iter` and concatenating the results.
    ///
    /// Does *not* consume any elements from the iterator.
    ///
    @Lazy
    pub def regFlatMap(f: a -> RegionIterator[b, r1] \ Write(r1), iter: RegionIterator[a, r2]): RegionIterator[b, r1 and r2] \ Write(r2) =
        let RegionIterator(done, next) = iter;
        let cursor = ref None @ Scoped.regionOf(iter);
        let done1 = () -> match deref cursor {
            case None     => match regFlatMapForward(f, done, next) {
                case None     => true
                case Some(it) =>
                    cursor := Some(it);
                    false
            }
            case Some(it) =>
                let RegionIterator(d, _) = it;
                if (d())
                    match regFlatMapForward(f, done, next) {
                        case None      => true
                        case Some(it1) =>
                            cursor := Some(it1);
                            false
                    }
                else
                    false
        };
        let next1 = () -> match deref cursor {
            case None     => bug!("Empty Iterator!")
            case Some(it) =>
                let RegionIterator(_, n) = it;
                n()
        };
        RegionIterator(done1, next1)

    ///
    /// Forwards the given iterator to the first non-empty iterator produced by `f`.
    ///
    def regFlatMapForward(f: a -> RegionIterator[b, r1] & ef, done: RegionDone[r2], next: RegionNext[a, r2]): Option[RegionIterator[b, r1]] \ { ef, Read(r1, r2) } =
        if (done())
            None
        else
            let RegionIterator(d, n) = f(next());
            if (d()) regFlatMapForward(f, done, next) else Some(RegionIterator(d, n))

    ///
    /// Returns an iterator with `a` inserted between every of `iter`.
    ///
    /// Does *not* consume any elements from the iterator.
    ///
    @Lazy
    pub def regIntersperse(x: a, iter: RegionIterator[a, r]): RegionIterator[a, r] \ Write(r) =
        let RegionIterator(done, next) = iter;
        let returnX = ref false @ Scoped.regionOf(iter);
        let next1 = () -> {
            if (deref returnX) {
                returnX := false;
                x
            }
            else {
                returnX := true;
                next()
            }
        };
        RegionIterator(done, next1)
/*
    ///
    /// Returns the concatenation of the elements in `iterB` with the elements of `iterA` inserted between every two adjacent elements.
    ///
    /// That is, returns `b1 :: a1 ... an :: b2 :: ... bn-1 :: a1 :: ... :: an :: bn :: Nil`.
    ///
    /// Does *not* consume any elements from either iterator.
    ///
    /// The original iterators `iterA` and `iterB` should not be reused.
    ///
    @Lazy
    pub def intercalate(iterA: RegionIterator[a, r1], iterB: RegionIterator[RegionIterator[a, r2], r3]): RegionIterator[a, r1 and r2 and r3] \ Write(r3) =
        let RegionIterator(doneA, nextA) = iterA;
        let RegionIterator(doneB, nextB) = iterB;
        let r3 = Scoped.regionOf(iterB);
        let allRegions = () as Region[r1 or r2 or r3];
        let xs = ref Nil @ r3;
        let isReversed = ref false @ r3;
        let nextAA = () -> {
            let x = nextA();
            xs := x :: deref xs;
            x
        };
        let doneAA = () -> {
            if (doneA()) {
                if (deref isReversed)
                    ()
                else {
                    xs := List.reverse(deref xs);
                    isReversed := true
                };
                true
            }
            else
                false
        };
        let cursor = ref None @ allRegions;
        let mkCursor = i -> { // Appends an iterator with the contents of `iterA`. The boolean formula is inferred to be `b60995378 and (r3 or (not r1)) and r1`
            if (deref isReversed)
                // `iterA` has been consumed, so construct a new one from `xs`
                cursor := Some(regAppend(i, List.toRegionIterator(Scoped.regionOf(iterA), deref xs)))
            else
                cursor := Some(regAppend(i, RegionIterator(doneAA, nextAA)))
        };
        let done1 = () -> match deref cursor {
            case None    => match regIntercalateForward(doneB, nextB) {
                case None    => true
                case Some(i) =>
                    if (doneB())
                        cursor := Some(i) // Has Boolean formula r2 as it is the last iterator of `iterB`. Clashes with formula of mkCursor...
                    else
                        mkCursor(i);
                    false
            }
            case Some(i) =>
                let RegionIterator(d, _) = i;
                if (d()) match regIntercalateForward(doneB, nextB) {
                    case None     => true
                    case Some(i1) =>
                        if (doneB())
                            cursor := Some(i1)
                        else
                            mkCursor(i1);
                        false
                }
                else
                    false
        };
        let next1 = () -> match deref cursor {
            case None    => bug!("Empty Iterator!")
            case Some(i) =>
                let RegionIterator(_, n) = i;
                n()
        };
        RegionIterator(done1, next1)

    ///
    /// Forwards the iterator the first non-empty iterator produced by `next`.
    ///
    def regIntercalateForward(done: RegionDone[r1], next: RegionNext[RegionIterator[a, r2], r1]): Option[RegionIterator[a, r2]] \ Read(r1, r2) =
        if(done())
            None
        else
            let RegionIterator(d, n) = next();
            if (d()) regIntercalateForward(done, next) else Some(RegionIterator(d, n))
*/

}
