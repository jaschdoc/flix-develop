use Benchmark.Benchmark;
use Benchmark.defBenchmark;

///
/// An DevIterator is a stream of elements that are produced on demand.
///
pub enum DevIterator[a] {
    case DevIterator(Done, Next[a])
}

namespace DevIterator {

    ///
    /// Returns `Some(x)` if `x` is the first element of `iter`.
    ///
    /// Returns `None` if `iter` is empty.
    ///
    /// Consumes the first element of the DevIterator.
    ///
    pub def head(iter: DevIterator[a]): Option[a] & Impure = {
        let DevIterator(done, next) = iter;
        if (done())
            None
        else
            Some(next())
    }

    ///
    /// Returns an DevIterator of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns an empty DevIterator if `b >= e`.
    ///
    pub def range(b: Int32, e: Int32): DevIterator[Int32] & Impure =
        let i = ref b;
        let d: Done = () -> deref i >= e;
        let n: Next[Int32] = () -> {
                let res = deref i;
                if (res < e) {
                    i := res + 1;
                    res
                }
                else bug!("Empty DevIterator!")
            };
        DevIterator(d, n)

    ///
    /// Returns an DevIterator over an iterable with the element `x` repeated `n` times.
    ///
    /// Returns an empty DevIterator if `n < 0`.
    ///
    pub def repeat(x: a, n: Int32): DevIterator[a] & Impure =
        let i = ref 0;
        let done: Done = () -> deref i >= n;
        let next: Next[a] = () -> {
              let j = deref i;
              if (j > n)
                  bug!("Empty DevIterator!")
              else {
                  i := j + 1;
                  x
              }
            };
        DevIterator(done, next)

    ///
    /// Returns `true` if and only if `iter` contains the element `x`.
    ///
    /// Consumes elements of the DevIterator until an element equal to `x` is found.
    ///
    pub def memberOf(x: a, iter: DevIterator[a]): Bool & Impure with Eq[a] =
        let DevIterator(done, next) = iter;
        if (done())
            false
        else
            next() == x or memberOf(x, iter)

    ///
    /// Returns all elements in the DevIterator `iter` as an array.
    ///
    /// Consumes the entire DevIterator.
    ///
    pub def toArray(iter: DevIterator[a]): Array[a] & Impure =
        toArrayHelper(MutList.new(), iter) |> MutList.toArray

    ///
    /// Helper function for `toArray`
    ///
    def toArrayHelper(v: MutList[a], iter: DevIterator[a]): MutList[a] & Impure =
        let DevIterator(done, next) = iter;
        if (done())
            v
        else {
            v |> MutList.push!(next());
            toArrayHelper(v, iter)
        }

    ///
    /// Returns the association DevIterator `iter` as a map.
    ///
    /// If `iter` contains multiple mappings with the same key, `toMap` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    /// Consumes the entire DevIterator.
    ///
    pub def toMap(iter: DevIterator[(a, b)]): Map[a, b] & Impure with Order[a] =
        toMapHelper(iter, Map.empty())

    ///
    /// Helper function for `toMap`
    ///
    def toMapHelper(iter: DevIterator[(a, b)], m: Map[a, b]): Map[a, b] & Impure with Order[a] =
        let DevIterator(done, next) = iter;
        if (done())
            m
        else
            let (x, y) = next();
            toMapHelper(iter, Map.insert(x, y, m))

    ///
    /// Returns all elements in the DevIterator `iter` as a set.
    ///
    /// Consumes the entire DevIterator.
    ///
    pub def toSet(iter: DevIterator[a]): Set[a] & Impure with Order[a] =
        toSetHelper(iter, Set.empty())

    ///
    /// Helper function for `toSet`
    ///
    def toSetHelper(iter: DevIterator[a], xs: Set[a]): Set[a] & Impure with Order[a] =
        let DevIterator(done, next) = iter;
        if (done())
            xs
        else
            let x = next();
            toSetHelper(iter, Set.insert(x, xs))

    ///
    /// Returns `iter` with every occurrence of `x` replaced by `y`.
    ///
    /// Does *not* consume any elements of the DevIterator.
    ///
    /// The original DevIterator `iter` should not be reused.
    ///
    pub def replace(x: a, y: a, iter: DevIterator[a]): DevIterator[a] with Eq[a] =
        let DevIterator(done, next) = iter;
        let n = () -> {
            let el = next();
            if (el == x) y else el
        };
        DevIterator(done, n)

    ///
    /// Returns `true` if and only if at least one element in `iter` satisfies the predicate `f`.
    ///
    /// Returns `false` if `iter` is empty.
    ///
    /// The function `f` must be pure.
    ///
    /// Consumes elements of the DevIterator until an element that satisfies predicate `f` is found.
    ///
    pub def exists(f: a -> Bool, iter: DevIterator[a]): Bool & Impure =
        let DevIterator(done, next) = iter;
        if (done())
            false
        else
            f(next()) or exists(f, iter)

    ///
    /// Returns `true` if and only if all elements in `iter` satisfy the predicate `f`.
    ///
    /// Returns `true` if `iter` is empty.
    ///
    /// The function `f` must be pure.
    ///
    /// Consumes the entire DevIterator.
    ///
    pub def forall(f: a -> Bool, iter: DevIterator[a]): Bool & Impure =
        let DevIterator(done, next) = iter;
        if (done())
            true
        else
            f(next()) and forall(f, iter)

    ///
    /// Returns `iter` without the longest prefix that satisfies the predicate `f`.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    /// - If `f` is pure then it is applied lazily.
    /// - If `f` is impure then it is applied eagerly.
    ///
    /// Consumes elements of the DevIterator until the first element that does *not* satisfy predicate `f`.
    ///
    /// The original DevIterator `iter` should not be reused.
    ///
    pub def dropWhile(f: a -> Bool & ef, iter: DevIterator[a]): DevIterator[a] & Impure = dropWhileE(f as a -> Bool & Impure, iter)
        /*
        matchEff f {
            case Pure   => dropWhileL(f as a -> Bool, iter)
            case Impure => dropWhileE(f as a -> Bool & Impure, iter)
        }
        */

    ///
    /// Returns `iter` without the longest prefix that satisfies the predicate `f`, where `f` is applied *lazily*.
    ///
    /// The function `f` must be pure.
    ///
    /// Does *not* consume any elements of the DevIterator.
    ///
    /// The original DevIterator `iter` should not be reused.
    ///
    /*
    def dropWhileL(f: a -> Bool, iter: DevIterator[a]): DevIterator[a] & Impure =
        let needsEvaluation = ref true;
        let newIter = ref iter;
        // If another function asks whether the DevIterator returned by dropWhileL is done, it must evaluate
        // the dropped part such that a consumer knows whether it is empty or not.
        let done = () -> {
            if (deref needsEvaluation) {
                newIter := dropWhileE(f as a -> Bool & Impure, iter);
                needsEvaluation := false
            } else ();
            let DevIterator(d, _) = deref newIter;
            d()
        };
        let next = () -> {
            let DevIterator(_, n) = deref newIter;
            n()
        };
        DevIterator(done, next)
    */

    ///
    /// Returns `iter` without the longest prefix that satisfies the predicate `f`, where `f` is applied *eagerly*.
    ///
    /// The function `f` must be impure.
    ///
    /// Consumes elements of the DevIterator until the first element that does *not* satisfy predicate `f`.
    ///
    /// The original DevIterator `iter` should not be reused.
    ///
    def dropWhileE(f: a ~> Bool, iter: DevIterator[a]): DevIterator[a] & Impure =
        let DevIterator(done, next) = iter;
        if (done())
            iter
        else
            let peekedElement = next();
            let shouldReturnPeekedElement = ref true;
            let n = () -> {
                if (deref shouldReturnPeekedElement) {
                    shouldReturnPeekedElement := false;
                    peekedElement
                } else next()
            };
            if (f(peekedElement)) dropWhile(f, iter) else DevIterator(done, n)

    ///
    /// Returns the longest prefix of `iter` that satisfies the predicate `f`.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    /// - If `f` is pure then it is applied lazily.
    /// - If `f` is impure then it is applied eagerly (consuming elements of the DevIterator until and including
    /// the first element that does *not* satisfy predicate `f`).
    ///
    /// The original DevIterator `iter` should not be reused.
    ///
    pub def takeWhile(f: a -> Bool & ef, iter: DevIterator[a]): DevIterator[a] & Impure = takeWhileE(f as a -> Bool & Impure, iter)
        /*
        matchEff f {
            case Pure   => takeWhileL(f as a -> Bool, iter)
            case Impure => takeWhileE(f as a -> Bool & Impure, iter)
        }
        */

    ///
    /// Returns the longest prefix of `iter` that satisfies the predicate `f`.
    /// `f` is applied *lazily*.
    ///
    /// Does *not* consume any elements of the DevIterator.
    ///
    /// The original DevIterator `iter` should not be reused.
    ///
    /*
    def takeWhileL(f: a -> Bool, iter: DevIterator[a]): DevIterator[a] & Impure =
        let DevIterator(done, next) = iter;
        if (done())
            DevIterator(() -> true as & Impure, () -> bug!("Empty DevIterator") as & Impure)
        else
            let x = ref default;
            let p = () -> {
                x := next();
                not f(deref x)
            };
            let done1 = () -> done() or p();
            let next1 = () -> deref x;
            DevIterator(done1, next1)
    */

    ///
    /// Returns the longest prefix of `iter` that satisfies the predicate `f`.
    /// `f` is applied *eagerly*.
    ///
    /// Consumes elements of the DevIterator until and including the first element that does *not* satisfy predicate `f`.
    ///
    /// The original DevIterator `iter` should not be reused.
    ///
    def takeWhileE(f: a -> Bool & Impure, iter: DevIterator[a]): DevIterator[a] & Impure =
        let DevIterator(done, _) = iter;
        if (done())
            iter
        else
            takeWhileEHelper(f, iter, Nil)

    ///
    /// Helper function for takeWhileE
    ///
    ///
    /// The original DevIterator `iter` should not be reused.
    ///
    def takeWhileEHelper(f: a -> Bool & Impure, iter: DevIterator[a], acc: List[a]): DevIterator[a] & Impure =
        let DevIterator(done, next) = iter;
        if (done())
            acc |> List.reverse |> DevIterator.toIter
        else
            let x = next();
            if (f(x))
                takeWhileEHelper(f, iter, x :: acc)
            else
                let d = () -> true as & Impure;
                takeWhileEHelper(f, DevIterator(d, next), acc)

    ///
    /// Applies `f` to every element of `iter`.
    ///
    /// The function `f` must be impure.
    ///
    /// Consumes the entire DevIterator.
    ///
    pub def foreach(f: a ~> Unit, iter: DevIterator[a]): Unit & Impure =
        let DevIterator(done, next) = iter;
        if (not done()) {
            f(next());
            foreach(f, iter)
        }
        else ()

    ///
    /// Returns an DevIterator where the element at index `i` is `(a, b)` where
    /// `a` is the element at index `i` in `iterA` and `b` is the element at index `i` in `iterB`.
    ///
    /// If either `iterA` or `iterB` is depleted, then no further elements are added to the resulting DevIterator.
    ///
    /// Consumes elements of both `iterA` and `iterB` equal to the length of the shortest DevIterator.
    /// If `iterA` and `iterB` are equal length, both DevIterators are fully consumed.
    ///
    /// The original DevIterators `iterA` and `iterB` should not be reused.
    ///
    pub def zip(iterA: DevIterator[a], iterB: DevIterator[b]): DevIterator[(a, b)] =
        let DevIterator(doneA, nextA) = iterA;
        let DevIterator(doneB, nextB) = iterB;
        let doneAB = () -> doneA() or doneB();
        let nextAB = () -> (nextA(), nextB());
        DevIterator(doneAB, nextAB)

    ///
    /// Returns an DevIterator where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `iterA` and `b` is the element at index `i` in `iterB`.
    ///
    /// If either `iterA` or `iterB` becomes depleted, then no further elements are added to the resulting list.
    ///
    /// Consumes elements of both `iterA` and `iterB` equal to the length of the shortest DevIterator.
    /// If `iterA` and `iterB` are equal length, both DevIterators are fully consumed.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    /// - If `f` is pure then it is applied lazily.
    /// - If `f` is impure then it is applied eagerly.
    ///
    /// The original DevIterators `iterA` and `iterB` should not be reused.
    ///
    pub def zipWith(f: (a, b) -> c & ef, iterA: DevIterator[a], iterB: DevIterator[b]): DevIterator[c] & Impure =
        matchEff f {
            case Pure   => zipWithL(f as (a, b) -> c & Pure,   iterA, iterB)
            case Impure => zipWithE(f as (a, b) -> c & Impure, iterA, iterB)
        }

    ///
    /// Returns an DevIterator where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `iterA` and `b` is the element at index `i` in `iterB`.
    ///
    /// Applies `f` *lazily*.
    ///
    /// If either `iterA` or `iterB` becomes depleted, then no further elements are added to the resulting list.
    ///
    /// Consumes elements of both `iterA` and `iterB` equal to the length of the shortest DevIterator.
    /// If `iterA` and `iterB` are equal length, both DevIterators are fully consumed.
    ///
    /// The original DevIterators `iterA` and `iterB` should not be reused.
    ///
    def zipWithL(f: (a, b) -> c, iterA: DevIterator[a], iterB: DevIterator[b]): DevIterator[c] =
        let DevIterator(doneA, nextA) = iterA;
        let DevIterator(doneB, nextB) = iterB;
        let doneAB = () -> doneA() or doneB();
        let nextAB = () -> f(nextA(), nextB());
        DevIterator(doneAB, nextAB)

    ///
    /// Returns an DevIterator where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `iterA` and `b` is the element at index `i` in `iterB`.
    ///
    /// Applies `f` *eagerly*.
    ///
    /// If either `iterA` or `iterB` becomes depleted, then no further elements are added to the resulting list.
    ///
    /// Consumes elements of both `iterA` and `iterB` equal to the length of the shortest DevIterator.
    /// If `iterA` and `iterB` are equal length, both DevIterators are fully consumed.
    ///
    /// The original DevIterators `iterA` and `iterB` should not be reused.
    ///
    // TODO: Optimize this (use helper function)
    def zipWithE(f: (a, b) -> c & Impure, iterA: DevIterator[a], iterB: DevIterator[b]): DevIterator[c] & Impure =
        let listA = toList(iterA);
        let listB = toList(iterB);
        List.zipWith(f, listA, listB) |> toIter

    ///
    /// Applies `f` to a start value `s` and all elements in `iter` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    /// Consumes the entire DevIterator.
    ///
    pub def foldLeft(f: (b, a) -> b & ef, s: b, iter: DevIterator[a]): b & Impure =
        let DevIterator(done, next) = iter;
        if (done())
            s
        else
            foldLeft(f, f(s, next()), iter)

    ///
    /// Applies `f` to a start value `s` and all elements in `iter` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    /// Consumes the entire DevIterator.
    ///
    pub def foldRight(f: (a, b) -> b & ef, s: b, iter: DevIterator[a]): b & Impure =
        let DevIterator(done, next) = iter;
        if (done())
            s
        else
            f(next(), foldRight(f, s, iter))

    ///
    /// Optionally returns the first element of `iter` that satisfies the predicate `f` when searching from right to left.
    ///
    /// The function `f` must be pure.
    ///
    /// Consumes the entire DevIterator.
    ///
    pub def findRight(f: a -> Bool, iter: DevIterator[a]): Option[a] & Impure =
        toList(iter) |> List.findRight(f)

    ///
    /// Returns `true` if and only if the DevIterator `iter` has no more elements.
    ///
    /// Does *not* consume any elements of the DevIterator.
    ///
    pub def isEmpty(iter: DevIterator[a]): Bool & Impure =
        let DevIterator(done, _) = iter;
        done()

	///
    /// Returns the number of elements in `iter` that satisfies the predicate `f`.
	///
	/// Consumes the entire DevIterator.
    ///
    pub def count(f: a -> Bool, iter: DevIterator[a]): Int32 & Impure = countAcc(f, iter, 0)

    def countAcc(f: a -> Bool, iter: DevIterator[a], acc: Int32): Int32 & Impure =
        if (isEmpty(iter))
            acc
        else if (f(next(iter)))
            countAcc(f, iter, acc + 1)
        else
            countAcc(f, iter, acc)

    ///
    /// Returns an DevIterator without the first `n` elements of the DevIterator `iter`.
    ///
	/// Consumes the first `n` elements of the DevIterator.
    ///
    /// The original DevIterator `iter` should not be reused.
    ///
    pub def drop(n: Int32, iter: DevIterator[a]): DevIterator[a] & Impure =
        if (n <= 0)
            iter
        else if (isEmpty(iter))
            iter
        else {
            next(iter);
            drop(n  - 1, iter)
        }

    ///
    /// Returns an DevIterator with the first `n` elements of the DevIterator `iter`.
    ///
    /// Does *not* consume any elements from the DevIterator.
    ///
    /// The original DevIterator `iter` should not be reused.
    ///
    pub def take(n: Int32, iter: DevIterator[a]): DevIterator[a] & Impure =
        if (n <= 0)
            DevIterator(() -> true as & Impure, () -> bug!("Empty DevIterator") as & Impure)
        else {
            let DevIterator(done, next) = iter;
            let count = ref 0;
            let done1 = () -> deref count >= n or done();
            let next1 = () -> {
                count := (deref count + 1);
                next()
            };
            DevIterator(done1, next1)
        }

	///
	/// Returns an DevIterator with the function `f` applied to every element of
	/// the DevIterator `iter`.
	///
	/// Whether `f` is applied eagerly or lazily depends on its purity:
    /// - If `f` is pure then it is applied lazily.
    /// - If `f` is impure then it is applied eagerly (consuming the DevIterator).
    ///
    /// The original DevIterator `iter` should not be reused.
    ///
    pub def map(f: a -> b & ef, iter: DevIterator[a]): DevIterator[b] & Impure =
        matchEff f {
            case Pure   => mapL(f as a -> b, iter)
            case Impure => mapE(f as a -> b & Impure, iter)
        }

    ///
	/// Returns an DevIterator with the pure function `f` applied *lazily* to every
	/// element of the DevIterator `iter`.
	///
	/// Does *not* consume any elements from the DevIterator.
	///
	/// The original DevIterator `iter` should not be reused.
    ///
    def mapL(f: a -> b, iter: DevIterator[a]): DevIterator[b] =
		let DevIterator(done, next) = iter;
		let next1 = () -> f(next());
		DevIterator(done, next1)

	///
	/// Returns an DevIterator with the impure function `f` applied eagerly to
	/// every element of the DevIterator `iter`.
	///
	/// Consumes all elements in the DevIterator.
	///
	/// The original DevIterator `iter` should not be reused.
    ///
    def mapE(f: a -> b & Impure, iter: DevIterator[a]): DevIterator[b] & Impure =
		toList(iter) |> List.map(f) |> toIter

    ///
    /// Returns an DevIterator with every element of the DevIterator `iter` that
    /// satisfies the predicate `f`.
	///
	/// Whether `f` is applied eagerly or lazily depends on its purity:
	///
    /// - If `f` is pure then it is applied lazily.
    /// - If `f` is impure then it is applied eagerly (consuming the DevIterator).
    ///
    /// The original DevIterator `iter` should not be reused.
    ///
    pub def filter(f: a -> Bool & ef, iter: DevIterator[a]): DevIterator[a] & Impure =
        matchEff f {
            case Pure   => filterL(f as a -> Bool, iter)
            case Impure => filterE(f as a -> Bool & Impure, iter)
        }

    ///
    /// Returns an DevIterator with every element of the DevIterator `iter` that
    /// satisfies the predicate `f`.
	///
	/// Does *not* consume any elements from the DevIterator.
	///
	/// The original DevIterator `iter` should not be reused.
    ///
	pub def filterL(f: a -> Bool, iter: DevIterator[a]): DevIterator[a] & Impure =
        let DevIterator(done, next) = iter;

        // Allocate a reference to hold the next element.
        let cursor = ref None;

        let done1 = () -> match deref cursor {
            case None => match forward(f, done, next) {
                case None    => true
                case Some(x) =>
                    // An element was found. Store it in the reference.
                    cursor := Some(x);
                    false
            }
            case Some(_) => false
        };

        let next1 = () -> match deref cursor {
            case None    => bug!("Empty DevIterator")
            case Some(x) =>
                // An element is in the reference. Use it.
                cursor := None;
                x
        };

        DevIterator(done1, next1)

    ///
    /// Returns an DevIterator with every element of the DevIterator `iter` that
    /// satisfies the predicate `f`.
	///
	/// Consume the entire DevIterator.
	///
	/// The original DevIterator `iter` should not be reused.
    ///
	pub def filterE(f: a -> Bool & Impure, iter: DevIterator[a]): DevIterator[a] & Impure =
		(toList(iter) |> List.filter(f as a -> Bool) |> toIter) as & Impure

    ///
    /// Alias for `findLeft`.
    ///
    pub def find(f: a -> Bool & ef, iter: DevIterator[a]): Option[a] & Impure = findLeft(f, iter)

    ///
    /// Optionally returns the first element of `iter` that satisfies the
    /// predicate `f` when searching from left to right.
    ///
    pub def findLeft(f: a -> Bool & ef, iter: DevIterator[a]): Option[a] & Impure =
        let DevIterator(done, next) = iter;
        forward(f, done, next)

	///
	/// Returns an DevIterator over the elements of the given list `l`.
	///
    pub def toIter(l: List[a]): DevIterator[a] & Impure =
        let cursor = ref l;
        let done = () -> List.isEmpty(deref cursor);
        let next = () -> match (deref cursor) {
            case Nil     => bug!("Empty DevIterator.")
            case x :: xs =>
                cursor := xs;
                x
        };
        DevIterator(done, next)

	///
	/// Returns all elements in the given DevIterator `iter` as a list.
	///
	/// Consumes the entire DevIterator.
	///
    pub def toList(iter: DevIterator[a]): List[a] & Impure = toListAcc(iter, Nil) |> List.reverse

	def toListAcc(iter: DevIterator[a], rs: List[a]): List[a] & Impure =
		if (isEmpty(iter)) rs else toListAcc(iter, next(iter) :: rs)


	///
	/// Returns the next element of the given DevIterator `iter`.
	///
	/// Consumes one element from the DevIterator.
	///
	/// Note: The DevIterator _must_ have a next element.
	///
    def next(iter: DevIterator[a]): a & Impure = // TODO: Avoid because it is slow to destruct multiple times?
        let DevIterator(_, n) = iter;
        n()

	///
	/// Fast forwards the given DevIterator until the next element that satisfies
	/// the given predicate `f`. Returns it (if it exists).
	///
    def forward(f: a -> Bool & ef, done: Done, next: Next[a]): Option[a] & Impure =
        if (done())
            None
        else {
            let e = next();
            if (f(e)) Some(e) else forward(f, done, next)
        }

}
/*
pub def benchmarks(): Array[Benchmark] & Impure = [
        benchmarkDevIterator(),
        benchmarkList()
    ] |> Array.flatten

pub def benchmarkList(): Array[Benchmark] & Impure =
    let pipeline = n ->
        List.range(1, n) |>
        List.map(x -> x + 1) |>
        List.map(x -> List.length(List.range(1, 100)) + x) |>
        List.map(x -> x + 1) |>
        List.filter(x -> x % 2 == 0) |>
        List.filter(x -> x % 2 == 0) |>
        List.filter(x -> x % 2 == 0) |>
        List.map(x -> x + 1) |>
        List.map(x -> List.length(List.range(1, 100)) + x) |>
        List.map(x -> x + 1) |>
        List.take(n / 4) |>
        List.count(constant(true));
    [
        defBenchmark("List (n = ${2 **  2})", () -> { pipeline(2 **  2) }),
        defBenchmark("List (n = ${2 **  4})", () -> { pipeline(2 **  4) }),
        defBenchmark("List (n = ${2 **  8})", () -> { pipeline(2 **  8) }),
        defBenchmark("List (n = ${2 ** 12})", () -> { pipeline(2 ** 12) })
    ]

pub def benchmarkDevIterator(): Array[Benchmark] & Impure =
    let pipeline = n ->
        List.range(1, n) |>
        DevIterator.toIter |>
        DevIterator.map(x -> x + 1) |>
        DevIterator.map(x -> List.length(List.range(1, 100)) + x) |>
        DevIterator.map(x -> x + 1) |>
        DevIterator.filter(x -> x % 2 == 0) |>
        DevIterator.filter(x -> x % 2 == 0) |>
        DevIterator.filter(x -> x % 2 == 0) |>
        DevIterator.map(x -> x + 1) |>
        DevIterator.map(x -> List.length(List.range(1, 100)) + x) |>
        DevIterator.map(x -> x + 1) |>
        DevIterator.take(n / 4) |>
        DevIterator.count(constant(true));
    [
        defBenchmark("DevIterator (n = ${2 **  2})", () -> { pipeline(2 **  2) as & Pure }),
        defBenchmark("DevIterator (n = ${2 **  4})", () -> { pipeline(2 **  4) as & Pure }),
        defBenchmark("DevIterator (n = ${2 **  8})", () -> { pipeline(2 **  8) as & Pure }),
        defBenchmark("DevIterator (n = ${2 ** 12})", () -> { pipeline(2 ** 12) as & Pure })
    ]


//pub def main(_args: Array[String]) : Int32 & Impure =
//    Benchmark.runWithBudget(benchmarks(), 15_000_000_000i64)
//
//pub def main2(_args: Array[String]) : Int32 & Impure =
//    List.range(1, 4) |>
//    DevIterator.toIter |>
//    DevIterator.map(x -> { println("a"); x + 1 } as & Pure) |>
//    DevIterator.map(x -> { println("b"); x } as & Pure) |>
//    DevIterator.toList;
//    0
//
//pub def main3(_args: Array[String]) : Int32 & Impure =
//    let b1 = () -> {
//        List.range(1, 1000) |>
//        List.map(x -> List.range(1, x)) |>
//        List.find(l -> List.length(l) > 10)
//    };
//    let b2 = () -> {
//        List.range(1, 1000) |>
//        DevIterator.toIter |>
//        DevIterator.map(x -> List.range(1, x)) |>
//        DevIterator.find(l -> List.length(l) > 10)
//    } as & Pure;
//    Benchmark.runWithBudget([
//        defBenchmark("Bench 1", b1),
//        defBenchmark("Bench 2", b2)
//    ],
//    15_000_000_000i64)

*/