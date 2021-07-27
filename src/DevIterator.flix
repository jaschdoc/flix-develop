use Benchmark.Benchmark;
use Benchmark.defBenchmark;

namespace DevIterator {

    ///
    /// Returns `Some(x)` if `x` is the first element of `iter`.
    ///
    /// Returns `None` if `iter` is empty.
    ///
    /// Consumes the first element of the iterator.
    ///
    pub def head(iter: Iterator[a]): Option[a] & Impure = {
        let Iterator(done, next) = iter;
        if (done())
            None
        else
            Some(next())
    }

    ///
    /// Returns an iterator of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns an empty iterator if `b >= e`.
    ///
    pub def range(b: Int32, e: Int32): Iterator[Int32] & Impure =
        let i = ref b;
        let d: Done = () -> deref i >= e;
        let n: Next[Int32] = () -> {
                let res = deref i;
                if (res < e) {
                    i := res + 1;
                    res
                }
                else bug!("Empty Iterator!")
            };
        Iterator(d, n)

    ///
    /// Returns an iterator over an iterable with the element `x` repeated `n` times.
    ///
    /// Returns an empty iterator if `n < 0`.
    ///
    pub def repeat(x: a, n: Int32): Iterator[a] & Impure =
        let i = ref 0;
        let done: Done = () -> deref i >= n;
        let next: Next[a] = () -> {
              let j = deref i;
              if (j > n)
                  bug!("Empty Iterator!")
              else {
                  i := j + 1;
                  x
              }
            };
        Iterator(done, next)

    ///
    /// Returns `true` if and only if `iter` contains the element `x`.
    ///
    /// Consumes elements of the iterator until an element equal to `x` is found.
    ///
    pub def memberOf(x: a, iter: Iterator[a]): Bool & Impure with Eq[a] =
        let Iterator(done, next) = iter;
        if (done())
            false
        else
            next() == x or memberOf(x, iter)

    ///
    /// Returns all elements in the iterator `iter` as an array.
    ///
    /// Consumes the entire iterator.
    ///
    pub def toArray(iter: Iterator[a]): Array[a] & Impure =
        toArrayHelper(MutList.new(), iter) |> MutList.toArray

    ///
    /// Helper function for `toArray`
    ///
    def toArrayHelper(v: MutList[a], iter: Iterator[a]): MutList[a] & Impure =
        let Iterator(done, next) = iter;
        if (done())
            v
        else {
            v |> MutList.push!(next());
            toArrayHelper(v, iter)
        }

    ///
    /// Returns the association iterator `iter` as a map.
    ///
    /// If `iter` contains multiple mappings with the same key, `toMap` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    /// Consumes the entire iterator.
    ///
    pub def toMap(iter: Iterator[(a, b)]): Map[a, b] & Impure with Order[a] =
        toMapHelper(iter, Map.empty())

    ///
    /// Helper function for `toMap`
    ///
    def toMapHelper(iter: Iterator[(a, b)], m: Map[a, b]): Map[a, b] & Impure with Order[a] =
        let Iterator(done, next) = iter;
        if (done())
            m
        else
            let (x, y) = next();
            toMapHelper(iter, Map.insert(x, y, m))

    ///
    /// Returns all elements in the iterator `iter` as a set.
    ///
    /// Consumes the entire iterator.
    ///
    pub def toSet(iter: Iterator[a]): Set[a] & Impure with Order[a] =
        toSetHelper(iter, Set.empty())

    ///
    /// Helper function for `toSet`
    ///
    def toSetHelper(iter: Iterator[a], xs: Set[a]): Set[a] & Impure with Order[a] =
        let Iterator(done, next) = iter;
        if (done())
            xs
        else
            let x = next();
            toSetHelper(iter, Set.insert(x, xs))

    ///
    /// Returns `iter` with every occurrence of `x` replaced by `y`.
    ///
    /// Does *not* consume any elements of the iterator.
    ///
    /// The original iterator `iter` should not be reused.
    ///
    pub def replace(x: a, y: a, iter: Iterator[a]): Iterator[a] with Eq[a] =
        let Iterator(done, next) = iter;
        let n = () -> {
            let el = next();
            if (el == x) y else el
        };
        Iterator(done, n)

    ///
    /// Returns `true` if and only if at least one element in `iter` satisfies the predicate `f`.
    ///
    /// Returns `false` if `iter` is empty.
    ///
    /// The function `f` must be pure.
    ///
    /// Consumes elements of the iterator until an element that satisfies predicate `f` is found.
    ///
    pub def exists(f: a -> Bool, iter: Iterator[a]): Bool & Impure =
        let Iterator(done, next) = iter;
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
    /// Consumes the entire iterator.
    ///
    pub def forall(f: a -> Bool, iter: Iterator[a]): Bool & Impure =
        let Iterator(done, next) = iter;
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
    /// Consumes elements of the iterator until the first element that does *not* satisfy predicate `f`.
    ///
    /// The original iterator `iter` should not be reused.
    ///
    pub def dropWhile(f: a -> Bool & ef, iter: Iterator[a]): Iterator[a] & Impure =
        matchEff f {
            case Pure   => dropWhileL(f as a -> Bool, iter)
            case Impure => dropWhileE(f as a -> Bool & Impure, iter)
        }

    ///
    /// Returns `iter` without the longest prefix that satisfies the predicate `f`, where `f` is applied *lazily*.
    ///
    /// The function `f` must be pure.
    ///
    /// Does *not* consume any elements of the iterator.
    ///
    /// The original iterator `iter` should not be reused.
    ///
    def dropWhileL(f: a -> Bool, iter: Iterator[a]): Iterator[a] & Impure =
        let needsEvaluation = ref true;
        let newIter = ref iter;
        // If another function asks whether the iterator returned by dropWhileL is done, it must evaluate
        // the dropped part such that a consumer knows whether it is empty or not.
        let done = () -> {
            if (deref needsEvaluation) {
                newIter := dropWhileE(f as a -> Bool & Impure, iter);
                needsEvaluation := false
            } else ();
            let Iterator(d, _) = deref newIter;
            d()
        };
        let next = () -> {
            let Iterator(_, n) = deref newIter;
            n()
        };
        Iterator(done, next)

    ///
    /// Returns `iter` without the longest prefix that satisfies the predicate `f`, where `f` is applied *eagerly*.
    ///
    /// The function `f` must be impure.
    ///
    /// Consumes elements of the iterator until the first element that does *not* satisfy predicate `f`.
    ///
    /// The original iterator `iter` should not be reused.
    ///
    def dropWhileE(f: a ~> Bool, iter: Iterator[a]): Iterator[a] & Impure =
        let Iterator(done, next) = iter;
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
            if (f(peekedElement)) dropWhile(f, iter) else Iterator(done, n)

    ///
    /// Returns the longest prefix of `iter` that satisfies the predicate `f`.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    /// - If `f` is pure then it is applied lazily.
    /// - If `f` is impure then it is applied eagerly (consuming elements of the iterator until and including
    /// the first element that does *not* satisfy predicate `f`).
    ///
    /// The original iterator `iter` should not be reused.
    ///
    pub def takeWhile(f: a -> Bool & ef, iter: Iterator[a]): Iterator[a] & Impure =
        matchEff f {
            case Pure   => takeWhileL(f as a -> Bool, iter)
            case Impure => takeWhileE(f as a -> Bool & Impure, iter)
        }

    ///
    /// Returns the longest prefix of `iter` that satisfies the predicate `f`.
    /// `f` is applied *lazily*.
    ///
    /// Does *not* consume any elements of the iterator.
    ///
    /// The original iterator `iter` should not be reused.
    ///
    def takeWhileL(f: a -> Bool, iter: Iterator[a]): Iterator[a] & Impure =
        let Iterator(done, next) = iter;
        if (done())
            iter
        else
            let stopped = ref false;
            let hasStopped = () -> deref stopped;
            let nextElement = ref default;
            let shouldTakeElement = (x) -> {
                nextElement := x;
                let shouldTake = f(x);
                if (not shouldTake) stopped := true else ();
                shouldTake
            };
            let done1 = () -> (done() or hasStopped()) or not shouldTakeElement(next());
            let next1 = () -> deref nextElement;
            Iterator(done1, next1)

    ///
    /// Returns the longest prefix of `iter` that satisfies the predicate `f`.
    /// `f` is applied *eagerly*.
    ///
    /// Consumes elements of the iterator until and including the first element that does *not* satisfy predicate `f`.
    ///
    /// The original iterator `iter` should not be reused.
    ///
    def takeWhileE(f: a -> Bool & Impure, iter: Iterator[a]): Iterator[a] & Impure =
        let Iterator(done, _) = iter;
        if (done())
            iter
        else
            takeWhileEHelper(f, iter, Nil)

    ///
    /// Helper function for takeWhileE
    ///
    ///
    /// The original iterator `iter` should not be reused.
    ///
    def takeWhileEHelper(f: a -> Bool & Impure, iter: Iterator[a], acc: List[a]): Iterator[a] & Impure =
        let Iterator(done, next) = iter;
        if (done())
            acc |> List.reverse |> Iterator.toIter
        else
            let x = next();
            if (f(x))
                takeWhileEHelper(f, iter, x :: acc)
            else
                let d = () -> true as & Impure;
                takeWhileEHelper(f, Iterator(d, next), acc)

    ///
    /// Applies `f` to every element of `iter`.
    ///
    /// The function `f` must be impure.
    ///
    /// Consumes the entire iterator.
    ///
    pub def foreach(f: a ~> Unit, iter: Iterator[a]): Unit & Impure =
        let Iterator(done, next) = iter;
        if (not done()) {
            f(next());
            foreach(f, iter)
        }
        else ()

    ///
    /// Returns an iterator where the element at index `i` is `(a, b)` where
    /// `a` is the element at index `i` in `iterA` and `b` is the element at index `i` in `iterB`.
    ///
    /// If either `iterA` or `iterB` is depleted, then no further elements are added to the resulting iterator.
    ///
    /// Consumes elements of both `iterA` and `iterB` equal to the length of the shortest iterator.
    /// If `iterA` and `iterB` are equal length, both iterators are fully consumed.
    ///
    /// The original iterators `iterA` and `iterB` should not be reused.
    ///
    pub def zip(iterA: Iterator[a], iterB: Iterator[b]): Iterator[(a, b)] =
        let Iterator(doneA, nextA) = iterA;
        let Iterator(doneB, nextB) = iterB;
        let doneAB = () -> doneA() or doneB();
        let nextAB = () -> (nextA(), nextB());
        Iterator(doneAB, nextAB)

    ///
    /// Returns an iterator where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `iterA` and `b` is the element at index `i` in `iterB`.
    ///
    /// If either `iterA` or `iterB` becomes depleted, then no further elements are added to the resulting list.
    ///
    /// Consumes elements of both `iterA` and `iterB` equal to the length of the shortest iterator.
    /// If `iterA` and `iterB` are equal length, both iterators are fully consumed.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    /// - If `f` is pure then it is applied lazily.
    /// - If `f` is impure then it is applied eagerly.
    ///
    /// The original iterators `iterA` and `iterB` should not be reused.
    ///
    pub def zipWith(f: (a, b) -> c & ef, iterA: Iterator[a], iterB: Iterator[b]): Iterator[c] & Impure =
        matchEff f {
            case Pure   => zipWithL(f as (a, b) -> c & Pure,   iterA, iterB)
            case Impure => zipWithE(f as (a, b) -> c & Impure, iterA, iterB)
        }

    ///
    /// Returns an iterator where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `iterA` and `b` is the element at index `i` in `iterB`.
    ///
    /// Applies `f` *lazily*.
    ///
    /// If either `iterA` or `iterB` becomes depleted, then no further elements are added to the resulting list.
    ///
    /// Consumes elements of both `iterA` and `iterB` equal to the length of the shortest iterator.
    /// If `iterA` and `iterB` are equal length, both iterators are fully consumed.
    ///
    /// The original iterators `iterA` and `iterB` should not be reused.
    ///
    def zipWithL(f: (a, b) -> c, iterA: Iterator[a], iterB: Iterator[b]): Iterator[c] =
        let Iterator(doneA, nextA) = iterA;
        let Iterator(doneB, nextB) = iterB;
        let doneAB = () -> doneA() or doneB();
        let nextAB = () -> f(nextA(), nextB());
        Iterator(doneAB, nextAB)

    ///
    /// Returns an iterator where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `iterA` and `b` is the element at index `i` in `iterB`.
    ///
    /// Applies `f` *eagerly*.
    ///
    /// If either `iterA` or `iterB` becomes depleted, then no further elements are added to the resulting list.
    ///
    /// Consumes elements of both `iterA` and `iterB` equal to the length of the shortest iterator.
    /// If `iterA` and `iterB` are equal length, both iterators are fully consumed.
    ///
    /// The original iterators `iterA` and `iterB` should not be reused.
    ///
    // TODO: Optimize this (use helper function)
    def zipWithE(f: (a, b) -> c & Impure, iterA: Iterator[a], iterB: Iterator[b]): Iterator[c] & Impure =
        let listA = toList(iterA);
        let listB = toList(iterB);
        List.zipWith(f, listA, listB) |> toIter

    ///
    /// Applies `f` to a start value `s` and all elements in `iter` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def foldLeft(f: (b, a) -> b & ef, s: b, iter: Iterator[a]): b & Impure =
        let Iterator(done, next) = iter;
        if (done())
            s
        else
            foldLeft(f, f(s, next()), iter)

    ///
    /// Applies `f` to a start value `s` and all elements in `iter` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    /// Consumes the entire iterator.
    ///
    pub def foldRight(f: (a, b) -> b & ef, s: b, iter: Iterator[a]): b & Impure =
        let Iterator(done, next) = iter;
        if (done())
            s
        else
            f(next(), foldRight(f, s, iter))

    ///
    /// Optionally returns the first element of `iter` that satisfies the predicate `f` when searching from right to left.
    ///
    /// The function `f` must be pure.
    ///
    /// Consumes the entire iterator.
    ///
    pub def findRight(f: a -> Bool, iter: Iterator[a]): Option[a] & Impure =
        toList(iter) |> List.findRight(f)

    ///
    /// Returns `true` if and only if the iterator `iter` has no more elements.
    ///
    /// Does *not* consume any elements of the iterator.
    ///
    pub def isEmpty(iter: Iterator[a]): Bool & Impure =
        let Iterator(done, _) = iter;
        done()

	///
    /// Returns the number of elements in `iter` that satisfies the predicate `f`.
	///
	/// Consumes the entire iterator.
    ///
    pub def count(f: a -> Bool, iter: Iterator[a]): Int32 & Impure = countAcc(f, iter, 0)

    def countAcc(f: a -> Bool, iter: Iterator[a], acc: Int32): Int32 & Impure =
        if (isEmpty(iter))
            acc
        else if (f(next(iter)))
            countAcc(f, iter, acc + 1)
        else
            countAcc(f, iter, acc)

    ///
    /// Returns an iterator without the first `n` elements of the iterator `iter`.
    ///
	/// Consumes the first `n` elements of the iterator.
    ///
    /// The original iterator `iter` should not be reused.
    ///
    pub def drop(n: Int32, iter: Iterator[a]): Iterator[a] & Impure =
        if (n <= 0)
            iter
        else if (isEmpty(iter))
            iter
        else {
            next(iter);
            drop(n  - 1, iter)
        }

    ///
    /// Returns an iterator with the first `n` elements of the iterator `iter`.
    ///
    /// Does *not* consume any elements from the iterator.
    ///
    /// The original iterator `iter` should not be reused.
    ///
    pub def take(n: Int32, iter: Iterator[a]): Iterator[a] & Impure =
        if (n <= 0)
            Iterator(() -> true as & Impure, () -> bug!("Empty iterator") as & Impure)
        else {
            let Iterator(done, next) = iter;
            let count = ref 0;
            let done1 = () -> deref count >= n or done();
            let next1 = () -> {
                count := (deref count + 1);
                next()
            };
            Iterator(done1, next1)
        }

	///
	/// Returns an iterator with the function `f` applied to every element of
	/// the iterator `iter`.
	///
	/// Whether `f` is applied eagerly or lazily depends on its purity:
    /// - If `f` is pure then it is applied lazily.
    /// - If `f` is impure then it is applied eagerly (consuming the iterator).
    ///
    /// The original iterator `iter` should not be reused.
    ///
    pub def map(f: a -> b & ef, iter: Iterator[a]): Iterator[b] & Impure =
        matchEff f {
            case Pure   => mapL(f as a -> b, iter)
            case Impure => mapE(f as a -> b & Impure, iter)
        }

    ///
	/// Returns an iterator with the pure function `f` applied *lazily* to every
	/// element of the iterator `iter`.
	///
	/// Does *not* consume any elements from the iterator.
	///
	/// The original iterator `iter` should not be reused.
    ///
    def mapL(f: a -> b, iter: Iterator[a]): Iterator[b] =
		let Iterator(done, next) = iter;
		let next1 = () -> f(next());
		Iterator(done, next1)

	///
	/// Returns an iterator with the impure function `f` applied eagerly to
	/// every element of the iterator `iter`.
	///
	/// Consumes all elements in the iterator.
	///
	/// The original iterator `iter` should not be reused.
    ///
    def mapE(f: a -> b & Impure, iter: Iterator[a]): Iterator[b] & Impure =
		toList(iter) |> List.map(f) |> toIter

    ///
    /// Returns an iterator with every element of the iterator `iter` that
    /// satisfies the predicate `f`.
	///
	/// Whether `f` is applied eagerly or lazily depends on its purity:
	///
    /// - If `f` is pure then it is applied lazily.
    /// - If `f` is impure then it is applied eagerly (consuming the iterator).
    ///
    /// The original iterator `iter` should not be reused.
    ///
    pub def filter(f: a -> Bool & ef, iter: Iterator[a]): Iterator[a] & Impure =
        matchEff f {
            case Pure   => filterL(f as a -> Bool, iter)
            case Impure => filterE(f as a -> Bool & Impure, iter)
        }

    ///
    /// Returns an iterator with every element of the iterator `iter` that
    /// satisfies the predicate `f`.
	///
	/// Does *not* consume any elements from the iterator.
	///
	/// The original iterator `iter` should not be reused.
    ///
	pub def filterL(f: a -> Bool, iter: Iterator[a]): Iterator[a] & Impure =
        let Iterator(done, next) = iter;

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
            case None    => bug!("Empty iterator")
            case Some(x) =>
                // An element is in the reference. Use it.
                cursor := None;
                x
        };

        Iterator(done1, next1)

    ///
    /// Returns an iterator with every element of the iterator `iter` that
    /// satisfies the predicate `f`.
	///
	/// Consume the entire iterator.
	///
	/// The original iterator `iter` should not be reused.
    ///
	pub def filterE(f: a -> Bool & Impure, iter: Iterator[a]): Iterator[a] & Impure =
		(toList(iter) |> List.filter(f as a -> Bool) |> toIter) as & Impure

    ///
    /// Alias for `findLeft`.
    ///
    pub def find(f: a -> Bool & ef, iter: Iterator[a]): Option[a] & Impure = findLeft(f, iter)

    ///
    /// Optionally returns the first element of `iter` that satisfies the
    /// predicate `f` when searching from left to right.
    ///
    pub def findLeft(f: a -> Bool & ef, iter: Iterator[a]): Option[a] & Impure =
        let Iterator(done, next) = iter;
        forward(f, done, next)

	///
	/// Returns an iterator over the elements of the given list `l`.
	///
    pub def toIter(l: List[a]): Iterator[a] & Impure =
        let cursor = ref l;
        let done = () -> List.isEmpty(deref cursor);
        let next = () -> match (deref cursor) {
            case Nil     => bug!("Empty iterator.")
            case x :: xs =>
                cursor := xs;
                x
        };
        Iterator(done, next)

	///
	/// Returns all elements in the given iterator `iter` as a list.
	///
	/// Consumes the entire iterator.
	///
    pub def toList(iter: Iterator[a]): List[a] & Impure = toListAcc(iter, Nil) |> List.reverse

	def toListAcc(iter: Iterator[a], rs: List[a]): List[a] & Impure =
		if (isEmpty(iter)) rs else toListAcc(iter, next(iter) :: rs)


	///
	/// Returns the next element of the given iterator `iter`.
	///
	/// Consumes one element from the iterator.
	///
	/// Note: The iterator _must_ have a next element.
	///
    def next(iter: Iterator[a]): a & Impure = // TODO: Avoid because it is slow to destruct multiple times?
        let Iterator(_, n) = iter;
        n()

	///
	/// Fast forwards the given iterator until the next element that satisfies
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
