/*
 * Copyright 2021 Jakob Schneider Villumsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

pub enum LazyList[a] {
    case ENil,
    case ECons(a, LazyList[a]),
    case LCons(a, Lazy[LazyList[a]]),
    case LList(Lazy[LazyList[a]])
}

namespace LazyListNew {
    // TODO: Implement functions used in TestExamples

    ///
    /// Returns an empty LazyList.
    ///
    pub def empty(): LazyList[a] = LList(lazy ENil)

    ///
    /// Returns true if and only if `l` is the empty LazyList, i.e. `ENil`.
    ///
    /// Does not force the tail of `l`.
    ///
    pub def isEmpty(l: LazyList[a]): Bool = match l {
        case ENil      => true
        case LList(ys) => isEmpty(force ys)
        case _         => false
    }

    ///
    /// Returns a lazy list of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns an empty lazy list if `b >= e`.
    ///
    pub def range(b: Int32, e: Int32): LazyList[Int32] =
        if (b >= e)
            empty()
        else
            LList(lazy LCons(b, lazy range(b + 1, e)))

    ///
    /// Returns `Some(x)` if `x` is the first element of `l`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    /// Does not force the tail of `l`.
    ///
    pub def head(l: LazyList[a]): Option[a] = match l {
        case ENil        => None
        case ECons(x, _) => Some(x)
        case LCons(x, _) => Some(x)
        case LList(ys)   => head(force ys)
    }

    ///
    /// Returns the first `n` elements of `l`.
    ///
    /// Does not force the tail of `l`.
    ///
    pub def take(n: Int32, l: LazyList[a]): LazyList[a] =
        if (n <= 0)
            empty()
        else
            match l {
                case ENil                     => ENil
                case ECons(x, _)  if (n == 1) => LCons(x, lazy ENil)
                case ECons(x, ys)             => LCons(x, lazy take(n - 1, ys))
                case LCons(x, _)  if (n == 1) => LCons(x, lazy ENil)
                case LCons(x, ys)             => LCons(x, lazy take(n - 1, force ys))
                case LList(ys)                => LList(lazy take(n, force ys))
            }

    ///
    /// Returns the length of `l`.
    ///
    /// Forces the entire list `l`.
    ///
    pub def length(l: LazyList[a]): Int32 = lengthAcc(0, l)

    ///
    /// Helper function for `length`
    ///
    def lengthAcc(acc: Int32, l: LazyList[a]): Int32 = match l {
        case ENil         => acc
        case ECons(_, ys) => lengthAcc(acc + 1, ys)
        case LCons(_, ys) => lengthAcc(acc + 1, force ys)
        case LList(ys)    => lengthAcc(acc, force ys)
    }

    ///
    /// Returns the elements of `l` in a lazy list.
    ///
    // TODO: Move to List.
    pub def toLazy(l: List[a]): LazyList[a] = match l {
        case Nil     => ENil
        case x :: xs => LCons(x, lazy toLazy(xs))
    }

    ///
    /// Returns `l` as a `List`.
    ///
    /// Forces the entire list `l`.
    ///
    pub def toList(l: LazyList[a]): List[a] = toListAcc(l, Nil) |> List.reverse

    ///
    /// Helper function for `toList`.
    ///
    def toListAcc(l: LazyList[a], acc: List[a]): List[a] = match l {
        case ENil         => acc
        case ECons(x, ys) => toListAcc(ys, x :: acc)
        case LCons(x, ys) => toListAcc(force ys, x :: acc)
        case LList(ys)    => toListAcc(force ys, acc)
    }

    ///
    /// Returns an infinite LazyList of repeating `x`s.
    ///
    pub def repeat(x: a): LazyList[a] = LList(lazy LCons(x, lazy repeat(x)))

    ///
    /// Returns an infinite sequence of integers starting from and including `n`.
    ///
    pub def from(n: Int32): LazyList[Int32] = LList(lazy LCons(n, lazy from(n + 1)))

    ///
    /// Returns `Some(x)` if `x` is the last element of `l`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    /// Forces the entire list `l`.
    ///
    pub def last(l: LazyList[a]): Option[a] = match l {
        case ENil         => None
        case ECons(x, ys) => if (isEmpty(ys))       Some(x) else last(ys)
        case LCons(x, ys) => if (isEmpty(force ys)) Some(x) else last(force ys)
        case LList(ys)    => last(force ys)
    }
    
    ///
    /// Returns a lazy list of every element in `l` that satisfies the predicate `f`.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
	///
    /// - If `f` is pure then it is applied lazily (i.e. the tail is not forced).
    /// - If `f` is impure then it is applied eagerly (i.e. the entire list `l` is forced).
    ///
    pub def filter(f: a -> Bool & ef, l: LazyList[a]): LazyList[a] & Impure =
        matchEff f {
            case Pure   => filterL(f as a -> Bool & Pure,   l)
            case Impure => filterE(f as a -> Bool & Impure, l)
        }

    ///
    /// Returns a lazy list of every element in `l` that satisfies the predicate `f`.
    ///
    /// Applies `f` lazily (i.e. the tail is not forced).
    ///
    def filterL(f: a -> Bool, l: LazyList[a]): LazyList[a] = match l {
        case ENil                   => ENil
        case ECons(x, ys) if (f(x)) => LCons(x, lazy filterL(f, ys))
        case LCons(x, ys) if (f(x)) => LCons(x, lazy filterL(f, force ys))
        case ECons(_, ys)           => LList(lazy filterL(f, ys))
        case LCons(_, ys)           => LList(lazy filterL(f, force ys))
        case LList(ys)              => LList(lazy filterL(f, force ys))
    }

    ///
    /// Returns a lazy list of every element in `l` that satisfies the predicate `f`.
    ///
    /// Applies `f` eagerly (i.e. the entire list `l` is forced).
    ///
    def filterE(f: a -> Bool & Impure, l: LazyList[a]): LazyList[a] & Impure =
        filterEAcc(f, l, Nil) |> List.reverse |> toLazy

    ///
    /// Helper function for `filterE`
    ///
    def filterEAcc(f: a -> Bool & Impure, l: LazyList[a], acc: List[a]): List[a] & Impure = match l {
        case ENil         => acc
        case ECons(x, ys) => if (f(x)) filterEAcc(f, ys, x :: acc)       else filterEAcc(f, ys, acc)
        case LCons(x, ys) => if (f(x)) filterEAcc(f, force ys, x :: acc) else filterEAcc(f, force ys, acc)
        case LList(ys)    => filterEAcc(f, force ys, acc)
    }

    ///
    /// Returns the result of applying `f` to every element in `l`.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
	///
    /// - If `f` is pure then it is applied lazily (i.e. the tail is not forced).
    /// - If `f` is impure then it is applied eagerly (i.e. the entire list `l` is forced).
    ///
    pub def map(f: a -> b & ef, l: LazyList[a]): LazyList[b] & Impure =
        matchEff f {
            case Pure   => mapL(f as a -> b & Pure,   l)
            case Impure => mapE(f as a -> b & Impure, l)
        }

    ///
    /// Returns the result of applying `f` to every element in `l`.
    ///
    /// Applies `f` lazily (i.e. the tail is not forced).
    ///
    def mapL(f: a -> b, l: LazyList[a]): LazyList[b] = match l {
        case ENil         => ENil
        case ECons(x, ys) => LCons(f(x), lazy mapL(f, ys))
        case LCons(x, ys) => LCons(f(x), lazy mapL(f, force ys))
        case LList(ys)    => LList(lazy mapL(f, force ys))
    }

    ///
    /// Returns the result of applying `f` to every element in `l`.
    ///
    /// Applies `f` eagerly (i.e. the entire list `l` is forced).
    ///
    def mapE(f: a -> b & Impure, l: LazyList[a]): LazyList[b] & Impure = toList(l) |> List.map(f) |> toLazy

    ///
    /// Returns `l` as an `Array`.
    ///
    /// Forces the entire list `l`.
    ///
    pub def toArray(l: LazyList[a]): Array[a] & Impure = ???

    ///
    /// Returns `l` as an `Array`.
    ///
    /// Does not force any elements of the list.
    ///
    pub def toIter(l: LazyList[a]): Iterator[a] & Impure = ???

    ///
    /// Returns `l` as a `Set`.
    ///
    /// Forces the entire list `l`.
    ///
    pub def toSet(l: LazyList[a]): Set[a] = ???

    ///
    /// Returns `l` as a `Map`.
    ///
    /// Forces the entire list `l`.
    ///
    pub def toMap(l: LazyList[(a, b)]): Map[a, b] = ???

}