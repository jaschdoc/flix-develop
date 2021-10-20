/*
 * Copyright 2020 Esben Bjerre
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



/*

use LazyListOld.LazyList;

///
/// An experimental lazy list.
///
/// Note that both its head and tail is lazy.
///

opaque type LazyList2Old[t] = Lazy[LazyList[t]]

namespace LazyList2Old {

    ///
    /// Returns the empty LazyListOld.
    ///
    pub def empty(): LazyList2Old[a] = LazyList2Old(lazy LazyListOld.empty())

    ///
    /// Returns an infinite LazyList of repeating `x`s.
    ///
    pub def repeat(x: a): LazyList2Old[a] = LazyList2Old(lazy LazyListOld.repeat(x))

    ///
    /// Returns an infinite sequence of integers starting from and including `n`.
    ///
    pub def from(n: Int32): LazyList2Old[Int32] = LazyList2Old(lazy LazyListOld.from(n))

    ///
    /// Returns true if and only if `xs` is the empty LazyList, i.e. `Empty`.
    ///
    /// Forces the first element of the list `xs`.
    ///
    pub def isEmpty(xs: LazyList2Old[a]): Bool = {
        let LazyList2Old(ys) = xs;
        LazyListOld.isEmpty(force ys)
    }

    ///
    /// Returns the length of `xs`.
    ///
    /// Forces the entire list `xs`.
    ///
    pub def length(xs: LazyList2Old[a]): Int32 = {
        let LazyList2Old(ys) = xs;
        LazyListOld.length(force ys)
    }

    ///
    /// Returns `Some(x)` if `x` is the first element of `xs`.
    ///
    /// Returns `None` if `xs` is empty.
    ///
    /// Forces the first element of the list `xs`.
    ///
    pub def head(xs: LazyList2Old[a]): Option[a] = {
        let LazyList2Old(ys) = xs;
        LazyListOld.head(force ys)
    }

    ///
    /// Returns `Some(x)` if `x` is the last element of `xs`.
    ///
    /// Returns `None` if `xs` is empty.
    ///
    /// Forces the entire list `xs`.
    ///
    pub def last(xs: LazyList2Old[a]): Option[a] = {
        let LazyList2Old(ys) = xs;
        LazyListOld.last(force ys)
    }

    ///
    /// Returns `Some(x)` if `x` is the `n`th element of `xs`.
    ///
    /// Returns `None` if `xs` is empty or `n` >= `length(xs)`.
    ///
    /// Forces the first `n` elements of `xs`.
    ///
    pub def nth(n: Int32, xs: LazyList2Old[a]): Option[a] = {
        let LazyList2Old(ys) = xs;
        LazyListOld.nth(n, force ys)
    }

    ///
    /// Returns `ys` appended to `xs`.
    ///
    /// Forces the entire list `xs` (but only the first element of `ys`).
    ///
    pub def append(xs: LazyList2Old[a], ys: LazyList2Old[a]): LazyList2Old[a] = {
        let LazyList2Old(zs) = xs;
        let LazyList2Old(qs) = ys;
        LazyList2Old(lazy LazyListOld.append(force zs, force qs))
    }

    ///
    /// Returns the reverse of `xs`.
    ///
    /// Forces the entire list `xs`.
    ///
    pub def reverse(xs: LazyList2Old[a]): LazyList2Old[a] = {
        let LazyList2Old(ys) = xs;
        LazyList2Old(lazy LazyListOld.reverse(force ys))
    }

    ///
    /// Returns the first `n` elements of `xs`.
    ///
    /// Forces the first `n` elements of `xs`.
    ///
    pub def take(n: Int32, xs: LazyList2Old[a]): LazyList2Old[a] = {
        let LazyList2Old(ys) = xs;
        LazyList2Old(lazy LazyListOld.take(n, force ys))
    }

    ///
    /// Returns `xs` without the first `n` elements.
    ///
    /// Forces the first `n` elements of `xs`.
    ///
    pub def drop(n: Int32, xs: LazyList2Old[a]): LazyList2Old[a] = {
        let LazyList2Old(ys) = xs;
        LazyList2Old(lazy LazyListOld.drop(n, force ys))
    }

    ///
    /// Returns `x` without the first element.
    ///
    /// Forces the first element of `xs`.
    ///
    pub def tail(xs: LazyList2Old[a]): LazyList2Old[a] = drop(1, xs)

    ///
    /// Returns `xs` as a `List`.
    ///
    /// Forces the entire list `xs`.
    ///
    pub def toList(xs: LazyList2Old[a]): List[a] = {
        let LazyList2Old(ys) = xs;
        LazyListOld.toList(force ys)
    }

    ///
    /// Applies `f` to a start value `s` and all elements in `xs` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    /// Forces the entire list `xs`.
    ///
    pub def foldLeft(f: (b, a) -> b, s: b, xs: LazyList2Old[a]): b = {
        let LazyList2Old(ys) = xs;
        LazyListOld.foldLeft(f, s, force ys)
    }

    ///
    /// Applies `f` to a start value `s` and all elements in `xs` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    /// Forces the entire list `xs`.
    ///
    pub def foldRight(f: (a, Lazy[b]) -> b, s: b, xs: LazyList2Old[a]): b = {
        let LazyList2Old(ys) = xs;
        LazyListOld.foldRight(f, s, force ys)
    }

    ///
    /// Returns `true` if and only if at least one element in `xs` satisfies the predicate `f`.
    ///
    /// Returns `false` if `xs` is empty.
    ///
    /// Forces the list `xs` until an element that satisfies the predicate `f` is found.
    ///
    pub def exists(f: a -> Bool, xs: LazyList2Old[a]): Bool = {
        let LazyList2Old(ys) = xs;
        LazyListOld.exists(f, force ys)
    }

    ///
    /// Returns `true` if and only if all elements in `xs` satisfy the predicate `f`.
    ///
    /// Returns `true` if `xs` is empty.
    ///
    /// Forces the list `xs` until an element that does not satisfy the predicate `f` is found.
    ///
    pub def forall(f: a -> Bool, xs: LazyList2Old[a]): Bool = {
        let LazyList2Old(ys) = xs;
        LazyListOld.forall(f, force ys)
    }

    ///
    /// Returns the result of applying `f` to every element in `xs`.
    ///
    /// Does not force the list `xs`.
    ///
    pub def map(f: a -> b, xs: LazyList2Old[a]): LazyList2Old[b] = {
        let LazyList2Old(ys) = xs;
        LazyList2Old(lazy LazyListOld.map(f, force ys))
    }

    ///
    /// Returns a lazy list of every element in `xs` that satisfies the predicate `f`.
    ///
    /// Does not force the list `xs`.
    ///
    pub def filter(f: a -> Bool, xs: LazyList2Old[a]): LazyList2Old[a] = {
        let LazyList2Old(ys) = xs;
        LazyList2Old(lazy LazyListOld.filter(f, force ys))
    }

    ///
    /// Build a lazy list by applying `f` to the seed value `s`.
    ///
    /// `f` should return `Some(a, s1)` to signal a new list element `a` and a new seed value `s1`.
    /// `f` should return `None` to signal the end of building the list.
    ///
    pub def unfold(f: b -> Option[(a, b)], s: b): LazyList2Old[a] =
        LazyList2Old(lazy LazyListOld.unfold(f, s))

    ///
    /// Returns a list where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `xs` and `b` is the element at index `i` in `ys`.
    ///
    /// If either `xs` or `ys` becomes depleted, then no further elements are added to the resulting list.
    ///
    /// Does not force the lists `xs` and `ys`.
    ///
    pub def zipWith(f: (a, b) -> c, xs: LazyList2Old[a], ys: LazyList2Old[b]): LazyList2Old[c] = {
        let LazyList2Old(zs) = xs;
        let LazyList2Old(qs) = ys;
        LazyList2Old(lazy LazyListOld.zipWith(f, force zs, force qs))
    }

    ///
    /// Returns a list where the element at index `i` is `(a, b)` where
    /// `a` is the element at index `i` in `xs` and `b` is the element at index `i` in `ys`.
    ///
    /// If either `xs` or `ys` becomes depleted, then no further elements are added to the resulting list.
    ///
    /// Does not force the lists `xs` and `ys`.
    ///
    pub def zip(xs: LazyList2Old[a], ys: LazyList2Old[b]): LazyList2Old[(a, b)] = {
        let LazyList2Old(zs) = xs;
        let LazyList2Old(qs) = ys;
        LazyList2Old(lazy LazyListOld.zip(force zs, force qs))
    }

}
*/