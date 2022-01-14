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

namespace List1 {

    ///
    /// Returns the elements of `l` as a `DemandList`.
    ///
    pub def toDemandList(l: List[a]): DemandList1.DemandList[a, d] =
        use DemandList1.{DemandList};
        match l {
            case Nil     => DemandList.ENil
            case x :: xs => DemandList.LList(lazy DemandList.LCons(x, lazy toDemandList(xs)))
        }

    ///
    /// Returns an iterator over `l`.
    ///
    pub def toIterator(l: List[a]): Iterator[a] & Impure =
        let cursor = ref l;
        let done = () -> match (deref cursor) {
            case Nil => true
            case _   => false
        };
        let next = () -> match (deref cursor) {
            case Nil     => bug!("Empty Iterator!")
            case x :: xs => {
                cursor := xs;
                x
            }
        };
        Iterator(done, next)

    ///
    /// Returns the association list `xs` as a `DelayMap`.
    ///
    /// If `xs` contains multiple mappings with the same key, `toDelayMap` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    pub def toDelayMap(l: List[(a, b)]): DelayMap[a, b] with Order[a] =
        List.foldRight((x, m) -> DelayMap.insert(fst(x), snd(x), m), DelayMap.empty(), l)

    pub def take(n: Int32, l: List[a]): List[a] =
        def loop(m, xs, k) =
            if (m < 0)
                k(Nil)
            else
                match (m, xs) {
                    case (_, Nil)     => k(Nil)
                    case (0, _)       => k(Nil)
                    case (i, x :: rs) => loop(i - 1, rs, ks -> k(x :: ks))
                };
        loop(n, l, ks -> ks)

    ///
    /// Returns the reverse of `l`.
    ///
    pub def reverse(l: List[a]): List[a] =
        def loop(xs, acc) = match xs {
            case Nil     => acc
            case x :: rs => loop(rs, x :: acc)
        };
        loop(l, Nil)


    ///
    /// Returns a list of every element in `l` that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def filter(f: a -> Bool & ef, l: List[a]): List[a] & ef =
        def loop(xs, k) = match xs {
            case Nil     => k(Nil)
            case x :: rs => if (f(x)) loop(rs, ks -> k(x :: ks)) else loop(rs, k)
        };
        loop(l, ks -> ks)

    ///
    /// Applies `f` to a start value `s` and all elements in `l` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b & ef, s: b, l: List[a]): b & ef =
         def loop(g, t, xs) = match xs {
            case Nil     => t
            case x :: rs => loop(g, g(t, x), rs)
         };
         loop(f, s, l)

    ///
    /// Returns `l` without the first `n` elements.
    ///
    /// Returns `Nil` if `n > length(l)`.
    /// Returns `l` if `n < 0`.
    ///
    pub def drop(n: Int32, l: List[a]): List[a] =
        def loop(m, xs) =
            if (m <= 0)
                xs
            else
                match (m, xs) {
                    case (_, Nil)     => Nil
                    case (i, _ :: rs) => loop(i - 1, rs)
                };
        loop(n, l)

    ///
    /// Applies `f` to a start value `s` and all elements in `l` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    pub def foldRight(f: (a, b) -> b & ef, s: b, l: List[a]): b & ef =
        def loop(xs, k) = match xs {
            case Nil     => k(s)
            case x :: rs => loop(rs, ks -> k(f(x, ks)))
        };
        loop(l, ks -> ks as & ef)

    ///
    /// Returns a pair of lists, the first containing all first components in `l`
    /// and the second containing all second components in `l`.
    ///
    pub def unzip(l: List[(a, b)]): (List[a], List[b]) =
        def loop(xs, k) = match xs {
            case Nil            => k(Nil, Nil)
            case (x1, x2) :: rs => loop(rs, (ks1, ks2) -> k(x1 :: ks1, x2 :: ks2))
        };
        loop(l, (ks1, ks2) -> (ks1, ks2))

    ///
    /// Returns the product of all elements in the list `l`.
    ///
    pub def product(l: List[Int32]): Int32 =
        productWith(x -> x, l)

    ///
    /// Returns the product of all elements in the list `l` according to the function `f`.
    ///
    pub def productWith(f: a -> Int32 & ef, l: List[a]): Int32 & ef =
        if (List.isEmpty(l))
            0
        else
            def loop(l1, acc) = match l1 {
                case Nil     => acc
                case x :: xs => loop(xs, f(x) * acc)
            };
            loop(l, 1)
}
