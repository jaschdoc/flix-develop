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


/// Represents a mutable deque.
pub enum MutDeque[a] {
    case MutDeque(Ref[Array[a]], Ref[Int32], Ref[Int32])
}

namespace MutDeque {
// TODO: Add Doc
// TODO: Add tests
// TODO: Refactor `Array.length` to use `capacity`.

    def minCapacity(): Int32 = 8

    pub def new(): MutDeque[a] & Impure = MutDeque(ref [default; minCapacity()], ref 0, ref 0)

    pub def size(d: MutDeque[a]): Int32 & Impure =
        let MutDeque(a, f, b) = d;
        computeSize(Array.length(deref a), deref f, deref b)

    ///
    /// Returns the size of a MutDeque, where `l` = array length, `f` = front index, `b` = back index.
    ///
    def computeSize(l: Int32, f: Int32, b: Int32): Int32 =
        if (f <= b)
            b - f
        else
            l - (f - b)

    pub def isEmpty(d: MutDeque[a]): Bool & Impure =
        let MutDeque(_, f, b) = d;
        let f1 = deref f;
        let b1 = deref b;
        f1 == b1

    // TODO: Collapse array if load factor < threshold
    pub def popFront(d: MutDeque[a]): a & Impure =
        let MutDeque(a, f, _) = d;
        let a1 = deref a;
        let f1 = deref f;
        let x = a1[f1];
        let c = Array.length(a1);
        f := (f1 + 1) `mod` c;
        x

    // TODO: Collapse array if load factor < threshold
    pub def popBack(d: MutDeque[a]): a & Impure =
        let MutDeque(a, _, b) = d;
        let a1 = deref a;
        let c = Array.length(a1);
        let b1 = (deref b - 1) `mod` c;
        let x = a1[b1];
        b := b1;
        x

    pub def pushFront(x: a, d: MutDeque[a]): Unit & Impure =
        let MutDeque(a, f, b) = d;
        let a1 = deref a;
        let l = Array.length(a1);
        let f1 = (deref f - 1) `mod` l;
        a1[f1] = x;
        let b1 = deref b;
        if (computeSize(l, f1, b1) >= l / 2) {
            expand!(a1, f1, b1, d)
        }
        else {
            a := a1;
            f := f1
        }

    pub def pushBack(x: a, d: MutDeque[a]): Unit & Impure =
        let MutDeque(a, f, b) = d;
        let a1 = deref a;
        let l = Array.length(a1);
        let b1 = deref b;
        a1[b1] = x;
        let b2 = (b1 + 1) `mod` l;
        let f1 = deref f;
        if (computeSize(l, f1, b2) >= l / 2) { // refactor this
            expand!(a1, f1, b2, d)
        }
        else {
            a := a1;
            b := b2 
        }
    ///
    /// Expands `d` to double its length.
    ///
    def expand!(arr: Array[a], f: Int32, b: Int32, d: MutDeque[a]): Unit & Impure =
        let MutDeque(a1, f1, b1) = d;
        let l = Array.length(arr);
        let arr1 = [default; 2 * l];
        if (f <= b)
            Array.patch!(0, b - f, arr[f .. b], arr1)
        else {
            Array.patch!(0, b,     arr[0 .. b], arr1);
            Array.patch!(b, l - f, arr[f .. l], arr1)
        };
        f1 := 0;
        b1 := computeSize(l, f, b);
        a1 := arr1


    ///
    /// Compresses MutDeque `d` if the load factor is below 1 / 2.
    ///
    def _compress!(d: MutDeque[a]): Unit & Impure =
        if (shouldBeCompressed(d)){
            shrink!(d)
        }
        else
            ()

    ///
    /// Shrinks MutDeque `d` to half its size unless it's than the minimum capacity.
    ///
    def shrink!(d: MutDeque[a]): Unit & Impure =
        let MutDeque(a, f, b) = d;
        let mc = minCapacity();
        let c = capacity(d);
        if (c > mc) {
            let a1 = deref a;
            let f1 = deref f;
            let b1 = deref b;
            let s = size(d);
            let arr = [default; c / 2];
            if (f1 < b1) {
                Array.patch!(0, b1 - f1, a1[f1 .. b1], arr)
            } else {
                Array.patch!(0,  b1,     a1[0 .. b1], arr);
                Array.patch!(b1, s - f1, a1[f1 .. s], arr)
            };
            a := arr;
            f := 0;
            b := s
        } else
            ()

        
    ///
    /// Returns `true` if the load factor is less than 1 / 2.
    ///
    def shouldBeCompressed(d: MutDeque[a]): Bool & Impure =
        let s = size(d);
        let c = capacity(d);
        let loadFactor = loadFactorOf(s, c);
        loadFactor < 1.0f32 / 4.0f32 and s > 0 // and c > minCapacity()
        

    ///
    /// Returns the load factor, given size `s` and capacity `c`.
    ///
    def loadFactorOf(s: Int32, c: Int32): Float32 =
        Int32.toFloat32(s) / Int32.toFloat32(c)

    ///
    /// Returns the capacity of `d`.
    ///
    def capacity(d: MutDeque[a]): Int32 & Impure =
        let MutDeque(a, _, _) = d;
        Array.length(deref a)


/*
    ///
    /// Compresses the given mutable list `v` if needed.
    ///
    /// The mutable list will be shrunk to 1/2 of its size if the load factor is less than 1/4.
    ///
    pub def compress!(v: mut MutList[a]): Unit & Impure =
        let c = capacity(v);
        let len = length(v);
        let loadFactor = Int32.toFloat32(len) / Int32.toFloat32(c);
        if (loadFactor < 1.0f32/4.0f32 and len > 0) {
            if (len == 1)
                shrinkTo!(1, v)
            else
                shrinkTo!(c / 2, v)
        }
        else ()

    ///
    /// Shrinks the given mutable list `v` down to a capacity of `n` elements but no less than 8.
    ///
    /// Truncates the mutable list as needed.
    ///
    def shrinkTo!(n: Int32, v: MutList[a]): Unit & Impure =
        let minCap = minCapacity();
        let capv = capacity(v);
        let MutList(a, l) = v;
        if (n < capv and capv != minCap) {
            let len = deref l;
            let newCap = Order.max(n, minCap);
            a := Array.slice(deref a, 0, newCap);
            l := Order.min(len, newCap)
        }
        else ()

*/



    ///
    /// Returns `a` mod `b`.
    ///
    def mod(a: Int32, b: Int32): Int32 =
        ((a % b) + b) % b

    ///
    /// Returns the absolute value of `x`.
    ///
    def _abs(x: Int32): Int32 =
        if (x < 0) -x else x

}