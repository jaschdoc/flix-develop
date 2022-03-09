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

namespace TestMutList {
    use ToString.toString;
    use MutList1.sameElements;

    ///
    /// Helper function to test capacity of internal array.
    ///
    def capacity(v: MutList[a]): Int32 & Impure =
        let MutList(a, _) = v;
        Array.length(deref a)


    /////////////////////////////////////////////////////////////////////////////
    // eq                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def eq01(): Bool & Impure =
        let u: MutList[Int32] = MutList1.new();
        let v: MutList[Int32] = MutList1.new();
        u `sameElements` v

    @test
    def eq02(): Bool & Impure =
        let u = MutList1.new();
        MutList1.push!(0, u);
        let v = MutList1.new();
        (u `sameElements` v) == false

    @test
    def eq03(): Bool & Impure =
        let u = MutList1.new();
        MutList1.push!(2, u);
        MutList1.push!(3, u);
        let v = MutList1.new();
        MutList1.push!(2, v);
        MutList1.push!(3, v);
        u `sameElements` v

    @test
    def eq04(): Bool & Impure =
        let u = MutList1.new();
        MutList1.push!(2, u);
        MutList1.push!(3, u);
        let v = MutList1.new();
        MutList1.push!(2, v);
        MutList1.push!(2, v);
        (u `sameElements` v) == false

    @test
    def eq05(): Bool & Impure =
        let u = MutList1.new();
        MutList1.push!(1, u);
        let v = MutList1.new();
        (u `sameElements` v) == false

    /////////////////////////////////////////////////////////////////////////////
    // Order.compare                                                           //
    /////////////////////////////////////////////////////////////////////////////

    // TODO

    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def length01(): Bool & Impure = MutList1.length(MutList1.new()) == 0

    @test
    def length02(): Bool & Impure =
        let v = MutList1.new();
        MutList1.push!(42, v);
        MutList1.length(v) == 1

    @test
    def length03(): Bool & Impure = MutList1.length(MutList1.range(0, 2)) == 2

    @test
    def length04(): Bool & Impure = MutList1.length(MutList1.range(0, 4)) == 4

    @test
    def length05(): Bool & Impure = MutList1.length(MutList1.range(0, 42)) == 42

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isEmpty01(): Bool & Impure = MutList1.isEmpty(MutList1.new())

    @test
    def isEmpty02(): Bool & Impure = MutList1.isEmpty(MutList1.range(0, 0))

    @test
    def isEmpty03(): Bool & Impure = MutList1.isEmpty(MutList1.range(0, 1)) == false

    @test
    def isEmpty04(): Bool & Impure = MutList1.isEmpty(MutList1.range(0, 2)) == false

    @test
    def isEmpty05(): Bool & Impure = MutList1.isEmpty(MutList1.range(3, 4)) == false

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def memberOf01(): Bool & Impure = MutList1.memberOf(42, MutList1.new()) == false

    @test
    def memberOf02(): Bool & Impure =
        let v = MutList1.new();
        MutList1.push!(2, v);
        MutList1.memberOf(2, v)

    @test
    def memberOf03(): Bool & Impure =
        let v = MutList1.new();
        MutList1.push!(42, v);
        MutList1.memberOf(2, v) == false

    @test
    def memberOf04(): Bool & Impure = MutList1.memberOf(4, MutList1.range(0, 4)) == false

    @test
    def memberOf05(): Bool & Impure = MutList1.memberOf(0, MutList1.range(0, 3))

    @test
    def memberOf06(): Bool & Impure = MutList1.memberOf(2, MutList1.range(0, 3))

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def count01(): Bool & Impure = MutList1.count(x -> x > 2, MutList1.range(0, 4)) == 1

    @test
    def count02(): Bool & Impure = MutList1.count(x -> x >= 2, MutList1.range(0, 4)) == 2

    @test
    def count03(): Bool & Impure = MutList1.count(x -> x rem 2 == 0, MutList1.range(0, 10)) == 5

    @test
    def count04(): Bool & Impure = MutList1.count(x -> x == 11, MutList1.range(0, 21)) == 1


    /////////////////////////////////////////////////////////////////////////////
    // product                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def product01(): Bool & Impure =
        List.toMutList(Nil) |> MutList1.product == 0

    @test
    def product02(): Bool & Impure =
        List.toMutList(1 :: Nil) |> MutList1.product == 1

    @test
    def product03(): Bool & Impure =
        List.toMutList(1 :: 2 :: 3 :: Nil) |> MutList1.product == 6

    @test
    def product04(): Bool & Impure =
        List.toMutList(1 :: 2 :: 3 :: -3 :: Nil) |> MutList1.product == -18

    @test
    def product05(): Bool & Impure =
        List.toMutList(-1 :: -2 :: -3 :: -4 :: Nil) |> MutList1.product == 24

    @test
    def product06(): Bool & Impure =
        List.toMutList(10 :: -10 :: Nil) |> MutList1.product == -100


    /////////////////////////////////////////////////////////////////////////////
    // productWith                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productWith01(): Bool & Impure =
        List.toMutList(Nil) |> MutList1.productWith(x -> x + 1) == 0

    @test
    def productWith02(): Bool & Impure =
        List.toMutList(1 :: Nil) |> MutList1.productWith(x -> x + 1) == 2

    @test
    def productWith03(): Bool & Impure =
        List.toMutList(1 :: 2 :: 3 :: Nil) |> MutList1.productWith(x -> x + 1) == 24

    @test
    def productWith04(): Bool & Impure =
        List.toMutList(1 :: 2 :: 3 :: -3 :: Nil) |> MutList1.productWith(x -> x + 1) == -48

    @test
    def productWith05(): Bool & Impure =
        List.toMutList(-2 :: -3 :: -4 :: -5 :: Nil) |> MutList1.productWith(x -> x + 1) == 24

    @test
    def productWith06(): Bool & Impure =
        List.toMutList(10 :: -10 :: Nil) |> MutList1.productWith(x -> x + 1) == -99


    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def exists01(): Bool & Impure = MutList1.exists(x -> x > 9, MutList1.range(0, 9)) == false

    @test
    def exists02(): Bool & Impure = MutList1.exists(x -> x < 0, MutList1.range(0, 9)) == false

    @test
    def exists03(): Bool & Impure = MutList1.exists(x -> x == 8, MutList1.range(0, 9))

    @test
    def exists04(): Bool & Impure = MutList1.exists(x -> x > 0, MutList1.range(0, 9))

    @test
    def exists05(): Bool & Impure = MutList1.exists(x -> x == 0, MutList1.new()) == false

    /////////////////////////////////////////////////////////////////////////////
    // forall                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forall01(): Bool & Impure = MutList1.forall(x -> x > 0, MutList1.range(0, 9)) == false

    @test
    def forall02(): Bool & Impure = MutList1.forall(x -> x >= 0, MutList1.range(0, 9))

    @test
    def forall03(): Bool & Impure = MutList1.forall(x -> x >= 0 and x < 9, MutList1.range(0, 9))

    @test
    def forall05(): Bool & Impure = MutList1.forall(x -> x > 0, MutList1.new())

    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def head01(): Bool & Impure =
        let v = MutList1.new();
        MutList1.push!(42, v);
        MutList1.head(v) == Some(42)

    @test
    def head02(): Bool & Impure = MutList1.head(MutList1.new(): MutList[Unit]) == None

    @test
    def head03(): Bool & Impure =
        let v = MutList1.new();
        MutList1.push!(101, v);
        MutList1.push!(102, v);
        MutList1.head(v) == Some(101)

    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def last01(): Bool & Impure = MutList1.last(MutList1.new(): MutList[Unit]) == None

    @test
    def last02(): Bool & Impure = MutList1.last(MutList1.range(0, 8)) == Some(7)

    @test
    def last03(): Bool & Impure =
        let v = MutList1.new();
        MutList1.push!('A', v);
        MutList1.push!('B', v);
        MutList1.last(v) == Some('B')

    /////////////////////////////////////////////////////////////////////////////
    // indexOfLeft                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indexOfLeft01(): Bool & Impure = MutList1.indexOfLeft(2, MutList1.range(2, 3)) == Some(0)

    @test
    def indexOfLeft02(): Bool & Impure = MutList1.indexOfLeft(10, MutList1.range(0, 10)) == None

    @test
    def indexOfLeft03(): Bool & Impure =
        let v = MutList1.new();
        MutList1.push!('A', v);
        MutList1.push!('B', v);
        MutList1.push!('C', v);
        MutList1.indexOfLeft('B', v) == Some(1)

    @test
    def indexOfLeft04(): Bool & Impure = MutList1.indexOfLeft(99, MutList1.range(0, 100)) == Some(99)

    /////////////////////////////////////////////////////////////////////////////
    // indexOfRight                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indexOfRight01(): Bool & Impure = MutList1.indexOfRight(2, MutList1.range(2, 3)) == Some(0)

    @test
    def indexOfRight02(): Bool & Impure = MutList1.indexOfRight(10, MutList1.range(0, 10)) == None

    @test
    def indexOfRight03(): Bool & Impure =
        let v = MutList1.new();
        MutList1.push!('A', v);
        MutList1.push!('B', v);
        MutList1.push!('A', v);
        MutList1.indexOfRight('A', v) == Some(2)

    @test
    def indexOfRight04(): Bool & Impure =
        let v1 = MutList1.range(0, 4);
        let v2 = MutList1.range(0, 4);
        MutList1.append!(v2, v1);
        MutList1.indexOfRight(2, v1) == Some(6)

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findLeft01(): Bool & Impure = MutList1.findLeft(x -> x > 3, MutList1.range(3, 11)) == Some(4)

    @test
    def findLeft02(): Bool & Impure = MutList1.findLeft(x -> x > 10, MutList1.range(3, 11)) == None

    @test
    def findLeft03(): Bool & Impure = MutList1.findLeft(x -> x < 3, MutList1.range(3, 11)) == None

    @test
    def findLeft04(): Bool & Impure = MutList1.findLeft(x -> x rem 2 == 1, MutList1.range(3, 11)) == Some(3)

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findRight01(): Bool & Impure = MutList1.findRight(x -> x > 0, MutList1.range(2, 5)) == Some(4)

    @test
    def findRight02(): Bool & Impure = MutList1.findRight(x -> x == 10, MutList1.range(9, 10)) == None

    @test
    def findRight03(): Bool & Impure = MutList1.findRight(x -> x rem 2 == 0, MutList1.range(3, 4)) == None

    @test
    def findRight04(): Bool & Impure = MutList1.findRight(x -> x < 5, MutList1.range(3, 11)) == Some(4)

    /////////////////////////////////////////////////////////////////////////////
    // scanLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def scanLeft01(): Bool & Impure =
        let v = MutList1.scanLeft((x, y) -> x + y, 0, MutList1.new());
        MutList1.length(v) == 1 and MutList1.head(v) == Some(0)

    @test
    def scanLeft02(): Bool & Impure =
        let v1 = MutList1.range(1, 6);
        let v2 = MutList1.new();
        MutList1.push!(0, v2);
        MutList1.push!(1, v2);
        MutList1.push!(3, v2);
        MutList1.push!(6, v2);
        MutList1.push!(10, v2);
        MutList1.push!(15, v2);
        let v3 = MutList1.scanLeft((x, y) -> x + y, 0, v1);
        MutList1.sameElements(v2, v3)

    @test
    def scanLeft03(): Bool & Impure =
        let v = MutList1.scanLeft((x, y) -> x + y, 7, MutList1.range(5, 13));
        MutList1.last(v) == Some(75)

    /////////////////////////////////////////////////////////////////////////////
    // scanRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def scanRight01(): Bool & Impure =
        let v = MutList1.scanRight((x, y) -> x + y, 0, MutList1.new());
        MutList1.length(v) == 1 and MutList1.head(v) == Some(0)

    @test
    def scanRight02(): Bool & Impure =
        let v1 = MutList1.range(1, 6);
        let v2 = MutList1.new();
        MutList1.push!(15, v2);
        MutList1.push!(14, v2);
        MutList1.push!(12, v2);
        MutList1.push!(9, v2);
        MutList1.push!(5, v2);
        MutList1.push!(0, v2);
        let v3 = MutList1.scanRight((x, y) -> x + y, 0, v1);
        MutList1.sameElements(v2, v3)

    @test
    def scanRight03(): Bool & Impure =
        let v = MutList1.scanRight((x, y) -> x + y, 7, MutList1.range(5, 13));
        MutList1.last(v) == Some(7)

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map01(): Bool & Impure =
        let v = MutList1.map(x -> x * 2, MutList1.range(0, 6));
        MutList1.sameElements(v, Array.toMutList([0, 2, 4, 6, 8, 10]))

    @test
    def map02(): Bool & Impure =
        let v = MutList1.map(identity, MutList1.range(0, 10));
        MutList1.sameElements(v, MutList1.range(0, 10))

    @test
    def map03(): Bool & Impure =
        let v = MutList1.map(identity, MutList1.new(): MutList[Unit]);
        MutList1.sameElements(v, MutList1.new())

    @test
    def map04(): Bool & Impure =
        let v = MutList1.map(x -> x + 1, MutList1.range(0, 100));
        MutList1.sameElements(v, MutList1.range(1, 101))

    /////////////////////////////////////////////////////////////////////////////
    // transform!                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def transform!01(): Bool & Impure =
        let v = MutList1.range(0, 6);
        MutList1.transform!(x -> x * 2, v);
        MutList1.sameElements(v, Array.toMutList([0, 2, 4, 6, 8, 10]))

    @test
    def transform!02(): Bool & Impure =
        let v = MutList1.range(0, 10);
        MutList1.transform!(identity, v);
        MutList1.sameElements(v, MutList1.range(0, 10))

    @test
    def transform!03(): Bool & Impure =
        let v: MutList[Unit] = MutList1.new();
        MutList1.transform!(identity, v);
        MutList1.sameElements(v, MutList1.new())

    @test
    def transform!04(): Bool & Impure =
        let v = MutList1.range(0, 100);
        MutList1.transform!(x -> x + 1, v);
        MutList1.sameElements(v, MutList1.range(1, 101))

    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithIndex01(): Bool & Impure =
        let v = MutList1.mapWithIndex((_, i) -> if (i < 3) 0 else 1, MutList1.range(0, 6));
        MutList1.sameElements(v, Array.toMutList([0, 0, 0, 1, 1, 1]))

    @test
    def mapWithIndex02(): Bool & Impure =
        let v = MutList1.mapWithIndex((x, i) -> if (x == 8 and i == 1) 42 else 0, MutList1.range(7, 10));
        MutList1.sameElements(v, Array.toMutList([0, 42, 0]))

    @test
    def mapWithIndex03(): Bool & Impure =
        let v = MutList1.mapWithIndex((_, i) -> if (i == 0) 42 else 0, MutList1.new());
        MutList1.sameElements(v, MutList1.new())

    /////////////////////////////////////////////////////////////////////////////
    // transformWithIndex!                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def transformWithIndex!01(): Bool & Impure =
        let v = MutList1.range(0, 6);
        MutList1.transformWithIndex!((_, i) -> if (i < 3) 0 else 1, v);
        MutList1.sameElements(v, Array.toMutList([0, 0, 0, 1, 1, 1]))

    @test
    def transformWithIndex!02(): Bool & Impure =
        let v = MutList1.range(7, 10);
        MutList1.transformWithIndex!((x, i) -> if (x == 8 and i == 1) 42 else 0, v);
        MutList1.sameElements(v, Array.toMutList([0, 42, 0]))

    @test
    def transformWithIndex!03(): Bool & Impure =
        let v = MutList1.new();
        MutList1.transformWithIndex!((_, i) -> if (i == 0) 42 else 0, v);
        MutList1.sameElements(v, MutList1.new())

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool & Impure = MutList1.foldLeft((acc, x) -> acc + x, 0, MutList1.range(1, 5)) == 10

    @test
    def foldLeft02(): Bool & Impure =
        let v1 = MutList1.new();
        let v2 = MutList1.range(0, 10);
        MutList1.foldLeft((_, x) -> MutList1.push!(x, v1), (), v2);
        MutList1.sameElements(v1, v2)

    @test
    def foldLeft03(): Bool & Impure = MutList1.foldLeft((acc, x) -> x :: acc, Nil, MutList1.range(0, 6)) == 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil

    @test
    def foldLeft04(): Bool & Impure = MutList1.foldLeft((acc, x) -> acc - x, 0, MutList1.range(0, 100)) == -4950

    @test
    def foldLeft05(): Bool & Impure = MutList1.foldRight((acc, x) -> acc + x, 42, MutList1.new()) == 42

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool & Impure = MutList1.foldRight((x, acc) -> acc + x, 0, MutList1.range(1, 5)) == 10

    @test
    def foldRight02(): Bool & Impure = MutList1.foldRight((x, acc) -> Int32.toString(x) + acc, "", MutList1.range(3, 8)) == "34567"

    @test
    def foldRight03(): Bool & Impure = MutList1.foldRight((x, acc) -> x :: acc, Nil, MutList1.range(0, 6)) == 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil

    @test
    def foldRight04(): Bool & Impure = MutList1.foldRight((x, acc) -> x - acc, 0, MutList1.range(0, 100)) == -50

    @test
    def foldRight05(): Bool & Impure = MutList1.foldRight((x, acc) -> x + acc, 42, MutList1.new()) == 42

    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceLeft01(): Bool & Impure = MutList1.reduceLeft((x, y) -> x + y, MutList1.new(): MutList[Int32]) == None

    @test
    def reduceLeft02(): Bool & Impure = MutList1.reduceLeft((x, y) -> x + y, MutList1.range(0, 5)) == Some(10)

    @test
    def reduceLeft03(): Bool & Impure = MutList1.reduceLeft((x, y) -> x - y, MutList1.range(0, 101)) == Some(-5050)

    @test
    def reduceLeft04(): Bool & Impure = MutList1.reduceLeft((_, y) -> if (y > 4) 1 else 0, MutList1.range(0, 6)) == Some(1)

    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceRight01(): Bool & Impure = MutList1.reduceRight((x, y) -> x + y, MutList1.new(): MutList[Int32]) == None

    @test
    def reduceRight02(): Bool & Impure = MutList1.reduceRight((x, y) -> x + y, MutList1.range(0, 5)) == Some(10)

    @test
    def reduceRight03(): Bool & Impure = MutList1.reduceRight((x, y) -> x - y, MutList1.range(0, 6)) == Some(-3)

    @test
    def reduceRight04(): Bool & Impure = MutList1.reduceRight((x, _) -> if (x < 2) 1 else 0, MutList1.range(0, 6)) == Some(1)

    /////////////////////////////////////////////////////////////////////////////
    // clear!                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def clear!01(): Bool & Impure =
        let v = MutList1.range(0, 1);
        let len = MutList1.length(v);
        MutList1.clear!(v);
        len == 1 and MutList1.isEmpty(v)

    @test
    def clear!02(): Bool & Impure =
        let v = MutList1.new();
        let len = MutList1.length(v);
        MutList1.clear!(v);
        len == MutList1.length(v)

    @test
    def clear!03(): Bool & Impure =
        let v = MutList1.range(0, 100);
        MutList1.clear!(v);
        MutList1.isEmpty(v)

    /////////////////////////////////////////////////////////////////////////////
    // copy                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def copy01(): Bool & Impure =
        let v1 = MutList1.range(0, 6);
        let v2 = MutList1.copy(v1);
        v1 `sameElements` v2

    @test
    def copy02(): Bool & Impure =
        let v1 = MutList1.range(0, 100);
        let v2 = MutList1.copy(v1);
        MutList1.transform!(x -> x + 1, v2);
        MutList1.last(v1) == Some(99) and MutList1.last(v2) == Some(100)

    @test
    def copy03(): Bool & Impure =
        let v1 = MutList1.range(0, 6);
        let v2 = MutList1.copy(v1);
        MutList1.clear!(v1);
        MutList1.length(v1) == 0 and MutList1.length(v2) == 6

    @test
    def copy04(): Bool & Impure =
        let v1 = MutList1.range(0, 1000);
        let MutList(l1, n1) = v1;
        let MutList(l2, n2) = MutList1.copy(v1);
        (deref n1 == deref n2) and (deref l1 `Array.sameElements` deref l2)

    @test
    def copy05(): Bool & Impure =
        let v1 = MutList1.range(0, 3);
        let MutList(l1, n1) = v1;
        let MutList(l2, n2) = MutList1.copy(v1);
        (deref n1 == deref n2) and (deref l1 `Array.sameElements` deref l2) and
            (Array.length(deref l2) == 8)


    /////////////////////////////////////////////////////////////////////////////
    // pop                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def pop01(): Bool & Impure =
        let v = MutList1.range(0, 6);
        let len = MutList1.length(v);
        let last = MutList1.pop!(v);
        MutList1.length(v) == len - 1 and last == Some(5)

    @test
    def pop02(): Bool & Impure =
        let v: MutList[Unit] = MutList1.new();
        MutList1.pop!(v) == None

    @test
    def pop03(): Bool & Impure =
        let v = MutList1.range(1, 4);
        let three = MutList1.pop!(v);
        let two = MutList1.pop!(v);
        let one = MutList1.pop!(v);
        let none = MutList1.pop!(v);
        three == Some(3) and two == Some(2) and one == Some(1) and none == None

    /////////////////////////////////////////////////////////////////////////////
    // push                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def push01(): Bool & Impure =
        let v = MutList1.new();
        let len = MutList1.length(v);
        MutList1.push!(1, v);
        MutList1.last(v) == Some(1) and MutList1.length(v) == len + 1

    @test
    def push02(): Bool & Impure =
        let v = MutList1.new();
        let len = MutList1.length(v);
        List.foreach(x -> MutList1.push!(x, v), List.range(0, 97));
        MutList1.head(v) == Some(0) and MutList1.last(v) == Some(96) and MutList1.length(v) == len + 97

    @test
    def push03(): Bool & Impure =
        let v = MutList1.new();
        MutList1.push!(42, v);
        MutList1.push!(31, v);
        MutList1.push!(1019, v);
        MutList1.length(v) == 3 and MutList1.get(0, v) == 42 and MutList1.get(1, v) == 31 and MutList1.get(2, v) == 1019

    @test
    def push04(): Bool & Impure =
        let v = MutList1.new();
        MutList1.push!(0, v);
        MutList1.push!(0, v);
        MutList1.push!(0, v);
        MutList1.push!(0, v);
        MutList1.length(v) == 4 and MutList1.get(0, v) == 0 and MutList1.get(1, v) == 0 and MutList1.get(2, v) == 0 and MutList1.get(3, v) == 0

    /////////////////////////////////////////////////////////////////////////////
    // insert!                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def insert!01(): Bool & Impure =
        let v = MutList1.new();
        MutList1.insert!(42, 0, v);
        MutList1.length(v) == 1 and MutList1.head(v) == Some(42)

    @test
    def insert!02(): Bool & Impure =
        let v = MutList1.new();
        MutList1.insert!(107, 0, v);
        MutList1.insert!(7, 0, v);
        MutList1.length(v) == 2 and MutList1.head(v) == Some(7)

    @test
    def insert!03(): Bool & Impure =
        let v = MutList1.new();
        List.foreach(x -> MutList1.insert!(x, 0, v), List.range(0, 107));
        MutList1.length(v) == 107 and MutList1.head(v) == Some(106)

    @test
    def insert!04(): Bool & Impure =
        let v = MutList1.new();
        MutList1.insert!(42, 0, v);
        MutList1.insert!(3, 1, v);
        MutList1.insert!(5, 2, v);
        MutList1.insert!(6, 0, v);
        MutList1.length(v) == 4 and MutList1.head(v) == Some(6) and MutList1.last(v) == Some(5)

    @test
    def insert!05(): Bool & Impure =
        let v = MutList1.new();
        List.foreach(x -> MutList1.push!(x, v), List.range(97, 147));
        MutList1.insert!(42, 0, v);
        MutList1.insert!(49, 0, v);
        MutList1.length(v) == 52 and MutList1.head(v) == Some(49) and MutList1.last(v) == Some(146)

    @test
    def insert!06(): Bool & Impure =
        let v = MutList1.new();
        MutList1.insert!(42, 0, v);
        MutList1.insert!(42, 0, v);
        MutList1.insert!(42, 0, v);
        MutList1.insert!(42, 1, v);
        MutList1.insert!(97, 0, v);
        MutList1.insert!(98, 2, v);
        MutList1.length(v) == 6 and MutList1.head(v) == Some(97) and MutList1.last(v) == Some(42)

    @test
    def insert!07(): Bool & Impure =
        let v = MutList1.new();
        MutList1.insert!(97, 0, v);
        MutList1.insert!(98, 1, v);
        MutList1.insert!(99, 2, v);
        MutList1.insert!(107, 1, v);
        MutList1.length(v) == 4 and MutList1.get(1, v) == 107

    @test
    def insert!08(): Bool & Impure =
        let v = MutList1.range(0, 4);
        MutList1.insert!(42, 3, v);
        MutList1.insert!(41, 2, v);
        MutList1.insert!(40, 1, v);
        MutList1.insert!(55, 0, v);
        MutList1.insert!(33, 1, v);
        MutList1.sameElements(v, List.toMutList(55 :: 33 :: 0 :: 40 :: 1 :: 41 :: 2 :: 42 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // remove!                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def remove!01(): Bool & Impure =
        let v: MutList[Unit] = MutList1.new();
        MutList1.remove!(0, v);
        MutList1.sameElements(v, MutList1.new())

    @test
    def remove!02(): Bool & Impure =
        let v = MutList1.range(0, 4);
        MutList1.remove!(0, v);
        MutList1.sameElements(v, MutList1.range(1, 4))

    @test
    def remove!03(): Bool & Impure =
        let v = MutList1.range(0, 4);
        MutList1.remove!(3, v);
        MutList1.sameElements(v, MutList1.range(0, 3))

    @test
    def remove!04(): Bool & Impure =
        let v = MutList1.range(3, 7);
        MutList1.remove!(0, v);
        MutList1.remove!(0, v);
        MutList1.remove!(0, v);
        MutList1.sameElements(v, MutList1.range(6, 7))

    @test
    def remove!05(): Bool & Impure =
        let v = MutList1.range(9, 12);
        MutList1.remove!(2, v);
        MutList1.remove!(1, v);
        MutList1.remove!(0, v);
        MutList1.sameElements(v, MutList1.new())

    @test
    def remove!06(): Bool & Impure =
        let v = MutList1.range(7, 13);
        MutList1.remove!(0, v);
        MutList1.remove!(0, v);
        MutList1.remove!(0, v);
        MutList1.remove!(0, v);
        MutList1.remove!(0, v);
        MutList1.remove!(0, v);
        MutList1.sameElements(v, MutList1.new())

    @test
    def remove!07(): Bool & Impure =
        let v = MutList1.range(0, 3);
        MutList1.remove!(3, v);
        MutList1.remove!(7, v);
        MutList1.remove!(99, v);
        MutList1.sameElements(v, MutList1.range(0, 3))

    /////////////////////////////////////////////////////////////////////////////
    // append!                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def append!01(): Bool & Impure =
        let v1 = MutList1.range(0, 4);
        let v2 = MutList1.range(4, 8);
        MutList1.append!(v2, v1);
        MutList1.sameElements(v1, MutList1.range(0, 8))

    @test
    def append!02(): Bool & Impure =
        let v1 = MutList1.new();
        let v2 = MutList1.range(1, 4);
        MutList1.append!(v2, v1);
        MutList1.sameElements(v1, MutList1.range(1, 4))

    @test
    def append!03(): Bool & Impure =
        let v1 = MutList1.range(9, 24);
        let v2 = MutList1.new();
        MutList1.append!(v2, v1);
        MutList1.sameElements(v1, MutList1.range(9, 24))

    @test
    def append!04(): Bool & Impure =
        let v1: MutList[Unit] = MutList1.new();
        let v2: MutList[Unit] = MutList1.new();
        MutList1.append!(v2, v1);
        MutList1.sameElements(v1, MutList1.new())

    @test
    def append!05(): Bool & Impure =
        let v1 = MutList1.range(1, 3);
        let v2 = MutList1.range(3, 5);
        let v3 = MutList1.range(5, 99);
        MutList1.append!(v2, v1);
        MutList1.append!(v3, v1);
        MutList1.sameElements(v1, MutList1.range(1, 99))

    /////////////////////////////////////////////////////////////////////////////
    // retain!                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def retain!01(): Bool & Impure =
        let v = MutList1.range(0, 10);
        MutList1.retain!(x -> x rem 2 == 0, v);
        MutList1.sameElements(List.toMutList(0 :: 2 :: 4 :: 6 :: 8 :: Nil), v)

    @test
    def retain!02(): Bool & Impure =
        let v = MutList1.range(0, 10);
        MutList1.retain!(x -> x rem 2 == 1, v);
        MutList1.sameElements(List.toMutList(1 :: 3 :: 5 :: 7 :: 9 :: Nil), v)

    @test
    def retain!03(): Bool & Impure =
        let v = MutList1.range(0, 10);
        MutList1.retain!(x -> x > 5, v);
        MutList1.sameElements(v, MutList1.range(6, 10))

    @test
    def retain!04(): Bool & Impure =
        let v = MutList1.range(0, 10);
        MutList1.retain!(x -> x > 10, v);
        MutList1.sameElements(v, MutList1.new())

    /////////////////////////////////////////////////////////////////////////////
    // replace!                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def replace!01(): Bool & Impure =
        let v = MutList1.new();
        MutList1.push!(1, v);
        MutList1.push!(2, v);
        MutList1.push!(2, v);
        MutList1.push!(1, v);
        MutList1.push!(1, v);
        MutList1.replace!(from = 1, to = 42, v);
        MutList1.sameElements(List.toMutList(42 :: 2 :: 2 :: 42 :: 42 :: Nil), v)

    @test
    def replace!02(): Bool & Impure =
        let v = MutList1.range(4, 7);
        MutList1.replace!(from = 4, to = 0, v);
        MutList1.sameElements(List.toMutList(0 :: 5 :: 6 :: Nil), v)

    @test
    def replace!03(): Bool & Impure =
        let v = MutList1.range(4, 7);
        MutList1.replace!(from = 5, to = 0, v);
        MutList1.sameElements(List.toMutList(4 :: 0 :: 6 :: Nil), v)

    @test
    def replace!04(): Bool & Impure =
        let v = MutList1.range(4, 7);
        MutList1.replace!(from = 6, to = 0, v);
        MutList1.sameElements(List.toMutList(4 :: 5 :: 0 :: Nil), v)

    @test
    def replace!05(): Bool & Impure =
        let v = List.toMutList(List.repeat(97, 'a'));
        MutList1.replace!(from = 'a', to = 'z', v);
        MutList1.sameElements(List.toMutList(List.repeat(97, 'z')), v)

    /////////////////////////////////////////////////////////////////////////////
    // reverse!                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reverse01!(): Bool & Impure =
        let v: MutList[Unit] = MutList1.new();
        MutList1.reverse!(v);
        MutList1.sameElements(v, MutList1.new())

    @test
    def reverse02!(): Bool & Impure =
        let v = MutList1.range(0, 1);
        MutList1.reverse!(v);
        MutList1.sameElements(v, MutList1.range(0, 1))

    @test
    def reverse03!(): Bool & Impure =
        let v = MutList1.range(0, 5);
        MutList1.reverse!(v);
        MutList1.sameElements(v, List.toMutList(4 :: 3 :: 2 :: 1 :: 0 :: Nil))

    @test
    def reverse04!(): Bool & Impure =
        let v = MutList1.range(0, 6);
        MutList1.reverse!(v);
        MutList1.sameElements(v, List.toMutList(5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil))

    @test
    def reverse05!(): Bool & Impure =
        let v = List.toMutList(7 :: 6 :: 5 :: 4 :: 3 :: 2 :: Nil);
        MutList1.reverse!(v);
        MutList1.sameElements(v, MutList1.range(2, 8))


    /////////////////////////////////////////////////////////////////////////////
    // shrink!                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def shrink!01(): Bool & Impure =
        let v = MutList1.new();
        MutList1.push!(0, v);
        MutList1.push!(1, v);
        MutList1.push!(2, v);
        MutList1.shrink!(v);
        MutList1.length(v) == 3

    @test
    def shrink!02(): Bool & Impure =
        let v = MutList1.new();
        MutList1.shrink!(v);
        MutList1.length(v) == 0

    @test
    def shrink!03(): Bool & Impure =
        let v = MutList1.range(0, 9);
        MutList1.push!(9, v);
        MutList1.shrink!(v);
        MutList1.length(v) == 10

    @test
    def shrink!04(): Bool & Impure =
        let v = MutList1.new();
        MutList1.push!(4, v);
        MutList1.append!(MutList1.range(0, 4), v);
        MutList1.push!(5, v);
        MutList1.shrink!(v);
        MutList1.length(v) == 6

    /////////////////////////////////////////////////////////////////////////////
    // truncate!                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def truncate!01(): Bool & Impure =
        let v: MutList[Unit] = MutList1.new();
        MutList1.truncate!(0, v);
        MutList1.sameElements(v, MutList1.new())

    @test
    def truncate!02(): Bool & Impure =
        let v = MutList1.range(0, 10);
        MutList1.truncate!(5, v);
        MutList1.sameElements(v, MutList1.range(0, 5))

    @test
    def truncate!03(): Bool & Impure =
        let v = MutList1.range(0, 99);
        MutList1.truncate!(-1, v);
        MutList1.sameElements(v, MutList1.new())

    @test
    def truncate!04(): Bool & Impure =
        let v = MutList1.range(7, 8);
        MutList1.truncate!(2, v);
        MutList1.sameElements(v, MutList1.range(7, 8))

    @test
    def truncate!05(): Bool & Impure =
        let v = MutList1.range(99, 102);
        MutList1.truncate!(1, v);
        MutList1.sameElements(v, MutList1.range(99, 100))

    /////////////////////////////////////////////////////////////////////////////
    // reserve!                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reserve!01(): Bool & Impure =
        let v = MutList1.new();
        MutList1.reserve!(4, v);
        capacity(v) - MutList1.length(v) >= 4

    @test
    def reserve!02(): Bool & Impure =
        let v = MutList1.range(0, 5);
        MutList1.reserve!(5, v);
        capacity(v) - MutList1.length(v) >= 5

    @test
    def reserve!03(): Bool & Impure =
        let v = MutList1.range(0, 99);
        MutList1.reserve!(1, v);
        capacity(v) - MutList1.length(v) >= 1

    @test
    def reserve!04(): Bool & Impure =
        let v = MutList1.range(0, 100);
        MutList1.reserve!(100, v);
        capacity(v) - MutList1.length(v) >= 100

    @test
    def reserve!05(): Bool & Impure =
        let v = MutList1.range(0, 5);
        MutList1.reserve!(MutList1.length(v), v);
        capacity(v) - MutList1.length(v) >= MutList1.length(v)

    @test
    def reserve!06(): Bool & Impure =
        let n = 197;
        let v = MutList1.range(0, n);
        MutList1.reserve!(1, v);
        MutList1.reserve!(1, v);
        MutList1.reserve!(1, v);
        MutList1.reserve!(1, v);
        MutList1.reserve!(1, v);
        capacity(v) - MutList1.length(v) >= 1

    /////////////////////////////////////////////////////////////////////////////
    // formatWith                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def formatWith01(): Bool & Impure =
        let v = MutList1.range(0, 5);
        MutList1.formatWith(Int32.toString, ", ", v) == "0, 1, 2, 3, 4"

    @test
    def formatWith02(): Bool & Impure =
        let v = MutList1.range(1, 4);
        MutList1.formatWith(Int32.toString, "+", v) == "1+2+3"

    @test
    def formatWith03(): Bool & Impure =
        let v = MutList1.range(2, 3);
        MutList1.formatWith(Int32.toString, "/", v) == "2"

    @test
    def formatWith04(): Bool & Impure =
        let v = MutList1.range(6, 9);
        MutList1.formatWith(Int32.toString, "/", v) == "6/7/8"

    /////////////////////////////////////////////////////////////////////////////
    // foreach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foreach01(): Bool & Impure =
        let v = MutList1.new();
        let sb = StringBuilder.new();
        let f = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MutList1.foreach(f, v);
        StringBuilder.toString(sb) == ""

    @test
    def foreach02(): Bool & Impure =
        let v = MutList1.range(0, 5);
        let sb = StringBuilder.new();
        let f = x -> if (x rem 2 == 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MutList1.foreach(f, v);
        StringBuilder.toString(sb) == "TFTFT"

    @test
    def foreach03(): Bool & Impure =
        let v = MutList1.range(0, 2);
        let sb = StringBuilder.new();
        let f = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MutList1.foreach(f, v);
        StringBuilder.toString(sb) == "FT"

    @test
    def foreach04(): Bool & Impure =
        let v = MutList1.range(0, 2);
        let sb = StringBuilder.new();
        let f = x -> if (x < 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MutList1.foreach(f, v);
        StringBuilder.toString(sb) == "FF"

    /////////////////////////////////////////////////////////////////////////////
    // foreachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foreachWithIndex01(): Bool & Impure =
        let v1 = MutList1.range(0, 100);
        let v2 = MutList1.new();
        MutList1.foreachWithIndex((x, i) -> if (x rem 2 == 0 and i < 10) MutList1.push!(x, v2) else (), v1);
        MutList1.sameElements(v2, List.toMutList(0 :: 2 :: 4 :: 6 :: 8 :: Nil))

    @test
    def foreachWithIndex02(): Bool & Impure =
        let v = MutList1.range(1, 4);
        let sb = StringBuilder.new();
        let f = (x, i) -> if (x rem 2 == 1 and i == 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MutList1.foreachWithIndex(f, v);
        StringBuilder.toString(sb) == "TFF"

    @test
    def foreachWithIndex03(): Bool & Impure =
        let v = MutList1.range(0, 5);
        let sb = StringBuilder.new();
        let f = (x, _) -> if (x rem 2 == 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MutList1.foreachWithIndex(f, v);
        StringBuilder.toString(sb) == "TFTFT"

    /////////////////////////////////////////////////////////////////////////////
    // compress!                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def compress!01(): Bool & Impure =
        // Pop 76 element such that the load factor is strictly lower than 1 / 4.
        let v = MutList1.range(0, 100);
        let c = capacity(v);
        let i = ref 0;
        let f = _ -> {
            if (deref i < 76) {
                MutList1.pop!(v);
                i := deref i + 1
            }
            else ()
        };
        MutList1.foreach(f, v);
        capacity(v) == c / 2 and
            MutList1.length(v) == 100 - 75 - 1

    @test
    def compress!02(): Bool & Impure =
        let v = MutList1.new();
        MutList1.compress!(v);
        capacity(v) == capacity(MutList1.new())

    @test
    def compress!03(): Bool & Impure =
        let v = MutList1.new();
        let c = capacity(v);
        MutList1.push!(42, v);
        MutList1.compress!(v);
        capacity(v) <= c and
        MutList1.length(v) == 1

    @test
    def compress!04(): Bool & Impure =
        let v = MutList1.range(0, 5);
        MutList1.compress!(v);
        capacity(v) <= capacity(MutList1.range(0, 5))

    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def minimumBy01(): Bool & Impure = MutList1.minimumBy((x, y) -> x <=> y, MutList1.new(): MutList[Int32]) == None

    @test
    def minimumBy02(): Bool & Impure = MutList1.minimumBy((x, y) -> x <=> y, MutList1.range(0, 5)) == Some(0)

    @test
    def minimumBy03(): Bool & Impure = MutList1.minimumBy((x, y) -> x <=> y, MutList1.range(5, 10)) == Some(5)

    @test
    def minimumBy04(): Bool & Impure =
        let v = MutList1.range(0, 6);
        MutList1.push!(-2, v);
        MutList1.minimumBy((x, y) -> x <=> y, v) == Some(-2)

    @test
    def minimumBy05(): Bool & Impure =
        let v = MutList1.range(9, 19);
        let cmp = (x, y) -> if (x < y and x rem 2 == 0) LessThan else GreaterThan;
        MutList1.minimumBy(cmp, v) == Some(10)

    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def maximumBy01(): Bool & Impure = MutList1.maximumBy((x, y) -> x <=> y, MutList1.new(): MutList[Int32]) == None

    @test
    def maximumBy02(): Bool & Impure = MutList1.maximumBy((x, y) -> x <=> y, MutList1.range(0, 5)) == Some(4)

    @test
    def maximumBy03(): Bool & Impure = MutList1.maximumBy((x, y) -> x <=> y, MutList1.range(5, 10)) == Some(9)

    @test
    def maximumBy04(): Bool & Impure =
        let v = MutList1.range(0, 6);
        MutList1.push!(11, v);
        MutList1.maximumBy((x, y) -> x <=> y, v) == Some(11)

    @test
    def maximumBy05(): Bool & Impure =
        let v = MutList1.range(9, 19);
        let cmp = (x, y) -> if (x > y and x rem 2 == 0) GreaterThan else LessThan;
        MutList1.maximumBy(cmp, v) == Some(18)

    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toArray01(): Bool & Impure =
        Array.sameElements(MutList1.toArray(Array.toMutList([1, 2, 3])), [1, 2, 3])

    @test
    def toArray02(): Bool & Impure =
        let v = Array.toMutList([1, 2, 3]);
        let a = MutList1.toArray(v);
        a[1] = 42;
        MutList1.get(1, v) == 2

    @test
    def toArray03(): Bool & Impure =
        let v = Array.toMutList([1, 2, 3]);
        MutList1.pop!(v);
        MutList1.pop!(v);
        Array.length(MutList1.toArray(v)) == 1

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toList01(): Bool & Impure =
        MutList1.new(): MutList[Int32] |> MutList1.toList == Nil: List[Int32]

    @test
    def toList02(): Bool & Impure =
        let ml = MutList1.range(0, 1000);
        ml |> MutList1.toList == List.range(0, 1000) and
            not (MutList1.isEmpty(ml))

    @test
    def toList03(): Bool & Impure =
        let ml = MutList1.range(0, 1000);
        ml |> MutList1.toList == List.range(0, 1000) and
           ml `sameElements` MutList1.range(0, 1000)

    /////////////////////////////////////////////////////////////////////////////
    // toChain                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toChain01(): Bool & Impure =
        MutList1.toChain(List.toMutList(Nil): MutList[Int32]) == Chain.empty(): Chain[Int32]

    @test
    def toChain02(): Bool & Impure =
        MutList1.toChain(List.toMutList(1 :: Nil)) == Chain.singleton(1)

    @test
    def toChain03(): Bool & Impure =
        MutList1.toChain(List.toMutList(1 :: 2 :: Nil)) == List.toChain(1 :: 2 :: Nil)

    @test
    def toChain04(): Bool & Impure =
        MutList1.toChain(List.toMutList(1 :: 2 :: 3 :: Nil)) == List.toChain(1 :: 2 :: 3 :: Nil)



/////////////////////////////////////////////////////////////////////////////
// The following tests for sort functions correspond to those in TestArray //
/////////////////////////////////////////////////////////////////////////////
// sortWith                                                                //
/////////////////////////////////////////////////////////////////////////////

def cmp(x: Int32, y: Int32): Comparison =
    if (x < y) LessThan
    else if (x == y) EqualTo
    else GreaterThan

@test
def sortWith01(): Bool & Impure =
    let a = MutList1.sortWith(cmp, Array.toMutList([]: Array[Int32]));
    a `sameElements` Array.toMutList([]: Array[Int32])

@test
def sortWith02(): Bool & Impure =
    let a = MutList1.sortWith(cmp, Array.toMutList([0]));
    a `sameElements` Array.toMutList([0])

@test
def sortWith03(): Bool & Impure =
    let a = MutList1.sortWith(cmp, Array.toMutList([0,1]));
    a `sameElements` Array.toMutList([0,1])

@test
def sortWith04(): Bool & Impure =
    let a = MutList1.sortWith(cmp, Array.toMutList([1,0]));
    a `sameElements` Array.toMutList([0,1])

@test
def sortWith05(): Bool & Impure =
    let a = MutList1.sortWith(cmp, Array.toMutList([1,1]));
    a `sameElements` Array.toMutList([1,1])

@test
def sortWith06(): Bool & Impure =
    let a = MutList1.sortWith(cmp, Array.toMutList([0,1,2,3,4,5]));
    a `sameElements` Array.toMutList([0,1,2,3,4,5])

@test
def sortWith07(): Bool & Impure =
    let a = MutList1.sortWith(cmp, Array.toMutList([5,4,3,2,1,0]));
    a `sameElements` Array.toMutList([0,1,2,3,4,5])

@test
def sortWith08(): Bool & Impure =
    let a = MutList1.sortWith(cmp, Array.toMutList([5,3,0,4,1,2]));
    a `sameElements` Array.toMutList([0,1,2,3,4,5])

@test
def sortWith09(): Bool & Impure =
    let a = MutList1.sortWith(cmp, Array.toMutList([2,3,0,4,1,2]));
    a `sameElements` Array.toMutList([0,1,2,2,3,4])

@test
def sortWith10(): Bool & Impure =
    let a = MutList1.sortWith(flip(cmp), Array.toMutList([0,1,2,3,4,5]));
    a `sameElements` Array.toMutList([5,4,3,2,1,0])

@test
def sortWith11(): Bool & Impure =
    let a = MutList1.sortWith(flip(cmp), Array.toMutList([5,4,3,2,1,0]));
    a `sameElements` Array.toMutList([5,4,3,2,1,0])

@test
def sortWith12(): Bool & Impure =
    let a = MutList1.sortWith(flip(cmp), Array.toMutList([5,3,0,4,1,2]));
    a `sameElements` Array.toMutList([5,4,3,2,1,0])

@test
def sortWith13(): Bool & Impure =
    let a = MutList1.sortWith(flip(cmp), Array.toMutList([2,3,0,4,1,2]));
    a `sameElements` Array.toMutList([4,3,2,2,1,0])

/////////////////////////////////////////////////////////////////////////////
// sort                                                                    //
/////////////////////////////////////////////////////////////////////////////

def testSortVsSortWith(a: MutList[Int32]) : Bool & Impure =
    MutList1.sort(a) `sameElements` MutList1.sortWith(cmp, a)

@test
def sort01(): Bool & Impure = testSortVsSortWith(Array.toMutList([]: Array[Int32]))

@test
def sort02(): Bool & Impure = testSortVsSortWith(Array.toMutList([0]))

@test
def sort03(): Bool & Impure = testSortVsSortWith(Array.toMutList([0,1]))

@test
def sort04(): Bool & Impure = testSortVsSortWith(Array.toMutList([1,0]))

@test
def sort05(): Bool & Impure = testSortVsSortWith(Array.toMutList([1,1]))

@test
def sort06(): Bool & Impure = testSortVsSortWith(Array.toMutList([0,1,2,3,4,5]))

@test
def sort07(): Bool & Impure = testSortVsSortWith(Array.toMutList([5,4,3,2,1,0]))

@test
def sort08(): Bool & Impure = testSortVsSortWith(Array.toMutList([5,3,0,4,1,2]))

@test
def sort09(): Bool & Impure = testSortVsSortWith(Array.toMutList([2,3,0,4,1,2]))

/////////////////////////////////////////////////////////////////////////////
// sortWith!                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def sortWith01!(): Bool & Impure =
    let a = Array.toMutList([]: Array[Int32]);
    MutList1.sortWith(cmp, a);
    a `sameElements` Array.toMutList([]: Array[Int32])

@test
def sortWith02!(): Bool & Impure =
    let a = Array.toMutList([0]);
    MutList1.sortWith!(cmp, a);
    a `sameElements` Array.toMutList([0])

@test
def sortWith03!(): Bool & Impure =
    let a = Array.toMutList([0,1]);
    MutList1.sortWith!(cmp, a);
    a `sameElements` Array.toMutList([0,1])

@test
def sortWith04!(): Bool & Impure =
    let a = Array.toMutList([1,0]);
    MutList1.sortWith!(cmp, a);
    a `sameElements` Array.toMutList([0,1])

@test
def sortWith05!(): Bool & Impure =
    let a = Array.toMutList([1,1]);
    MutList1.sortWith!(cmp, a);
    a `sameElements` Array.toMutList([1,1])

@test
def sortWith06!(): Bool & Impure =
    let a = Array.toMutList([0,1,2,3,4,5]);
    MutList1.sortWith!(cmp, a);
    a `sameElements` Array.toMutList([0,1,2,3,4,5])

@test
def sortWith07!(): Bool & Impure =
    let a = Array.toMutList([5,4,3,2,1,0]);
    MutList1.sortWith!(cmp, a);
    a `sameElements` Array.toMutList([0,1,2,3,4,5])

@test
def sortWith08!(): Bool & Impure =
    let a = Array.toMutList([5,3,0,4,1,2]);
    MutList1.sortWith!(cmp, a);
    a `sameElements` Array.toMutList([0,1,2,3,4,5])

@test
def sortWith09!(): Bool & Impure =
    let a = Array.toMutList([2,3,0,4,1,2]);
    MutList1.sortWith!(cmp, a);
    a `sameElements` Array.toMutList([0,1,2,2,3,4])

@test
def sortWith10!(): Bool & Impure =
    let a = Array.toMutList([0,1,2,3,4,5]);
    MutList1.sortWith!(flip(cmp), a);
    a `sameElements` Array.toMutList([5,4,3,2,1,0])

@test
def sortWith11!(): Bool & Impure =
    let a = Array.toMutList([5,4,3,2,1,0]);
    MutList1.sortWith!(flip(cmp), a);
    a `sameElements` Array.toMutList([5,4,3,2,1,0])

@test
def sortWith12!(): Bool & Impure =
    let a = Array.toMutList([5,3,0,4,1,2]);
    MutList1.sortWith!(flip(cmp), a);
    a `sameElements` Array.toMutList([5,4,3,2,1,0])

@test
def sortWith13!(): Bool & Impure =
    let a = Array.toMutList([2,3,0,4,1,2]);
    MutList1.sortWith!(flip(cmp), a);
    a `sameElements` Array.toMutList([4,3,2,2,1,0])

/////////////////////////////////////////////////////////////////////////////
// sort!                                                                   //
/////////////////////////////////////////////////////////////////////////////

def testSort!VsSortWith!(a: MutList[Int32]) : Bool & Impure =
    let b = MutList1.copy(a);
    let c = MutList1.copy(a);
    MutList1.sort!(b);
    MutList1.sortWith!(cmp, c);
    b `sameElements` c

@test
def sort!01(): Bool & Impure = testSort!VsSortWith!(Array.toMutList([]: Array[Int32]))

@test
def sort!02(): Bool & Impure = testSort!VsSortWith!(Array.toMutList([0]))

@test
def sort!03(): Bool & Impure = testSort!VsSortWith!(Array.toMutList([0,1]))

@test
def sort!04(): Bool & Impure = testSort!VsSortWith!(Array.toMutList([1,0]))

@test
def sort!05(): Bool & Impure = testSort!VsSortWith!(Array.toMutList([1,1]))

@test
def sort!06(): Bool & Impure = testSort!VsSortWith!(Array.toMutList([0,1,2,3,4,5]))

@test
def sort!07(): Bool & Impure = testSort!VsSortWith!(Array.toMutList([5,4,3,2,1,0]))

@test
def sort!08(): Bool & Impure = testSort!VsSortWith!(Array.toMutList([5,3,0,4,1,2]))

@test
def sort!09(): Bool & Impure = testSort!VsSortWith!(Array.toMutList([2,3,0,4,1,2]))

/////////////////////////////////////////////////////////////////////////////
// sortBy                                                                  //
/////////////////////////////////////////////////////////////////////////////

def testSortByVsSort(a: MutList[Int32]) : Bool & Impure =
    (MutList1.sortBy(identity, a) `sameElements` MutList1.sort(a)) and
    (MutList1.sortBy(x -> 4*x+7, a) `sameElements` MutList1.sort(a)) and
    (MutList1.sortBy(x -> -x, a) `sameElements` MutList1.sortWith(flip(cmp),a))

@test
def sortBy01(): Bool & Impure = testSortByVsSort(Array.toMutList([]: Array[Int32]))

@test
def sortBy02(): Bool & Impure = testSortByVsSort(Array.toMutList([0]))

@test
def sortBy03(): Bool & Impure = testSortByVsSort(Array.toMutList([0,1]))

@test
def sortBy04(): Bool & Impure = testSortByVsSort(Array.toMutList([1,0]))

@test
def sortBy05(): Bool & Impure = testSortByVsSort(Array.toMutList([1,1]))

@test
def sortBy06(): Bool & Impure = testSortByVsSort(Array.toMutList([0,1,2,3,4,5]))

@test
def sortBy07(): Bool & Impure = testSortByVsSort(Array.toMutList([5,4,3,2,1,0]))

@test
def sortBy08(): Bool & Impure = testSortByVsSort(Array.toMutList([5,3,0,4,1,2]))

@test
def sortBy09(): Bool & Impure = testSortByVsSort(Array.toMutList([2,3,0,4,1,2]))

enum R {
    case R({i :: Int32, s :: String})
}

instance Eq[R] {
    pub def eq(a: R, b: R): Bool =
        let R(x) = a;
        let R(y) = b;
        x.i == y.i and x.s == y.s
}

@test
def sortBy10(): Bool & Impure =
    MutList1.sortBy(r -> let R(x) = r; x.i, Array.toMutList([R({i = 2, s = "A"}), R({i = 1, s = "B"}), R({i = 3, s = "C"})]))
    `sameElements` Array.toMutList([R({i = 1, s = "B"}), R({i = 2, s = "A"}), R({i = 3, s = "C"})])

/////////////////////////////////////////////////////////////////////////////
// sortBy!                                                                 //
/////////////////////////////////////////////////////////////////////////////

def testSortBy!VsSortBy(a: MutList[Int32]) : Bool & Impure =
    let b = MutList1.copy(a);
    let c = MutList1.copy(a);
    MutList1.sortBy!(identity, b);
    MutList1.sortBy!(x -> 4*x+7, c);
    (b `sameElements` MutList1.sortBy(x -> 4*x+7, a)) and
    (c `sameElements` MutList1.sortBy(identity, a))

@test
def sortBy!01(): Bool & Impure = testSortBy!VsSortBy(Array.toMutList([]: Array[Int32]))

@test
def sortBy!02(): Bool & Impure = testSortBy!VsSortBy(Array.toMutList([0]))

@test
def sortBy!03(): Bool & Impure = testSortBy!VsSortBy(Array.toMutList([0,1]))

@test
def sortBy!04(): Bool & Impure = testSortBy!VsSortBy(Array.toMutList([1,0]))

@test
def sortBy!05(): Bool & Impure = testSortBy!VsSortBy(Array.toMutList([1,1]))

@test
def sortBy!06(): Bool & Impure = testSortBy!VsSortBy(Array.toMutList([0,1,2,3,4,5]))

@test
def sortBy!07(): Bool & Impure = testSortBy!VsSortBy(Array.toMutList([5,4,3,2,1,0]))

@test
def sortBy!08(): Bool & Impure = testSortBy!VsSortBy(Array.toMutList([5,3,0,4,1,2]))

@test
def sortBy!09(): Bool & Impure = testSortBy!VsSortBy(Array.toMutList([2,3,0,4,1,2]))

    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def range01(): Bool & Impure =
        let v1 = MutList1.range(1, 4) |> MutList1.toArray;
        let v2 = MutList1.range(0, 5) |> MutList1.toArray;
        let arr1 = [1, 2, 3];
        let arr2 = [0, 1, 2, 3, 4];
        (v1 `Array.sameElements` arr1) and (v2 `Array.sameElements` arr2)

    @test
    def range02(): Bool & Impure =
        let v = MutList1.range(-1, 3) |> MutList1.toArray;
        let arr = [-1, 0, 1, 2];
        (v `Array.sameElements` arr)

    @test
    def range03(): Bool & Impure =
        MutList1.range(0, 1000) |> MutList1.toList == List.range(0, 1000)


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sum01(): Bool & Impure =
        List.toMutList(Nil) |> MutList1.sum == 0

    @test
    def sum02(): Bool & Impure =
        List.toMutList(1 :: Nil) |> MutList1.sum == 1

    @test
    def sum03(): Bool & Impure =
        List.toMutList(1 :: 2 :: 3 :: Nil) |> MutList1.sum == 6

    @test
    def sum04(): Bool & Impure =
        List.toMutList(1 :: 2 :: 3 :: -3 :: Nil) |> MutList1.sum == 3

    @test
    def sum05(): Bool & Impure =
        List.toMutList(-1 :: -2 :: -3 :: -4 :: Nil) |> MutList1.sum == -10

    @test
    def sum06(): Bool & Impure =
        List.toMutList(10 :: -10 :: Nil) |> MutList1.sum == 0

    @test
    def sum07(): Bool & Impure =
        List.range(1, 101) |> List.toMutList |> MutList1.sum == 5050


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumWith01(): Bool & Impure =
        List.toMutList(Nil) |> MutList1.sumWith(x -> x + 1) == 0

    @test
    def sumWith02(): Bool & Impure =
        List.toMutList(1 :: Nil) |> MutList1.sumWith(x -> x + 1) == 2

    @test
    def sumWith03(): Bool & Impure =
        List.toMutList(1 :: 2 :: 3 :: Nil) |> MutList1.sumWith(x -> x + 1) == 9

    @test
    def sumWith04(): Bool & Impure =
        List.toMutList(1 :: 2 :: 3 :: -3 :: Nil) |> MutList1.sumWith(x -> x + 1) == 7

    @test
    def sumWith05(): Bool & Impure =
        List.toMutList(-1 :: -2 :: -3 :: -4 :: Nil) |> MutList1.sumWith(x -> x + 1) == -6

    @test
    def sumWith06(): Bool & Impure =
        List.toMutList(10 :: -10 :: Nil) |> MutList1.sumWith(x -> x + 1) == 2


    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def join01(): Bool & Impure =
        (Nil: List[Int32]) |> List.toMutList |>
            MutList1.join(",") == ""

    @test
    def join02(): Bool & Impure =
        (1 :: Nil) |> List.toMutList |>
            MutList1.join(",") == "1"

    @test
    def join03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toMutList |>
            MutList1.join(",") == "1,2,3"

    @test
    def join04(): Bool & Impure =
        ("1" :: "2" :: "3" :: Nil) |> List.toMutList |>
            MutList1.join(",") == "1,2,3"


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinWith01(): Bool & Impure =
        List.toMutList(Nil: List[Int32]) |> MutList1.joinWith(x -> "${x + 1}", ",") == ""

    @test
    def joinWith02(): Bool & Impure =
        List.toMutList(1 :: Nil) |> MutList1.joinWith(x -> "${x + 1}", ",") == "2"

    @test
    def joinWith03(): Bool & Impure =
        List.toMutList(1 :: 2 :: 3 :: Nil) |> MutList1.joinWith(x -> "${x + 1}", ",") == "2,3,4"

    @test
    def joinWith04(): Bool & Impure =
        List.toMutList("1" :: "2" :: "3" :: Nil) |> MutList1.joinWith(x -> x + x, ",") == "11,22,33"

    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def iterator01(): Bool & Impure =
        let m = MutList1.new();
        match Iterable.iterator(m) {
            case Iterator(done, _) => done()
        }

    @test
    def iterator02(): Bool & Impure =
        let m = MutList1.new();
        MutList1.push!(1, m);
        match Iterable.iterator(m) {
            case Iterator(done, _) => not done()
        }

    @test
    def iterator03(): Bool & Impure =
        let m = MutList1.new();
        MutList1.push!(1, m);
        match Iterable.iterator(m) {
            case Iterator(_, next) => next() == 1
        }

    @test
    def iterator04(): Bool & Impure =
        let m = MutList1.new();
        MutList1.push!(1, m);
        MutList1.push!(2, m);
        match Iterable.iterator(m) {
            case Iterator(done, next) => {
                next();
                next();
                done()
            }
        }

    @test
    def iterator05(): Bool & Impure =
        let m = MutList1.new();
        MutList1.push!(1, m);
        MutList1.push!(2, m);
        match Iterable.iterator(m) {
            case Iterator(_, next) => {
                next();
                next() == 2
            }
        }

}
