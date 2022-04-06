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

namespace TestMList {
    use ToString.toString;
    use MList.sameElements;

    ///
    /// Helper function to test capacity of internal array.
    ///
    def capacity(v: MList[a, r]): Int32 \ Read(r) =
        let MList(a, _) = v;
        Array.length(deref a)


    /////////////////////////////////////////////////////////////////////////////
    // sameElements                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sameElements01(): Bool = region r {
        let u: MList[Int32, _] = new MList(r);
        let v: MList[Int32, _] = new MList(r);
        u `sameElements` v
    }

    @test
    def sameElements02(): Bool = region r {
        let u = new MList(r);
        MList.push!(0, u);
        let v = new MList(r);
        (u `sameElements` v) == false
    }

    @test
    def sameElements03(): Bool = region r {
        let u = new MList(r);
        MList.push!(2, u);
        MList.push!(3, u);
        let v = new MList(r);
        MList.push!(2, v);
        MList.push!(3, v);
        u `sameElements` v
    }

    @test
    def sameElements04(): Bool = region r {
        let u = new MList(r);
        MList.push!(2, u);
        MList.push!(3, u);
        let v = new MList(r);
        MList.push!(2, v);
        MList.push!(2, v);
        (u `sameElements` v) == false
    }

    @test
    def sameElements05(): Bool = region r {
        let u = new MList(r);
        MList.push!(1, u);
        let v = new MList(r);
        (u `sameElements` v) == false
    }


    /////////////////////////////////////////////////////////////////////////////
    // Order.compare                                                           //
    /////////////////////////////////////////////////////////////////////////////

    // TODO

    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def length01(): Bool = region r {
        MList.length(new MList(r)) == 0
    }

    @test
    def length02(): Bool = region r {
        let v = new MList(r);
        MList.push!(42, v);
        MList.length(v) == 1
    }

    @test
    def length03(): Bool = region r {
        MList.length(MList.range(r, 0, 2)) == 2
    }

    @test
    def length04(): Bool = region r {
        MList.length(MList.range(r, 0, 4)) == 4
    }

    @test
    def length05(): Bool = region r {
        MList.length(MList.range(r, 0, 42)) == 42
    }


    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isEmpty01(): Bool = region r {
        MList.isEmpty(new MList(r))
    }

    @test
    def isEmpty02(): Bool = region r {
        MList.isEmpty(MList.range(r, 0, 0))
    }

    @test
    def isEmpty03(): Bool = region r {
        MList.isEmpty(MList.range(r, 0, 1)) == false
    }

    @test
    def isEmpty04(): Bool = region r {
        MList.isEmpty(MList.range(r, 0, 2)) == false
    }

    @test
    def isEmpty05(): Bool = region r {
        MList.isEmpty(MList.range(r, 3, 4)) == false
    }


    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def memberOf01(): Bool = region r {
        MList.memberOf(42, new MList(r)) == false
    }

    @test
    def memberOf02(): Bool = region r {
        let v = new MList(r);
        MList.push!(2, v);
        MList.memberOf(2, v)
    }

    @test
    def memberOf03(): Bool = region r {
        let v = new MList(r);
        MList.push!(42, v);
        MList.memberOf(2, v) == false
    }

    @test
    def memberOf04(): Bool = region r {
        MList.memberOf(4, MList.range(r, 0, 4)) == false
    }

    @test
    def memberOf05(): Bool = region r {
        MList.memberOf(0, MList.range(r, 0, 3))
    }

    @test
    def memberOf06(): Bool = region r {
        MList.memberOf(2, MList.range(r, 0, 3))
    }


    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def count01(): Bool = region r {
        MList.count(x -> x > 2, MList.range(r, 0, 4)) == 1
    }

    @test
    def count02(): Bool = region r {
        MList.count(x -> x >= 2, MList.range(r, 0, 4)) == 2
    }

    @test
    def count03(): Bool = region r {
        MList.count(x -> x rem 2 == 0, MList.range(r, 0, 10)) == 5
    }

    @test
    def count04(): Bool = region r {
        MList.count(x -> x == 11, MList.range(r, 0, 21)) == 1
    }


    /////////////////////////////////////////////////////////////////////////////
    // product                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def product01(): Bool = region r {
        List.toMList(Nil) |> MList.product == 0
    }

    @test
    def product02(): Bool = region r {
        List.toMList(1 :: Nil) |> MList.product == 1
    }

    @test
    def product03(): Bool = region r {
        List.toMList(1 :: 2 :: 3 :: Nil) |> MList.product == 6
    }

    @test
    def product04(): Bool = region r {
        List.toMList(1 :: 2 :: 3 :: -3 :: Nil) |> MList.product == -18
    }

    @test
    def product05(): Bool = region r {
        List.toMList(-1 :: -2 :: -3 :: -4 :: Nil) |> MList.product == 24
    }

    @test
    def product06(): Bool = region r {
        List.toMList(10 :: -10 :: Nil) |> MList.product == -100
    }


    /////////////////////////////////////////////////////////////////////////////
    // productWith                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productWith01(): Bool = region r {
        List.toMList(Nil) |> MList.productWith(x -> x + 1) == 0
    }

    @test
    def productWith02(): Bool = region r {
        List.toMList(1 :: Nil) |> MList.productWith(x -> x + 1) == 2
    }

    @test
    def productWith03(): Bool = region r {
        List.toMList(1 :: 2 :: 3 :: Nil) |> MList.productWith(x -> x + 1) == 24
    }

    @test
    def productWith04(): Bool = region r {
        List.toMList(1 :: 2 :: 3 :: -3 :: Nil) |> MList.productWith(x -> x + 1) == -48
    }

    @test
    def productWith05(): Bool = region r {
        List.toMList(-2 :: -3 :: -4 :: -5 :: Nil) |> MList.productWith(x -> x + 1) == 24
    }

    @test
    def productWith06(): Bool = region r {
        List.toMList(10 :: -10 :: Nil) |> MList.productWith(x -> x + 1) == -99
    }


    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def exists01(): Bool = region r {
        MList.exists(x -> x > 9, MList.range(r, 0, 9)) == false
    }

    @test
    def exists02(): Bool = region r {
        MList.exists(x -> x < 0, MList.range(r, 0, 9)) == false
    }

    @test
    def exists03(): Bool = region r {
        MList.exists(x -> x == 8, MList.range(r, 0, 9))
    }

    @test
    def exists04(): Bool = region r {
        MList.exists(x -> x > 0, MList.range(r, 0, 9))
    }

    @test
    def exists05(): Bool = region r {
        MList.exists(x -> x == 0, new MList(r)) == false
    }


    /////////////////////////////////////////////////////////////////////////////
    // forall                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forall01(): Bool = region r {
        MList.forall(x -> x > 0, MList.range(r, 0, 9)) == false
    }

    @test
    def forall02(): Bool = region r {
        MList.forall(x -> x >= 0, MList.range(r, 0, 9))
    }

    @test
    def forall03(): Bool = region r {
        MList.forall(x -> x >= 0 and x < 9, MList.range(r, 0, 9))
    }

    @test
    def forall05(): Bool = region r {
        MList.forall(x -> x > 0, new MList(r))
    }


    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def head01(): Bool = region r {
        let v = new MList(r);
        MList.push!(42, v);
        MList.head(v) == Some(42)
    }

    @test
    def head02(): Bool = region r {
        MList.head(new MList(r): MList[Unit, _]) == None
    }

    @test
    def head03(): Bool = region r {
        let v = new MList(r);
        MList.push!(101, v);
        MList.push!(102, v);
        MList.head(v) == Some(101)
    }


    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def last01(): Bool = region r {
        MList.last(new MList(r): MList[Unit, _]) == None
    }

    @test
    def last02(): Bool = region r {
        MList.last(MList.range(r, 0, 8)) == Some(7)
    }

    @test
    def last03(): Bool = region r {
        let v = new MList(r);
        MList.push!('A', v);
        MList.push!('B', v);
        MList.last(v) == Some('B')
    }


    /////////////////////////////////////////////////////////////////////////////
    // indexOfLeft                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indexOfLeft01(): Bool = region r {
        MList.indexOfLeft(2, MList.range(r, 2, 3)) == Some(0)
    }

    @test
    def indexOfLeft02(): Bool = region r {
        MList.indexOfLeft(10, MList.range(r, 0, 10)) == None
    }

    @test
    def indexOfLeft03(): Bool = region r {
        let v = new MList(r);
        MList.push!('A', v);
        MList.push!('B', v);
        MList.push!('C', v);
        MList.indexOfLeft('B', v) == Some(1)
    }

    @test
    def indexOfLeft04(): Bool = region r {
        MList.indexOfLeft(99, MList.range(r, 0, 100)) == Some(99)
    }

    /////////////////////////////////////////////////////////////////////////////
    // indexOfRight                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indexOfRight01(): Bool = region r {
        MList.indexOfRight(2, MList.range(r, 2, 3)) == Some(0)
    }

    @test
    def indexOfRight02(): Bool = region r {
        MList.indexOfRight(10, MList.range(r, 0, 10)) == None
    }

    @test
    def indexOfRight03(): Bool = region r {
        let v = new MList(r);
        MList.push!('A', v);
        MList.push!('B', v);
        MList.push!('A', v);
        MList.indexOfRight('A', v) == Some(2)
    }

    @test
    def indexOfRight04(): Bool = region r {
        let v1 = MList.range(r, 0, 4);
        let v2 = MList.range(r, 0, 4);
        MList.foldLeft((_, x) -> MList.push!(x, v1), (), v2);
        MList.indexOfRight(2, v1) == Some(6)
    }

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findLeft01(): Bool = region r {
        MList.findLeft(x -> x > 3, MList.range(r, 3, 11)) == Some(4)
    }

    @test
    def findLeft02(): Bool = region r {
        MList.findLeft(x -> x > 10, MList.range(r, 3, 11)) == None
    }

    @test
    def findLeft03(): Bool = region r {
        MList.findLeft(x -> x < 3, MList.range(r, 3, 11)) == None
    }

    @test
    def findLeft04(): Bool = region r {
        MList.findLeft(x -> x rem 2 == 1, MList.range(r, 3, 11)) == Some(3)
    }

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findRight01(): Bool = region r {
        MList.findRight(x -> x > 0, MList.range(r, 2, 5)) == Some(4)
    }

    @test
    def findRight02(): Bool = region r {
        MList.findRight(x -> x == 10, MList.range(r, 9, 10)) == None
    }

    @test
    def findRight03(): Bool = region r {
        MList.findRight(x -> x rem 2 == 0, MList.range(r, 3, 4)) == None
    }

    @test
    def findRight04(): Bool = region r {
        MList.findRight(x -> x < 5, MList.range(r, 3, 11)) == Some(4)
    }


    /////////////////////////////////////////////////////////////////////////////
    // scanLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def scanLeft01(): Bool = region r {
        let v = MList.scanLeft((x, y) -> x + y, 0, new MList(r));
        MList.length(v) == 1 and MList.head(v) == Some(0)
    }

    @test
    def scanLeft02(): Bool = region r {
        let v1 = MList.range(r, 1, 6);
        let v2 = new MList(r);
        MList.push!(0, v2);
        MList.push!(1, v2);
        MList.push!(3, v2);
        MList.push!(6, v2);
        MList.push!(10, v2);
        MList.push!(15, v2);
        let v3 = MList.scanLeft((x, y) -> x + y, 0, v1);
        MList.sameElements(v2, v3)
    }

    @test
    def scanLeft03(): Bool = region r {
        let v = MList.scanLeft((x, y) -> x + y, 7, MList.range(r, 5, 13));
        MList.last(v) == Some(75)
    }


    /////////////////////////////////////////////////////////////////////////////
    // scanRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def scanRight01(): Bool = region r {
        let v = MList.scanRight((x, y) -> x + y, 0, new MList(r));
        MList.length(v) == 1 and MList.head(v) == Some(0)
    }

    @test
    def scanRight02(): Bool = region r {
        let v1 = MList.range(r, 1, 6);
        let v2 = new MList(r);
        MList.push!(15, v2);
        MList.push!(14, v2);
        MList.push!(12, v2);
        MList.push!(9, v2);
        MList.push!(5, v2);
        MList.push!(0, v2);
        let v3 = MList.scanRight((x, y) -> x + y, 0, v1);
        MList.sameElements(v2, v3)
    }

    @test
    def scanRight03(): Bool = region r {
        let v = MList.scanRight((x, y) -> x + y, 7, MList.range(r, 5, 13));
        MList.last(v) == Some(7)
    }


    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map01(): Bool = region r {
        let v = MList.map(x -> x * 2, MList.range(r, 0, 6));
        MList.sameElements(v, Array.toMList([0, 2, 4, 6, 8, 10]))
    }

    @test
    def map02(): Bool = region r {
        let v = MList.map(identity, MList.range(r, 0, 10));
        MList.sameElements(v, MList.range(r, 0, 10))
    }

    @test
    def map03(): Bool = region r {
        let v = MList.map(identity, new MList(r): MList[Unit, _]);
        MList.sameElements(v, new MList(r))
    }

    @test
    def map04(): Bool = region r {
        let v = MList.map(x -> x + 1, MList.range(r, 0, 100));
        MList.sameElements(v, MList.range(r, 1, 101))
    }


    /////////////////////////////////////////////////////////////////////////////
    // transform!                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def transform!01(): Bool = region r {
        let v = MList.range(r, 0, 6);
        MList.transform!(x -> x * 2, v);
        MList.sameElements(v, Array.toMList([0, 2, 4, 6, 8, 10]))
    }

    @test
    def transform!02(): Bool = region r {
        let v = MList.range(r, 0, 10);
        MList.transform!(identity, v);
        MList.sameElements(v, MList.range(r, 0, 10))
    }

    @test
    def transform!03(): Bool = region r {
        let v: MList[Unit, _] = new MList(r);
        MList.transform!(identity, v);
        MList.sameElements(v, new MList(r))
    }

    @test
    def transform!04(): Bool = region r {
        let v = MList.range(r, 0, 100);
        MList.transform!(x -> x + 1, v);
        MList.sameElements(v, MList.range(r, 1, 101))
    }


    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithIndex01(): Bool = region r {
        let v = MList.mapWithIndex((_, i) -> if (i < 3) 0 else 1, MList.range(r, 0, 6));
        MList.sameElements(v, Array.toMList([0, 0, 0, 1, 1, 1]))
    }

    @test
    def mapWithIndex02(): Bool = region r {
        let v = MList.mapWithIndex((x, i) -> if (x == 8 and i == 1) 42 else 0, MList.range(r, 7, 10));
        MList.sameElements(v, Array.toMList([0, 42, 0]))
    }

    @test
    def mapWithIndex03(): Bool = region r {
        let v = MList.mapWithIndex((_, i) -> if (i == 0) 42 else 0, new MList(r));
        MList.sameElements(v, new MList(r))
    }


    /////////////////////////////////////////////////////////////////////////////
    // transformWithIndex!                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def transformWithIndex!01(): Bool = region r {
        let v = MList.range(r, 0, 6);
        MList.transformWithIndex!((_, i) -> if (i < 3) 0 else 1, v);
        MList.sameElements(v, Array.toMList([0, 0, 0, 1, 1, 1]))
    }

    @test
    def transformWithIndex!02(): Bool = region r {
        let v = MList.range(r, 7, 10);
        MList.transformWithIndex!((x, i) -> if (x == 8 and i == 1) 42 else 0, v);
        MList.sameElements(v, Array.toMList([0, 42, 0]))
    }

    @test
    def transformWithIndex!03(): Bool = region r {
        let v = new MList(r);
        MList.transformWithIndex!((_, i) -> if (i == 0) 42 else 0, v);
        MList.sameElements(v, new MList(r))
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool = region r {
        MList.foldLeft((acc, x) -> acc + x, 0, MList.range(r, 1, 5)) == 10
    }

    @test
    def foldLeft02(): Bool = region r {
        let v1 = new MList(r);
        let v2 = MList.range(r, 0, 10);
        MList.foldLeft((_, x) -> MList.push!(x, v1), (), v2);
        MList.sameElements(v1, v2)
    }

    @test
    def foldLeft03(): Bool = region r {
        MList.foldLeft((acc, x) -> x :: acc, Nil, MList.range(r, 0, 6)) == 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil
    }

    @test
    def foldLeft04(): Bool = region r {
        MList.foldLeft((acc, x) -> acc - x, 0, MList.range(r, 0, 100)) == -4950
    }

    @test
    def foldLeft05(): Bool = region r {
        MList.foldRight((acc, x) -> acc + x, 42, new MList(r)) == 42
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool = region r {
        MList.foldRight((x, acc) -> acc + x, 0, MList.range(r, 1, 5)) == 10
    }

    @test
    def foldRight02(): Bool = region r {
        MList.foldRight((x, acc) -> Int32.toString(x) + acc, "", MList.range(r, 3, 8)) == "34567"
    }

    @test
    def foldRight03(): Bool = region r {
        MList.foldRight((x, acc) -> x :: acc, Nil, MList.range(r, 0, 6)) == 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil
    }

    @test
    def foldRight04(): Bool = region r {
        MList.foldRight((x, acc) -> x - acc, 0, MList.range(r, 0, 100)) == -50
    }

    @test
    def foldRight05(): Bool = region r {
        MList.foldRight((x, acc) -> x + acc, 42, new MList(r)) == 42
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldRightLazy                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRightLazy01(): Bool = region r {
        MList.foldRightLazy((x, acc) -> force acc + x, 0, MList.range(r, 1, 5)) == 10
    }

    @test
    def foldRightLazy02(): Bool = region r {
        MList.foldRightLazy((x, acc) -> Int32.toString(x) + force acc, "", MList.range(r, 3, 8)) == "34567"
    }

    @test
    def foldRightLazy03(): Bool = region r {
        MList.foldRightLazy((x, acc) -> x :: force acc, Nil, MList.range(r, 0, 6)) == 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil
    }

    @test
    def foldRightLazy04(): Bool = region r {
        MList.foldRightLazy((x, acc) -> x - force acc, 0, MList.range(r, 0, 100)) == -50
    }

    @test
    def foldRightLazy05(): Bool = region r {
        MList.foldRightLazy((x, acc) -> x + force acc, 42, new MList(r)) == 42
    }


    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceLeft01(): Bool = region r {
        MList.reduceLeft((x, y) -> x + y, new MList(r): MList[Int32, _]) == None
    }

    @test
    def reduceLeft02(): Bool = region r {
        MList.reduceLeft((x, y) -> x + y, MList.range(r, 0, 5)) == Some(10)
    }

    @test
    def reduceLeft03(): Bool = region r {
        MList.reduceLeft((x, y) -> x - y, MList.range(r, 0, 101)) == Some(-5050)
    }

    @test
    def reduceLeft04(): Bool = region r {
        MList.reduceLeft((_, y) -> if (y > 4) 1 else 0, MList.range(r, 0, 6)) == Some(1)
    }


    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceRight01(): Bool = region r {
        MList.reduceRight((x, y) -> x + y, new MList(r): MList[Int32, _]) == None
    }

    @test
    def reduceRight02(): Bool = region r {
        MList.reduceRight((x, y) -> x + y, MList.range(r, 0, 5)) == Some(10)
    }

    @test
    def reduceRight03(): Bool = region r {
        MList.reduceRight((x, y) -> x - y, MList.range(r, 0, 6)) == Some(-3)
    }

    @test
    def reduceRight04(): Bool = region r {
        MList.reduceRight((x, _) -> if (x < 2) 1 else 0, MList.range(r, 0, 6)) == Some(1)
    }


    /////////////////////////////////////////////////////////////////////////////
    // clear!                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def clear!01(): Bool = region r {
        let v = MList.range(r, 0, 1);
        let len = MList.length(v);
        MList.clear!(v);
        len == 1 and MList.isEmpty(v)
    }

    @test
    def clear!02(): Bool = region r {
        let v = new MList(r);
        let len = MList.length(v);
        MList.clear!(v);
        len == MList.length(v)
    }

    @test
    def clear!03(): Bool = region r {
        let v = MList.range(r, 0, 100);
        MList.clear!(v);
        MList.isEmpty(v)
    }


    /////////////////////////////////////////////////////////////////////////////
    // copy                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def copy01(): Bool = region r {
        let v1 = MList.range(r, 0, 6);
        let v2 = MList.copy(v1);
        v1 `sameElements` v2
    }

    @test
    def copy02(): Bool = region r {
        let v1 = MList.range(r, 0, 100);
        let v2 = MList.copy(v1);
        MList.transform!(x -> x + 1, v2);
        MList.last(v1) == Some(99) and MList.last(v2) == Some(100)
    }

    @test
    def copy03(): Bool = region r {
        let v1 = MList.range(r, 0, 6);
        let v2 = MList.copy(v1);
        MList.clear!(v1);
        MList.length(v1) == 0 and MList.length(v2) == 6
    }

    @test
    def copy04(): Bool = region r {
        let v1 = MList.range(r, 0, 1000);
        let MList(l1, n1) = v1;
        let MList(l2, n2) = MList.copy(v1);
        (deref n1 == deref n2) and (deref l1 `Array.sameElements` deref l2)
    }

    @test
    def copy05(): Bool = region r {
        let v1 = MList.range(r, 0, 3);
        let MList(l1, n1) = v1;
        let MList(l2, n2) = MList.copy(v1);
        (deref n1 == deref n2) and (deref l1 `Array.sameElements` deref l2) and
            (Array.length(deref l2) == 8)
    }


    /////////////////////////////////////////////////////////////////////////////
    // pop                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def pop01(): Bool = region r {
        let v = MList.range(r, 0, 6);
        let len = MList.length(v);
        let last = MList.pop!(v);
        MList.length(v) == len - 1 and last == Some(5)
    }

    @test
    def pop02(): Bool = region r {
        let v: MList[Unit, _] = new MList(r);
        MList.pop!(v) == None
    }

    @test
    def pop03(): Bool = region r {
        let v = MList.range(r, 1, 4);
        let three = MList.pop!(v);
        let two = MList.pop!(v);
        let one = MList.pop!(v);
        let none = MList.pop!(v);
        three == Some(3) and two == Some(2) and one == Some(1) and none == None
    }


    /////////////////////////////////////////////////////////////////////////////
    // push                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def push01(): Bool = region r {
        let v = new MList(r);
        let len = MList.length(v);
        MList.push!(1, v);
        MList.last(v) == Some(1) and MList.length(v) == len + 1
    }

    @test
    def push02(): Bool = region r {
        let v = new MList(r);
        let len = MList.length(v);
        List.foreach(x -> MList.push!(x, v), List.range(0, 97));
        MList.head(v) == Some(0) and MList.last(v) == Some(96) and MList.length(v) == len + 97
    }

    @test
    def push03(): Bool = region r {
        let v = new MList(r);
        MList.push!(42, v);
        MList.push!(31, v);
        MList.push!(1019, v);
        MList.length(v) == 3 and MList.get(0, v) == 42 and MList.get(1, v) == 31 and MList.get(2, v) == 1019
    }

    @test
    def push04(): Bool = region r {
        let v = new MList(r);
        MList.push!(0, v);
        MList.push!(0, v);
        MList.push!(0, v);
        MList.push!(0, v);
        MList.length(v) == 4 and MList.get(0, v) == 0 and MList.get(1, v) == 0 and MList.get(2, v) == 0 and MList.get(3, v) == 0
    }


    /////////////////////////////////////////////////////////////////////////////
    // insert!                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def insert!01(): Bool = region r {
        let v = new MList(r);
        MList.insert!(42, 0, v);
        MList.length(v) == 1 and MList.head(v) == Some(42)
    }

    @test
    def insert!02(): Bool = region r {
        let v = new MList(r);
        MList.insert!(107, 0, v);
        MList.insert!(7, 0, v);
        MList.length(v) == 2 and MList.head(v) == Some(7)
    }

    @test
    def insert!03(): Bool = region r {
        let v = new MList(r);
        List.foreach(x -> MList.insert!(x, 0, v), List.range(0, 107));
        MList.length(v) == 107 and MList.head(v) == Some(106)
    }

    @test
    def insert!04(): Bool = region r {
        let v = new MList(r);
        MList.insert!(42, 0, v);
        MList.insert!(3, 1, v);
        MList.insert!(5, 2, v);
        MList.insert!(6, 0, v);
        MList.length(v) == 4 and MList.head(v) == Some(6) and MList.last(v) == Some(5)
    }

    @test
    def insert!05(): Bool = region r {
        let v = new MList(r);
        List.foreach(x -> MList.push!(x, v), List.range(97, 147));
        MList.insert!(42, 0, v);
        MList.insert!(49, 0, v);
        MList.length(v) == 52 and MList.head(v) == Some(49) and MList.last(v) == Some(146)
    }

    @test
    def insert!06(): Bool = region r {
        let v = new MList(r);
        MList.insert!(42, 0, v);
        MList.insert!(42, 0, v);
        MList.insert!(42, 0, v);
        MList.insert!(42, 1, v);
        MList.insert!(97, 0, v);
        MList.insert!(98, 2, v);
        MList.length(v) == 6 and MList.head(v) == Some(97) and MList.last(v) == Some(42)
    }

    @test
    def insert!07(): Bool = region r {
        let v = new MList(r);
        MList.insert!(97, 0, v);
        MList.insert!(98, 1, v);
        MList.insert!(99, 2, v);
        MList.insert!(107, 1, v);
        MList.length(v) == 4 and MList.get(1, v) == 107
    }

    @test
    def insert!08(): Bool = region r {
        let v = MList.range(r, 0, 4);
        MList.insert!(42, 3, v);
        MList.insert!(41, 2, v);
        MList.insert!(40, 1, v);
        MList.insert!(55, 0, v);
        MList.insert!(33, 1, v);
        MList.sameElements(v, List.toMList(55 :: 33 :: 0 :: 40 :: 1 :: 41 :: 2 :: 42 :: 3 :: Nil))
    }


    /////////////////////////////////////////////////////////////////////////////
    // remove!                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def remove!01(): Bool = region r {
        let v: MList[Unit, _] = new MList(r);
        MList.remove!(0, v);
        MList.sameElements(v, new MList(r))
    }

    @test
    def remove!02(): Bool = region r {
        let v = MList.range(r, 0, 4);
        MList.remove!(0, v);
        MList.sameElements(v, MList.range(r, 1, 4))
    }

    @test
    def remove!03(): Bool = region r {
        let v = MList.range(r, 0, 4);
        MList.remove!(3, v);
        MList.sameElements(v, MList.range(r, 0, 3))
    }

    @test
    def remove!04(): Bool = region r {
        let v = MList.range(r, 3, 7);
        MList.remove!(0, v);
        MList.remove!(0, v);
        MList.remove!(0, v);
        MList.sameElements(v, MList.range(r, 6, 7))
    }

    @test
    def remove!05(): Bool = region r {
        let v = MList.range(r, 9, 12);
        MList.remove!(2, v);
        MList.remove!(1, v);
        MList.remove!(0, v);
        MList.sameElements(v, new MList(r))
    }

    @test
    def remove!06(): Bool = region r {
        let v = MList.range(r, 7, 13);
        MList.remove!(0, v);
        MList.remove!(0, v);
        MList.remove!(0, v);
        MList.remove!(0, v);
        MList.remove!(0, v);
        MList.remove!(0, v);
        MList.sameElements(v, new MList(r))
    }

    @test
    def remove!07(): Bool = region r {
        let v = MList.range(r, 0, 3);
        MList.remove!(3, v);
        MList.remove!(7, v);
        MList.remove!(99, v);
        MList.sameElements(v, MList.range(r, 0, 3))
    }


    /////////////////////////////////////////////////////////////////////////////
    // append!                                                                 //
    /////////////////////////////////////////////////////////////////////////////
/*
    @test
    def append!01(): Bool = region r {
        let v1 = MList.range(r, 0, 4);
        let v2 = MList.range(r, 4, 8);
        MList.append!(v2, v1);
        MList.sameElements(v1, MList.range(r, 0, 8))
    }

    @test
    def append!02(): Bool = region r {
        let v1 = new MList(r);
        let v2 = MList.range(r, 1, 4);
        MList.append!(v2, v1);
        MList.sameElements(v1, MList.range(r, 1, 4))
    }

    @test
    def append!03(): Bool = region r {
        let v1 = MList.range(r, 9, 24);
        let v2 = new MList(r);
        MList.append!(v2, v1);
        MList.sameElements(v1, MList.range(r, 9, 24))
    }

    @test
    def append!04(): Bool = region r {
        let v1: MList[Unit, _] = new MList(r);
        let v2: MList[Unit, _] = new MList(r);
        MList.append!(v2, v1);
        MList.sameElements(v1, new MList(r))
    }

    @test
    def append!05(): Bool = region r {
        let v1 = MList.range(r, 1, 3);
        let v2 = MList.range(r, 3, 5);
        let v3 = MList.range(r, 5, 99);
        MList.append!(v2, v1);
        MList.append!(v3, v1);
        MList.sameElements(v1, MList.range(r, 1, 99))
    }
*/

    /////////////////////////////////////////////////////////////////////////////
    // retain!                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def retain!01(): Bool = region r {
        let v = MList.range(r, 0, 10);
        MList.retain!(x -> x rem 2 == 0, v);
        MList.sameElements(List.toMList(0 :: 2 :: 4 :: 6 :: 8 :: Nil), v)
    }

    @test
    def retain!02(): Bool = region r {
        let v = MList.range(r, 0, 10);
        MList.retain!(x -> x rem 2 == 1, v);
        MList.sameElements(List.toMList(1 :: 3 :: 5 :: 7 :: 9 :: Nil), v)
    }

    @test
    def retain!03(): Bool = region r {
        let v = MList.range(r, 0, 10);
        MList.retain!(x -> x > 5, v);
        MList.sameElements(v, MList.range(r, 6, 10))
    }

    @test
    def retain!04(): Bool = region r {
        let v = MList.range(r, 0, 10);
        MList.retain!(x -> x > 10, v);
        MList.sameElements(v, new MList(r))
    }


    /////////////////////////////////////////////////////////////////////////////
    // replace!                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def replace!01(): Bool = region r {
        let v = new MList(r);
        MList.push!(1, v);
        MList.push!(2, v);
        MList.push!(2, v);
        MList.push!(1, v);
        MList.push!(1, v);
        MList.replace!(from = 1, to = 42, v);
        MList.sameElements(List.toMList(42 :: 2 :: 2 :: 42 :: 42 :: Nil), v)
    }

    @test
    def replace!02(): Bool = region r {
        let v = MList.range(r, 4, 7);
        MList.replace!(from = 4, to = 0, v);
        MList.sameElements(List.toMList(0 :: 5 :: 6 :: Nil), v)
    }

    @test
    def replace!03(): Bool = region r {
        let v = MList.range(r, 4, 7);
        MList.replace!(from = 5, to = 0, v);
        MList.sameElements(List.toMList(4 :: 0 :: 6 :: Nil), v)
    }

    @test
    def replace!04(): Bool = region r {
        let v = MList.range(r, 4, 7);
        MList.replace!(from = 6, to = 0, v);
        MList.sameElements(List.toMList(4 :: 5 :: 0 :: Nil), v)
    }

    @test
    def replace!05(): Bool = region r {
        let v = List.toMList(List.repeat(97, 'a'));
        MList.replace!(from = 'a', to = 'z', v);
        MList.sameElements(List.toMList(List.repeat(97, 'z')), v)
    }


    /////////////////////////////////////////////////////////////////////////////
    // reverse!                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reverse01!(): Bool = region r {
        let v: MList[Unit, _] = new MList(r);
        MList.reverse!(v);
        MList.sameElements(v, new MList(r))
    }

    @test
    def reverse02!(): Bool = region r {
        let v = MList.range(r, 0, 1);
        MList.reverse!(v);
        MList.sameElements(v, MList.range(r, 0, 1))
    }

    @test
    def reverse03!(): Bool = region r {
        let v = MList.range(r, 0, 5);
        MList.reverse!(v);
        MList.sameElements(v, List.toMList(4 :: 3 :: 2 :: 1 :: 0 :: Nil))
    }

    @test
    def reverse04!(): Bool = region r {
        let v = MList.range(r, 0, 6);
        MList.reverse!(v);
        MList.sameElements(v, List.toMList(5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil))
    }

    @test
    def reverse05!(): Bool = region r {
        let v = List.toMList(7 :: 6 :: 5 :: 4 :: 3 :: 2 :: Nil);
        MList.reverse!(v);
        MList.sameElements(v, MList.range(r, 2, 8))
    }


    /////////////////////////////////////////////////////////////////////////////
    // shrink!                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def shrink!01(): Bool = region r {
        let v = new MList(r);
        MList.push!(0, v);
        MList.push!(1, v);
        MList.push!(2, v);
        MList.shrink!(v);
        MList.length(v) == 3
    }

    @test
    def shrink!02(): Bool = region r {
        let v = new MList(r);
        MList.shrink!(v);
        MList.length(v) == 0
    }

    @test
    def shrink!03(): Bool = region r {
        let v = MList.range(r, 0, 9);
        MList.push!(9, v);
        MList.shrink!(v);
        MList.length(v) == 10
    }

/*
    @test
    def shrink!04(): Bool = region r {
        let v = new MList(r);
        MList.push!(4, v);
        MList.append!(MList.range(r, 0, 4), v);
        MList.push!(5, v);
        MList.shrink!(v);
        MList.length(v) == 6
    }

*/
    /////////////////////////////////////////////////////////////////////////////
    // truncate!                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def truncate!01(): Bool = region r {
        let v: MList[Unit, _] = new MList(r);
        MList.truncate!(0, v);
        MList.sameElements(v, new MList(r))
    }

    @test
    def truncate!02(): Bool = region r {
        let v = MList.range(r, 0, 10);
        MList.truncate!(5, v);
        MList.sameElements(v, MList.range(r, 0, 5))
    }

    @test
    def truncate!03(): Bool = region r {
        let v = MList.range(r, 0, 99);
        MList.truncate!(-1, v);
        MList.sameElements(v, new MList(r))
    }

    @test
    def truncate!04(): Bool = region r {
        let v = MList.range(r, 7, 8);
        MList.truncate!(2, v);
        MList.sameElements(v, MList.range(r, 7, 8))
    }

    @test
    def truncate!05(): Bool = region r {
        let v = MList.range(r, 99, 102);
        MList.truncate!(1, v);
        MList.sameElements(v, MList.range(r, 99, 100))
    }


    /////////////////////////////////////////////////////////////////////////////
    // reserve!                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reserve!01(): Bool = region r {
        let v = new MList(r);
        MList.reserve!(4, v);
        capacity(v) - MList.length(v) >= 4
    }

    @test
    def reserve!02(): Bool = region r {
        let v = MList.range(r, 0, 5);
        MList.reserve!(5, v);
        capacity(v) - MList.length(v) >= 5
    }

    @test
    def reserve!03(): Bool = region r {
        let v = MList.range(r, 0, 99);
        MList.reserve!(1, v);
        capacity(v) - MList.length(v) >= 1
    }

    @test
    def reserve!04(): Bool = region r {
        let v = MList.range(r, 0, 100);
        MList.reserve!(100, v);
        capacity(v) - MList.length(v) >= 100
    }

    @test
    def reserve!05(): Bool = region r {
        let v = MList.range(r, 0, 5);
        MList.reserve!(MList.length(v), v);
        capacity(v) - MList.length(v) >= MList.length(v)
    }

    @test
    def reserve!06(): Bool = region r {
        let n = 197;
        let v = MList.range(r, 0, n);
        MList.reserve!(1, v);
        MList.reserve!(1, v);
        MList.reserve!(1, v);
        MList.reserve!(1, v);
        MList.reserve!(1, v);
        capacity(v) - MList.length(v) >= 1
    }


    /////////////////////////////////////////////////////////////////////////////
    // formatWith                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def formatWith01(): Bool = region r {
        let v = MList.range(r, 0, 5);
        MList.formatWith(Int32.toString, ", ", v) == "0, 1, 2, 3, 4"
    }

    @test
    def formatWith02(): Bool = region r {
        let v = MList.range(r, 1, 4);
        MList.formatWith(Int32.toString, "+", v) == "1+2+3"
    }

    @test
    def formatWith03(): Bool = region r {
        let v = MList.range(r, 2, 3);
        MList.formatWith(Int32.toString, "/", v) == "2"
    }

    @test
    def formatWith04(): Bool = region r {
        let v = MList.range(r, 6, 9);
        MList.formatWith(Int32.toString, "/", v) == "6/7/8"
    }


    /////////////////////////////////////////////////////////////////////////////
    // foreach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foreach01(): Bool = region r {
        let v = new MList(r);
        let sb = StringBuilder.new();
        let f = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MList.foreach(f, v);
        StringBuilder.toString(sb) == ""
    }

    @test
    def foreach02(): Bool = region r {
        let v = MList.range(r, 0, 5);
        let sb = StringBuilder.new();
        let f = x -> if (x rem 2 == 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MList.foreach(f, v);
        StringBuilder.toString(sb) == "TFTFT"
    }

    @test
    def foreach03(): Bool = region r {
        let v = MList.range(r, 0, 2);
        let sb = StringBuilder.new();
        let f = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MList.foreach(f, v);
        StringBuilder.toString(sb) == "FT"
    }

    @test
    def foreach04(): Bool = region r {
        let v = MList.range(r, 0, 2);
        let sb = StringBuilder.new();
        let f = x -> if (x < 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MList.foreach(f, v);
        StringBuilder.toString(sb) == "FF"
    }


    /////////////////////////////////////////////////////////////////////////////
    // foreachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foreachWithIndex01(): Bool = region r {
        let v1 = MList.range(r, 0, 100);
        let v2 = new MList(r);
        MList.foreachWithIndex((x, i) -> if (x rem 2 == 0 and i < 10) MList.push!(x, v2) else (), v1);
        MList.sameElements(v2, List.toMList(0 :: 2 :: 4 :: 6 :: 8 :: Nil))
    }

    @test
    def foreachWithIndex02(): Bool = region r {
        let v = MList.range(r, 1, 4);
        let sb = StringBuilder.new();
        let f = (x, i) -> if (x rem 2 == 1 and i == 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MList.foreachWithIndex(f, v);
        StringBuilder.toString(sb) == "TFF"
    }

    @test
    def foreachWithIndex03(): Bool = region r {
        let v = MList.range(r, 0, 5);
        let sb = StringBuilder.new();
        let f = (x, _) -> if (x rem 2 == 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MList.foreachWithIndex(f, v);
        StringBuilder.toString(sb) == "TFTFT"
    }


    /////////////////////////////////////////////////////////////////////////////
    // compress!                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def compress!01(): Bool = region r {
        // Pop 76 element such that the load factor is strictly lower than 1 / 4.
        let v = MList.range(r, 0, 100);
        let c = capacity(v);
        let i = ref 0;
        let f = _ -> {
            if (deref i < 76) {
                MList.pop!(v);
                i := deref i + 1
            }
            else ()
        };
        MList.foreach(f, v);
        capacity(v) == c / 2 and
            MList.length(v) == 100 - 75 - 1
    }

    @test
    def compress!02(): Bool = region r {
        let v = new MList(r);
        MList.compress!(v);
        capacity(v) == capacity(new MList(r))
    }

    @test
    def compress!03(): Bool = region r {
        let v = new MList(r);
        let c = capacity(v);
        MList.push!(42, v);
        MList.compress!(v);
        capacity(v) <= c and
        MList.length(v) == 1
    }

    @test
    def compress!04(): Bool = region r {
        let v1 = new MList(r);
        let l = List.range(0, 5);
        List.foldLeft((_, x) -> MList.push!(x, v1), (), l);
        let v2 = new MList(r);
        List.foldLeft((_, x) -> MList.push!(x, v2), (), l);
        MList.compress!(v1);
        capacity(v1) <= capacity(v2)
    }


    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def minimumBy01(): Bool = region r {
        MList.minimumBy((x, y) -> x <=> y, new MList(r): MList[Int32, _]) == None
    }

    @test
    def minimumBy02(): Bool = region r {
        MList.minimumBy((x, y) -> x <=> y, MList.range(r, 0, 5)) == Some(0)
    }

    @test
    def minimumBy03(): Bool = region r {
        MList.minimumBy((x, y) -> x <=> y, MList.range(r, 5, 10)) == Some(5)
    }

    @test
    def minimumBy04(): Bool = region r {
        let v = MList.range(r, 0, 6);
        MList.push!(-2, v);
        MList.minimumBy((x, y) -> x <=> y, v) == Some(-2)
    }

    @test
    def minimumBy05(): Bool = region r {
        let v = MList.range(r, 9, 19);
        let cmp = (x, y) -> if (x < y and x rem 2 == 0) LessThan else GreaterThan;
        MList.minimumBy(cmp, v) == Some(10)
    }


    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def maximumBy01(): Bool = region r {
        MList.maximumBy((x, y) -> x <=> y, new MList(r): MList[Int32, _]) == None
    }

    @test
    def maximumBy02(): Bool = region r {
        MList.maximumBy((x, y) -> x <=> y, MList.range(r, 0, 5)) == Some(4)
    }

    @test
    def maximumBy03(): Bool = region r {
        MList.maximumBy((x, y) -> x <=> y, MList.range(r, 5, 10)) == Some(9)
    }

    @test
    def maximumBy04(): Bool = region r {
        let v = MList.range(r, 0, 6);
        MList.push!(11, v);
        MList.maximumBy((x, y) -> x <=> y, v) == Some(11)
    }

    @test
    def maximumBy05(): Bool = region r {
        let v = MList.range(r, 9, 19);
        let cmp = (x, y) -> if (x > y and x rem 2 == 0) GreaterThan else LessThan;
        MList.maximumBy(cmp, v) == Some(18)
    }


    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toArray01(): Bool = region r {
        Array.sameElements(MList.toArray(Array.toMList([1, 2, 3])), [1, 2, 3])
    }

    @test
    def toArray02(): Bool = region r {
        let v = Array.toMList([1, 2, 3]);
        let a = MList.toArray(v);
        a[1] = 42;
        MList.get(1, v) == 2
    }

    @test
    def toArray03(): Bool = region r {
        let v = Array.toMList([1, 2, 3]);
        MList.pop!(v);
        MList.pop!(v);
        Array.length(MList.toArray(v)) == 1
    }


    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toList01(): Bool = region r {
        new MList(r): MList[Int32, _] |> MList.toList == Nil: List[Int32]
    }

    @test
    def toList02(): Bool = region r {
        let ml = MList.range(r, 0, 1000);
        ml |> MList.toList == List.range(0, 1000) and
            not (MList.isEmpty(ml))
    }

    @test
    def toList03(): Bool = region r {
        let ml = MList.range(r, 0, 1000);
        ml |> MList.toList == List.range(0, 1000) and
           ml `sameElements` MList.range(r, 0, 1000)
    }


    /////////////////////////////////////////////////////////////////////////////
    // toChain                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toChain01(): Bool = region r {
        MList.toChain(List.toMList(Nil): MList[Int32, _]) == Chain.empty(): Chain[Int32]
    }

    @test
    def toChain02(): Bool = region r {
        MList.toChain(List.toMList(1 :: Nil)) == Chain.singleton(1)
    }

    @test
    def toChain03(): Bool = region r {
        MList.toChain(List.toMList(1 :: 2 :: Nil)) == List.toChain(1 :: 2 :: Nil)
    }

    @test
    def toChain04(): Bool = region r {
        MList.toChain(List.toMList(1 :: 2 :: 3 :: Nil)) == List.toChain(1 :: 2 :: 3 :: Nil)
    }


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
    def sortWith01(): Bool = region r {
        let a = MList.sortWith(cmp, Array.toMList([]: ScopedArray[Int32, _]));
        a `sameElements` Array.toMList([]: ScopedArray[Int32, _])
    }

    @test
    def sortWith02(): Bool = region r {
        let a = MList.sortWith(cmp, Array.toMList([0]));
        a `sameElements` Array.toMList([0])
    }

    @test
    def sortWith03(): Bool = region r {
        let a = MList.sortWith(cmp, Array.toMList([0,1]));
        a `sameElements` Array.toMList([0,1])
    }

    @test
    def sortWith04(): Bool = region r {
        let a = MList.sortWith(cmp, Array.toMList([1,0]));
        a `sameElements` Array.toMList([0,1])
    }

    @test
    def sortWith05(): Bool = region r {
        let a = MList.sortWith(cmp, Array.toMList([1,1]));
        a `sameElements` Array.toMList([1,1])
    }

    @test
    def sortWith06(): Bool = region r {
        let a = MList.sortWith(cmp, Array.toMList([0,1,2,3,4,5]));
        a `sameElements` Array.toMList([0,1,2,3,4,5])
    }

    @test
    def sortWith07(): Bool = region r {
        let a = MList.sortWith(cmp, Array.toMList([5,4,3,2,1,0]));
        a `sameElements` Array.toMList([0,1,2,3,4,5])
    }

    @test
    def sortWith08(): Bool = region r {
        let a = MList.sortWith(cmp, Array.toMList([5,3,0,4,1,2]));
        a `sameElements` Array.toMList([0,1,2,3,4,5])
    }

    @test
    def sortWith09(): Bool = region r {
        let a = MList.sortWith(cmp, Array.toMList([2,3,0,4,1,2]));
        a `sameElements` Array.toMList([0,1,2,2,3,4])
    }

    @test
    def sortWith10(): Bool = region r {
        let a = MList.sortWith(flip(cmp), Array.toMList([0,1,2,3,4,5]));
        a `sameElements` Array.toMList([5,4,3,2,1,0])
    }

    @test
    def sortWith11(): Bool = region r {
        let a = MList.sortWith(flip(cmp), Array.toMList([5,4,3,2,1,0]));
        a `sameElements` Array.toMList([5,4,3,2,1,0])
    }

    @test
    def sortWith12(): Bool = region r {
        let a = MList.sortWith(flip(cmp), Array.toMList([5,3,0,4,1,2]));
        a `sameElements` Array.toMList([5,4,3,2,1,0])
    }


    @test
    def sortWith13(): Bool = region r {
        let a = MList.sortWith(flip(cmp), Array.toMList([2,3,0,4,1,2]));
        a `sameElements` Array.toMList([4,3,2,2,1,0])
    }

    /////////////////////////////////////////////////////////////////////////////
    // sort                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    def testSortVsSortWith(a: MList[Int32, r]): Bool \ { Read(r), Write(r) } =
        MList.sort(a) `sameElements` MList.sortWith(cmp, a)

    @test
    def sort01(): Bool = region r {
        testSortVsSortWith(Array.toMList([]: ScopedArray[Int32, _]))
    }

    @test
    def sort02(): Bool = region r {
        testSortVsSortWith(Array.toMList([0]))
    }

    @test
    def sort03(): Bool = region r {
        testSortVsSortWith(Array.toMList([0,1]))
    }

    @test
    def sort04(): Bool = region r {
        testSortVsSortWith(Array.toMList([1,0]))
    }

    @test
    def sort05(): Bool = region r {
        testSortVsSortWith(Array.toMList([1,1]))
    }

    @test
    def sort06(): Bool = region r {
        testSortVsSortWith(Array.toMList([0,1,2,3,4,5]))
    }

    @test
    def sort07(): Bool = region r {
        testSortVsSortWith(Array.toMList([5,4,3,2,1,0]))
    }

    @test
    def sort08(): Bool = region r {
        testSortVsSortWith(Array.toMList([5,3,0,4,1,2]))
    }

    @test
    def sort09(): Bool = region r {
        testSortVsSortWith(Array.toMList([2,3,0,4,1,2]))
    }

    /////////////////////////////////////////////////////////////////////////////
    // sortWith!                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sortWith01!(): Bool = region r {
        let a = Array.toMList([]: ScopedArray[Int32, _]);
        MList.sortWith(cmp, a);
        a `sameElements` Array.toMList([]: ScopedArray[Int32, _])
    }

    @test
    def sortWith02!(): Bool = region r {
        let a = Array.toMList([0]);
        MList.sortWith!(cmp, a);
        a `sameElements` Array.toMList([0])
    }

    @test
    def sortWith03!(): Bool = region r {
        let a = Array.toMList([0,1]);
        MList.sortWith!(cmp, a);
        a `sameElements` Array.toMList([0,1])
    }

    @test
    def sortWith04!(): Bool = region r {
        let a = Array.toMList([1,0]);
        MList.sortWith!(cmp, a);
        a `sameElements` Array.toMList([0,1])
    }
    @test
    def sortWith05!(): Bool = region r {
        let a = Array.toMList([1,1]);
        MList.sortWith!(cmp, a);
        a `sameElements` Array.toMList([1,1])
    }

    @test
    def sortWith06!(): Bool = region r {
        let a = Array.toMList([0,1,2,3,4,5]);
        MList.sortWith!(cmp, a);
        a `sameElements` Array.toMList([0,1,2,3,4,5])
    }

    @test
    def sortWith07!(): Bool = region r {
        let a = Array.toMList([5,4,3,2,1,0]);
        MList.sortWith!(cmp, a);
        a `sameElements` Array.toMList([0,1,2,3,4,5])
    }

    @test
    def sortWith08!(): Bool = region r {
        let a = Array.toMList([5,3,0,4,1,2]);
        MList.sortWith!(cmp, a);
        a `sameElements` Array.toMList([0,1,2,3,4,5])
    }

    @test
    def sortWith09!(): Bool = region r {
        let a = Array.toMList([2,3,0,4,1,2]);
        MList.sortWith!(cmp, a);
        a `sameElements` Array.toMList([0,1,2,2,3,4])
    }

    @test
    def sortWith10!(): Bool = region r {
        let a = Array.toMList([0,1,2,3,4,5]);
        MList.sortWith!(flip(cmp), a);
        a `sameElements` Array.toMList([5,4,3,2,1,0])
    }

    @test
    def sortWith11!(): Bool = region r {
        let a = Array.toMList([5,4,3,2,1,0]);
        MList.sortWith!(flip(cmp), a);
        a `sameElements` Array.toMList([5,4,3,2,1,0])
    }

    @test
    def sortWith12!(): Bool = region r {
        let a = Array.toMList([5,3,0,4,1,2]);
        MList.sortWith!(flip(cmp), a);
        a `sameElements` Array.toMList([5,4,3,2,1,0])
    }

    @test
    def sortWith13!(): Bool = region r {
        let a = Array.toMList([2,3,0,4,1,2]);
        MList.sortWith!(flip(cmp), a);
        a `sameElements` Array.toMList([4,3,2,2,1,0])
    }

    /////////////////////////////////////////////////////////////////////////////
    // sort!                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    def testSort!VsSortWith!(a: MList[Int32, r]): Bool \ { Read(r), Write(r) } =
        let b = MList.copy(a);
        let c = MList.copy(a);
        MList.sort!(b);
        MList.sortWith!(cmp, c);
        b `sameElements` c

    @test
    def sort!01(): Bool = region r {
        testSort!VsSortWith!(Array.toMList([]: ScopedArray[Int32, _]))
    }

    @test
    def sort!02(): Bool = region r {
        testSort!VsSortWith!(Array.toMList([0]))
    }

    @test
    def sort!03(): Bool = region r {
        testSort!VsSortWith!(Array.toMList([0,1]))
    }

    @test
    def sort!04(): Bool = region r {
        testSort!VsSortWith!(Array.toMList([1,0]))
    }

    @test
    def sort!05(): Bool = region r {
        testSort!VsSortWith!(Array.toMList([1,1]))
    }

    @test
    def sort!06(): Bool = region r {
        testSort!VsSortWith!(Array.toMList([0,1,2,3,4,5]))
    }

    @test
    def sort!07(): Bool = region r {
        testSort!VsSortWith!(Array.toMList([5,4,3,2,1,0]))
    }

    @test
    def sort!08(): Bool = region r {
        testSort!VsSortWith!(Array.toMList([5,3,0,4,1,2]))
    }

    @test
    def sort!09(): Bool = region r {
        testSort!VsSortWith!(Array.toMList([2,3,0,4,1,2]))
    }


    /////////////////////////////////////////////////////////////////////////////
    // sortBy                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    def testSortByVsSort(a: MList[Int32, r]): Bool \ { Read(r), Write(r) } =
        (MList.sortBy(identity, a) `sameElements` MList.sort(a)) and
        (MList.sortBy(x -> 4 * x + 7, a) `sameElements` MList.sort(a)) and
        (MList.sortBy(x -> -x, a) `sameElements` MList.sortWith(flip(cmp),a))

    @test
    def sortBy01(): Bool = region r {
        testSortByVsSort(Array.toMList([]: ScopedArray[Int32, _]))
    }

    @test
    def sortBy02(): Bool = region r {
        testSortByVsSort(Array.toMList([0] @ r))
    }

    @test
    def sortBy03(): Bool = region r {
        testSortByVsSort(Array.toMList([0,1] @ r))
    }

    @test
    def sortBy04(): Bool = region r {
        testSortByVsSort(Array.toMList([1,0] @ r))
    }

    @test
    def sortBy05(): Bool = region r {
        testSortByVsSort(Array.toMList([1,1] @ r))
    }

    @test
    def sortBy06(): Bool = region r {
        testSortByVsSort(Array.toMList([0,1,2,3,4,5] @ r))
    }

    @test
    def sortBy07(): Bool = region r {
        testSortByVsSort(Array.toMList([5,4,3,2,1,0] @ r))
    }

    @test
    def sortBy08(): Bool = region r {
        testSortByVsSort(Array.toMList([5,3,0,4,1,2] @ r))
    }

    @test
    def sortBy09(): Bool = region r {
        testSortByVsSort(Array.toMList([2,3,0,4,1,2] @ r))
    }

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
    def sortBy10(): Bool = region r {
        MList.sortBy(r -> let R(x) = r; x.i, Array.toMList([R({i = 2, s = "A"}), R({i = 1, s = "B"}), R({i = 3, s = "C"})] @ r))
        `sameElements` Array.toMList([R({i = 1, s = "B"}), R({i = 2, s = "A"}), R({i = 3, s = "C"})] @ r)
    }


    /////////////////////////////////////////////////////////////////////////////
    // sortBy!                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    def testSortBy!VsSortBy(a: MList[Int32, r]): Bool \ { Read(r), Write(r) } =
        let b = MList.copy(a);
        let c = MList.copy(a);
        MList.sortBy!(identity, b);
        MList.sortBy!(x -> 4*x+7, c);
        (b `sameElements` MList.sortBy(x -> 4 * x + 7, a)) and
        (c `sameElements` MList.sortBy(identity, a))

    @test
    def sortBy!01(): Bool = region r {
        testSortBy!VsSortBy(Array.toMList([]: ScopedArray[Int32, _]))
    }

    @test
    def sortBy!02(): Bool = region r {
        testSortBy!VsSortBy(Array.toMList([0] @ r))
    }

    @test
    def sortBy!03(): Bool = region r {
        testSortBy!VsSortBy(Array.toMList([0,1] @ r))
    }

    @test
    def sortBy!04(): Bool = region r {
        testSortBy!VsSortBy(Array.toMList([1,0] @ r))
    }

    @test
    def sortBy!05(): Bool = region r {
        testSortBy!VsSortBy(Array.toMList([1,1] @ r))
    }

    @test
    def sortBy!06(): Bool = region r {
        testSortBy!VsSortBy(Array.toMList([0,1,2,3,4,5] @ r))
    }

    @test
    def sortBy!07(): Bool = region r {
        testSortBy!VsSortBy(Array.toMList([5,4,3,2,1,0] @ r))
    }

    @test
    def sortBy!08(): Bool = region r {
        testSortBy!VsSortBy(Array.toMList([5,3,0,4,1,2] @ r))
    }

    @test
    def sortBy!09(): Bool = region r {
        testSortBy!VsSortBy(Array.toMList([2,3,0,4,1,2] @ r))
    }


    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def range01(): Bool = region r {
        let v1 = MList.range(r, 1, 4) |> MList.toArray;
        let v2 = MList.range(r, 0, 5) |> MList.toArray;
        let arr1 = [1, 2, 3] @ r;
        let arr2 = [0, 1, 2, 3, 4] @ r;
        (v1 `Array.sameElements` arr1) and (v2 `Array.sameElements` arr2)
    }

    @test
    def range02(): Bool = region r {
        let v = MList.range(r, -1, 3) |> MList.toArray;
        let arr = [-1, 0, 1, 2] @ r;
        (v `Array.sameElements` arr)
    }

    @test
    def range03(): Bool = region r {
        MList.range(r, 0, 1000) |> MList.toList == List.range(0, 1000)
    }


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sum01(): Bool = region r {
        List.toMList(Nil) |> MList.sum == 0
    }

    @test
    def sum02(): Bool = region r {
        List.toMList(1 :: Nil) |> MList.sum == 1
    }

    @test
    def sum03(): Bool = region r {
        List.toMList(1 :: 2 :: 3 :: Nil) |> MList.sum == 6
    }

    @test
    def sum04(): Bool = region r {
        List.toMList(1 :: 2 :: 3 :: -3 :: Nil) |> MList.sum == 3
    }

    @test
    def sum05(): Bool = region r {
        List.toMList(-1 :: -2 :: -3 :: -4 :: Nil) |> MList.sum == -10
    }

    @test
    def sum06(): Bool = region r {
        List.toMList(10 :: -10 :: Nil) |> MList.sum == 0
    }

    @test
    def sum07(): Bool = region r {
        List.range(1, 101) |> List.toMList |> MList.sum == 5050
    }


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumWith01(): Bool = region r {
        List.toMList(Nil) |> MList.sumWith(x -> x + 1) == 0
    }

    @test
    def sumWith02(): Bool = region r {
        List.toMList(1 :: Nil) |> MList.sumWith(x -> x + 1) == 2
    }

    @test
    def sumWith03(): Bool = region r {
        List.toMList(1 :: 2 :: 3 :: Nil) |> MList.sumWith(x -> x + 1) == 9
    }

    @test
    def sumWith04(): Bool = region r {
        List.toMList(1 :: 2 :: 3 :: -3 :: Nil) |> MList.sumWith(x -> x + 1) == 7
    }

    @test
    def sumWith05(): Bool = region r {
        List.toMList(-1 :: -2 :: -3 :: -4 :: Nil) |> MList.sumWith(x -> x + 1) == -6
    }

    @test
    def sumWith06(): Bool = region r {
        List.toMList(10 :: -10 :: Nil) |> MList.sumWith(x -> x + 1) == 2
    }


    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def join01(): Bool & Impure =
        (Nil: List[Int32]) |> List.toMList |>
            MList.join(",") == ""

    @test
    def join02(): Bool & Impure =
        (1 :: Nil) |> List.toMList |>
            MList.join(",") == "1"

    @test
    def join03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toMList |>
            MList.join(",") == "1,2,3"

    @test
    def join04(): Bool & Impure =
        ("1" :: "2" :: "3" :: Nil) |> List.toMList |>
            MList.join(",") == "1,2,3"


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinWith01(): Bool & Impure =
        List.toMList(Nil: List[Int32]) |> MList.joinWith(x -> "${x + 1}", ",") == ""

    @test
    def joinWith02(): Bool & Impure =
        List.toMList(1 :: Nil) |> MList.joinWith(x -> "${x + 1}", ",") == "2"

    @test
    def joinWith03(): Bool & Impure =
        List.toMList(1 :: 2 :: 3 :: Nil) |> MList.joinWith(x -> "${x + 1}", ",") == "2,3,4"

    @test
    def joinWith04(): Bool & Impure =
        List.toMList("1" :: "2" :: "3" :: Nil) |> MList.joinWith(x -> x + x, ",") == "11,22,33"

/*
    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def iterator01(): Bool = region r {
        let m = new MList(r);
        match Iterable.iterator(m) {
            case Iterator(done, _) => done()
        }
    }

    @test
    def iterator02(): Bool = region r {
        let m = new MList(r);
        MList.push!(1, m);
        match Iterable.iterator(m) {
            case Iterator(done, _) => not done()
        }
    }

    @test
    def iterator03(): Bool = region r {
        let m = new MList(r);
        MList.push!(1, m);
        match Iterable.iterator(m) {
            case Iterator(_, next) => next() == 1
        }
    }

    @test
    def iterator04(): Bool = region r {
        let m = new MList(r);
        MList.push!(1, m);
        MList.push!(2, m);
        match Iterable.iterator(m) {
            case Iterator(done, next) => {
                next();
                next();
                done()
            }
        }
    }

    @test
    def iterator05(): Bool = region r {
        let m = new MList(r);
        MList.push!(1, m);
        MList.push!(2, m);
        match Iterable.iterator(m) {
            case Iterator(_, next) => {
                next();
                next() == 2
            }
        }
    }
*/
}
