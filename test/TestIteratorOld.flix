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
/*
namespace TestIteratorOld {

    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testHead01(): Bool & Impure =
        (Nil: List[Int32] |> IteratorOld.toIter |> IteratorOld.head) == None

    @test
    def testHead02(): Bool & Impure =
        ((1 :: 2 :: Nil): List[Int32] |> IteratorOld.toIter |> IteratorOld.head) == Some(1)


    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def range01(): Bool & Impure =
        IteratorOld.range(0, 0) |> IteratorOld.isEmpty

    @test
    def range02(): Bool & Impure =
        IteratorOld.range(10, 0) |> IteratorOld.isEmpty

    @test
    def range03(): Bool & Impure =
        (IteratorOld.range(1, 4) |> IteratorOld.toList) == (1 :: 2 :: 3 :: Nil)

    @test
    def range04(): Bool & Impure =
        (IteratorOld.range(-1, 2) |> IteratorOld.toList) == (-1 :: 0 :: 1 :: Nil)

    @test
    def range05(): Bool & Impure =
        (IteratorOld.range(-100, 200) |> IteratorOld.toList) == List.range(-100, 200)


    /////////////////////////////////////////////////////////////////////////////
    // repeat                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def repeat01(): Bool & Impure =
        IteratorOld.repeat(10, 0) |> IteratorOld.isEmpty

    @test
    def repeat02(): Bool & Impure =
        IteratorOld.repeat(2, -100) |> IteratorOld.isEmpty

    @test
    def repeat03(): Bool & Impure =
        not (IteratorOld.repeat(2, 100) |> IteratorOld.isEmpty)

    @test
    def repeat04(): Bool & Impure =
        (IteratorOld.repeat(1, 3) |> IteratorOld.toList) == (1 :: 1 :: 1 :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def memberOf01(): Bool & Impure =
        not (Nil: List[Int32] |> IteratorOld.toIter |> IteratorOld.memberOf(1))

    @test
    def memberOf02(): Bool & Impure =
        let iter = IteratorOld.range(-10, 2);
        iter |> IteratorOld.memberOf(1) and
        iter |> IteratorOld.isEmpty

    @test
    def memberOf03(): Bool & Impure =
        let el = "A";
        let iter = IteratorOld.repeat(el, 100);
        not (iter |> IteratorOld.memberOf("Not A")) and
        iter |> IteratorOld.isEmpty

    @test
    def memberOf04(): Bool & Impure =
        let iter = IteratorOld.range(0, 1000);
        iter |> IteratorOld.memberOf(0) and
        iter |> IteratorOld.memberOf(500) and
        not (iter |> IteratorOld.isEmpty)

    @test
    def memberOf05(): Bool & Impure =
        let iter = IteratorOld.range(0, 1000);
        not (iter |> IteratorOld.memberOf(-1)) and
        not (iter |> IteratorOld.memberOf(500)) and
        iter |> IteratorOld.isEmpty

    @test
    def memberOf06(): Bool & Impure =
        let iter = IteratorOld.range(0, 1000);
        not (iter |> IteratorOld.memberOf(1000)) and
        not (iter |> IteratorOld.memberOf(500)) and
        iter |> IteratorOld.isEmpty

    @test
    def memberOf07(): Bool & Impure =
        let iter = IteratorOld.range(0, 1000);
        iter |> IteratorOld.memberOf(999) and
        iter |> IteratorOld.isEmpty


    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toArray01(): Bool & Impure =
        (Nil: List[Int32] |> IteratorOld.toIter |> IteratorOld.toArray) == []

    @test
    def toArray02(): Bool & Impure =
        let iter = IteratorOld.range(-10, -5);
        (iter |> IteratorOld.toArray) == [-10, -9, -8, -7, -6] and
        iter |> IteratorOld.isEmpty

    @test
    def toArray03(): Bool & Impure =
        let iter = IteratorOld.repeat(1, 3);
        (iter |> IteratorOld.toArray) == [1, 1, 1] and
        iter |> IteratorOld.isEmpty

    @test
    def toArray04(): Bool & Impure =
        let iter = IteratorOld.range(-100, 200);
        (iter |> IteratorOld.toArray) == (List.range(-100, 200) |> List.toArray) and
        iter |> IteratorOld.isEmpty


    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMap01(): Bool & Impure =
        (Nil: List[(String, Int32)] |> IteratorOld.toIter |> IteratorOld.toMap) == Map.empty()

    @test
    def toMap02(): Bool & Impure =
        let list = ("A", 1) :: ("B", 2) :: ("C", 3) :: ("D", 4) :: Nil;
        let iter = list |> IteratorOld.toIter;
        iter |> IteratorOld.toMap == list |> List.toMap and
        iter |> IteratorOld.isEmpty

    @test
    def toMap03(): Bool & Impure =
        let map = Map.empty() |> Map.insert("1", 2) |> Map.insert("2", 3) |> Map.insert("3", 4);
        let iter = map |> Map.toList |> IteratorOld.toIter;
        iter |> IteratorOld.toMap == map and
        iter |> IteratorOld.isEmpty


    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toSet01(): Bool & Impure =
        Nil: List[Int32] |> IteratorOld.toIter |> IteratorOld.toSet == Set.empty()

    @test
    def toSet02(): Bool & Impure =
        let xs = ("a" :: "b" :: "b" :: "c" :: Nil);
        let iter = xs |> IteratorOld.toIter;
        iter |> IteratorOld.toSet == List.toSet(xs) and
        iter |> IteratorOld.isEmpty

    @test
    def toSet03(): Bool & Impure =
        let xs = Set.range(0, 10000);
        let iter = xs |> Set.toList |> IteratorOld.toIter;
        iter |> IteratorOld.toSet == xs and
        iter |> IteratorOld.isEmpty


    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def replace01(): Bool & Impure =
        Nil: List[Int32] |> IteratorOld.toIter |> IteratorOld.toList ==
        Nil: List[Int32] |> IteratorOld.toIter |> IteratorOld.replace(1, 2) |> IteratorOld.toList

    @test
    def replace02(): Bool & Impure =
        IteratorOld.repeat("a", 10) |> IteratorOld.replace("a", "b") |> IteratorOld.toList == List.repeat("b", 10)

    @test
    def replace03(): Bool & Impure =
        let iter = IteratorOld.range(0, 10);
        not (iter |> IteratorOld.isEmpty) and
        not (iter |> IteratorOld.replace(5, 100) |> IteratorOld.isEmpty)

    @test
    def replace04(): Bool & Impure =
        let iter = IteratorOld.range(0, 10) |> IteratorOld.replace(5, 100);
        not (iter |> IteratorOld.memberOf(5)) and iter |> IteratorOld.isEmpty

    @test
    def replace05(): Bool & Impure =
        let iter = IteratorOld.range(0, 10) |> IteratorOld.replace(5, 100);
        iter |> IteratorOld.memberOf(100) and not (iter |> IteratorOld.isEmpty)

    @test
    def replace06(): Bool & Impure =
        let list = "a" :: "b" :: "c" :: Nil;
        list |> IteratorOld.toIter |> IteratorOld.replace("c", "d") |> IteratorOld.toList == ("a" :: "b" :: "d" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def exists01(): Bool & Impure =
        not (IteratorOld.range(0, 0) |> IteratorOld.exists(_ -> true)) and
        not (IteratorOld.range(0, 0) |> IteratorOld.exists(_ -> false))

    @test
    def exists02(): Bool & Impure =
        let iter = IteratorOld.range(0, 1);
        iter |> IteratorOld.exists(x -> x == 0) and
        iter |> IteratorOld.isEmpty and
        not (iter |> IteratorOld.exists(_ -> true))

    @test
    def exists03(): Bool & Impure =
        let iter = ("a" :: "b" :: "c" :: Nil): List[String] |> IteratorOld.toIter;
        iter |> IteratorOld.exists(x -> x == "b") and
        not (iter |> IteratorOld.isEmpty) and
        iter |> IteratorOld.toList == ("c" :: Nil)

    @test
    def exists04(): Bool & Impure =
        let iter = ("a" :: "b" :: "c" :: Nil): List[String] |> IteratorOld.toIter;
        not (iter |> IteratorOld.exists(x -> x == "1000")) and
        iter |> IteratorOld.isEmpty

    @test
    def exists05(): Bool & Impure =
        let iter = IteratorOld.range(0, 1000);
        not (iter |> IteratorOld.exists(_ -> false)) and
        iter |> IteratorOld.isEmpty

    @test
    def exists06(): Bool & Impure =
        let iter = IteratorOld.range(0, 1000);
        iter |> IteratorOld.exists(_ -> true) and
        not (iter |> IteratorOld.isEmpty) and
        iter |> IteratorOld.toList == List.range(1, 1000)

    @test
    def exists07(): Bool & Impure =
        let iter = IteratorOld.repeat(2, 1000);
        iter |> IteratorOld.exists(x -> x == 2) and
        not (iter |> IteratorOld.isEmpty) and
        iter |> IteratorOld.toList == List.repeat(2, 999)

    @test
    def exists08(): Bool & Impure =
        let iter = IteratorOld.repeat(2, 1000);
        not (iter |> IteratorOld.exists(x -> x == 1)) and
        iter |> IteratorOld.isEmpty


    /////////////////////////////////////////////////////////////////////////////
    // forall                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forall01(): Bool & Impure =
        IteratorOld.range(0, 0) |> IteratorOld.forall(_ -> false) and
        IteratorOld.range(0, 0) |> IteratorOld.forall(_ -> true)

    @test
    def forall02(): Bool & Impure =
        let iter = IteratorOld.range(0, 1);
        iter |> IteratorOld.forall(x -> x == 0) and
        iter |> IteratorOld.isEmpty and
        iter |> IteratorOld.forall(_ -> false)

    @test
    def forall03(): Bool & Impure =
        let iter = ("a" :: "b" :: "c" :: Nil): List[String] |> IteratorOld.toIter;
        not (iter |> IteratorOld.forall(x -> x == "b")) and
        not (iter |> IteratorOld.isEmpty) and
        iter |> IteratorOld.toList == ("b" :: "c" :: Nil)

    @test
    def forall04(): Bool & Impure =
        let iter = ("a" :: "b" :: "c" :: Nil): List[String] |> IteratorOld.toIter;
        not (iter |> IteratorOld.forall(x -> x == "1000")) and
        not (iter |> IteratorOld.isEmpty)

    @test
    def forall05(): Bool & Impure =
        let iter = IteratorOld.range(0, 1000);
        not (iter |> IteratorOld.forall(_ -> false)) and
        not (iter |> IteratorOld.isEmpty) and
        iter |> IteratorOld.toList == List.range(1, 1000)

    @test
    def forall06(): Bool & Impure =
        let iter = IteratorOld.range(0, 1000);
        iter |> IteratorOld.forall(_ -> true) and
        iter |> IteratorOld.isEmpty

    @test
    def forall07(): Bool & Impure =
        let iter = IteratorOld.repeat(2, 1000);
        iter |> IteratorOld.forall(x -> x == 2) and
        iter |> IteratorOld.isEmpty

    @test
    def forall08(): Bool & Impure =
        let iter = IteratorOld.repeat(2, 1000);
        not (iter |> IteratorOld.forall(x -> x == 1)) and
        not (iter |> IteratorOld.isEmpty) and
        iter |> IteratorOld.toList == List.repeat(2, 999)


    /////////////////////////////////////////////////////////////////////////////
    // dropWhile (pure)                                                        //
    /////////////////////////////////////////////////////////////////////////////
    /*

    @test
    def dropWhilePure01(): Bool & Impure =
        let list: List[Int32] = Nil;
        list |> IteratorOld.toIter |> IteratorOld.dropWhile(_ -> true)  |> IteratorOld.toList ==  list and
        list |> IteratorOld.toIter |> IteratorOld.dropWhile(_ -> false) |> IteratorOld.toList ==  list

    @test
    def dropWhilePure02(): Bool & Impure =
        let iter = () -> IteratorOld.range(0, 1000);
        iter() |> IteratorOld.dropWhile(_ -> false) |> IteratorOld.toList == List.range(0, 1000)

    @test
    def dropWhilePure03(): Bool & Impure =
        let iter = () -> IteratorOld.range(0, 1000);
        iter() |> IteratorOld.dropWhile(_ -> true) |> IteratorOld.isEmpty

    @test
    def dropWhilePure04(): Bool & Impure =
        let iter = () -> IteratorOld.range(0, 1000);
        iter() |> IteratorOld.dropWhile(x -> x < 500) |> IteratorOld.toList == List.range(500, 1000)

    @test
    def dropWhilePure05(): Bool & Impure =
        let iter = () -> IteratorOld.repeat(2, 1000);
        iter() |> IteratorOld.dropWhile(x -> x < 3) |> IteratorOld.isEmpty

    @test
    def dropWhilePure06(): Bool & Impure =
        let iter = () -> ("a" :: "a" :: "b" :: "b" :: "c" :: "c" :: Nil) |> IteratorOld.toIter;
        iter() |> IteratorOld.dropWhile(x -> x == "a") |> IteratorOld.toList == ("b" :: "b" :: "c" :: "c" :: Nil)

    @test
    def dropWhilePure07(): Bool & Impure =
        let iter = () -> ("a" :: "a" :: "b" :: "b" :: "c" :: "c" :: Nil) |> IteratorOld.toIter;
        iter() |> IteratorOld.dropWhile(x -> x == "b") |> IteratorOld.toList == iter() |> IteratorOld.toList

    @test
    def dropWhilePure08(): Bool & Impure =
        let iter = () -> ("a" :: "a" :: "b" :: "b" :: "c" :: "c" :: Nil) |> IteratorOld.toIter;
        iter() |> IteratorOld.dropWhile(x -> x == "a" or x == "c") |> IteratorOld.toList == ("b" :: "b" :: "c" :: "c" :: Nil)

    @test
    def dropWhilePure09(): Bool & Impure =
        let iter = () -> ("a" :: "a" :: "b" :: "b" :: "c" :: "c" :: Nil) |> IteratorOld.toIter;
        iter() |> IteratorOld.dropWhile(x -> x == "a" or x == "b") |> IteratorOld.toList == List.repeat("c", 2)


    /////////////////////////////////////////////////////////////////////////////
    // dropWhile (impure)                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhileImpure01(): Bool & Impure =
        let list: List[Int32] = Nil;
        list |> IteratorOld.toIter |> IteratorOld.dropWhile((_ -> true) as & Impure)  |> IteratorOld.toList ==  list and
        list |> IteratorOld.toIter |> IteratorOld.dropWhile((_ -> false) as & Impure) |> IteratorOld.toList ==  list

    @test
    def dropWhileImpure02(): Bool & Impure =
        let iter = () -> IteratorOld.range(0, 1000);
        iter() |> IteratorOld.dropWhile((_ -> false) as & Impure) |> IteratorOld.toList == List.range(0, 1000)

    @test
    def dropWhileImpure03(): Bool & Impure =
        let iter = () -> IteratorOld.range(0, 1000);
        iter() |> IteratorOld.dropWhile((_ -> true) as & Impure) |> IteratorOld.isEmpty

    @test
    def dropWhileImpure04(): Bool & Impure =
        let iter = () -> IteratorOld.range(0, 1000);
        iter() |> IteratorOld.dropWhile((x -> x < 500) as & Impure) |> IteratorOld.toList == List.range(500, 1000)

    @test
    def dropWhileImpure05(): Bool & Impure =
        let iter = () -> IteratorOld.repeat(2, 1000);
        iter() |> IteratorOld.dropWhile((x -> x < 3) as & Impure) |> IteratorOld.isEmpty

    @test
    def dropWhileImpure06(): Bool & Impure =
        let iter = () -> ("a" :: "a" :: "b" :: "b" :: "c" :: "c" :: Nil) |> IteratorOld.toIter;
        iter() |> IteratorOld.dropWhile((x -> x == "a") as & Impure) |> IteratorOld.toList == ("b" :: "b" :: "c" :: "c" :: Nil)

    @test
    def dropWhileImpure07(): Bool & Impure =
        let iter = () -> ("a" :: "a" :: "b" :: "b" :: "c" :: "c" :: Nil) |> IteratorOld.toIter;
        iter() |> IteratorOld.dropWhile((x -> x == "b") as & Impure) |> IteratorOld.toList == iter() |> IteratorOld.toList

    @test
    def dropWhileImpure08(): Bool & Impure =
        let iter = () -> ("a" :: "a" :: "b" :: "b" :: "c" :: "c" :: Nil) |> IteratorOld.toIter;
        iter() |> IteratorOld.dropWhile((x -> x == "a" or x == "c") as & Impure) |> IteratorOld.toList == ("b" :: "b" :: "c" :: "c" :: Nil)

    @test
    def dropWhileImpure09(): Bool & Impure =
        let iter = () -> ("a" :: "a" :: "b" :: "b" :: "c" :: "c" :: Nil) |> IteratorOld.toIter;
        iter() |> IteratorOld.dropWhile((x -> x == "a" or x == "b") as & Impure) |> IteratorOld.toList == List.repeat("c", 2)


    /////////////////////////////////////////////////////////////////////////////
    // foreach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foreach01(): Bool & Impure =
        let l: List[Int32] = Nil;
        let ml: MutList[Int32] = MutList.new();
        l |> IteratorOld.toIter |> IteratorOld.foreach(x -> ml |> MutList.push!(x));
        ml |> MutList.toImm == l

    @test
    def foreach02(): Bool & Impure =
        let iter = IteratorOld.range(0, 10000);
        let ml: MutList[Int32] = MutList.new();
        iter |> IteratorOld.foreach(x -> ml |> MutList.push!(x));
        ml |> MutList.toImm == List.range(0, 10000) and
        iter |> IteratorOld.isEmpty

    @test
    def foreach03(): Bool & Impure =
        let iter = IteratorOld.range(0, 10000);
        let x = ref 0;
        iter |> IteratorOld.foreach(_ -> x := (deref x) + 1);
        deref x == 10000 and iter |> IteratorOld.isEmpty

*/
    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////
/*
    @test
    def zip01(): Bool & Impure =
        let iterA = Nil: List[Int32] |> IteratorOld.toIter;
        let iterB = IteratorOld.repeat("b", 1000);
        IteratorOld.zip(iterA, iterB) |> IteratorOld.isEmpty and
        IteratorOld.zip(iterA, iterB) |> IteratorOld.toList == Nil

    @test
    def zip02(): Bool & Impure =
        let iterA = Nil: List[Int32] |> IteratorOld.toIter;
        let iterB = IteratorOld.repeat("b", 1000);
        let zipped = IteratorOld.zip(iterB, iterA);
        zipped |> IteratorOld.isEmpty and
        zipped |> IteratorOld.toList == Nil

    @test
    def zip03(): Bool & Impure =
        let iterA = Nil: List[Int32] |> IteratorOld.toIter;
        let zipped = IteratorOld.zip(iterA, iterA);
        zipped |> IteratorOld.isEmpty and
        zipped |> IteratorOld.toList == Nil

    @test
    def zip04(): Bool & Impure =
        let iterA = IteratorOld.repeat("a", 500);
        let iterB = IteratorOld.repeat("b", 1000);
        IteratorOld.zip(iterA, iterB) |> IteratorOld.toList == List.repeat(("a", "b"), 500) and
        IteratorOld.zip(iterA, iterB) |> IteratorOld.isEmpty and
        IteratorOld.zip(iterB, iterA) |> IteratorOld.isEmpty

    @test
    def zip05(): Bool & Impure =
        let iterA = IteratorOld.repeat("a", 1000);
        let iterB = IteratorOld.repeat("b", 500);
        IteratorOld.zip(iterA, iterB) |> IteratorOld.toList == List.repeat(("a", "b"), 500) and
        IteratorOld.zip(iterA, iterB) |> IteratorOld.isEmpty and
        IteratorOld.zip(iterB, iterA) |> IteratorOld.isEmpty

    @test
    def zip06(): Bool & Impure =
        let iterA = IteratorOld.repeat("a", 1000);
        let iterB = IteratorOld.repeat("b", 1000);
        IteratorOld.zip(iterA, iterB) |> IteratorOld.toList == List.repeat(("a", "b"), 1000) and
        IteratorOld.zip(iterA, iterB) |> IteratorOld.isEmpty and
        IteratorOld.zip(iterB, iterA) |> IteratorOld.isEmpty

    @test
    def zip07(): Bool & Impure =
        let iterA = IteratorOld.range(0, 500);
        let iterB = IteratorOld.repeat("b", 1000);
        let zipped = IteratorOld.zip(iterA, iterB);
        let list = List.zip(List.range(0, 500), List.repeat("b", 500));
        zipped |> IteratorOld.toList == list and
        zipped |> IteratorOld.isEmpty and
        IteratorOld.zip(iterB, iterA) |> IteratorOld.isEmpty

    @test
    def zip08(): Bool & Impure =
        let iter = IteratorOld.range(0, 500);
        let listEven = List.range(0, 500) |> List.filter(x -> x % 2 == 0);
        let listOdd = List.range(0, 500) |> List.filter(x -> x % 2 != 0);
        IteratorOld.zip(iter, iter) |> IteratorOld.toList == List.zip(listEven, listOdd) and
        iter |> IteratorOld.isEmpty


    /////////////////////////////////////////////////////////////////////////////
    // zipWith (pure)                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWithPure01(): Bool & Impure =
        let iterA = IteratorOld.repeat(0, 0);
        let iterB = IteratorOld.repeat(0, 0);
        IteratorOld.zipWith((x, y) -> x + y, iterA, iterB) |> IteratorOld.toList == List.repeat(0, 0)

    @test
    def zipWithPure02(): Bool & Impure =
        let iterA = IteratorOld.repeat(1, 1000);
        let iterB = IteratorOld.repeat(0, 0);
        IteratorOld.zipWith((x, y) -> x + y, iterA, iterB) |> IteratorOld.toList == List.repeat(0, 0)

    @test
    def zipWithPure03(): Bool & Impure =
        let iterA = IteratorOld.repeat(0, 0);
        let iterB = IteratorOld.repeat(1, 1000);
        IteratorOld.zipWith((x, y) -> x + y, iterA, iterB) |> IteratorOld.toList == List.repeat(0, 0)

    @test
    def zipWithPure04(): Bool & Impure =
        let iterA = IteratorOld.range(0, 1000);
        let iterB = IteratorOld.range(0, 1000);
        IteratorOld.zipWith((x, y) -> x + y, iterA, iterB) |> IteratorOld.toList == List.range(0, 1000) |> List.map(x -> 2 * x)


    /////////////////////////////////////////////////////////////////////////////
    // zipWith (impure)                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWithImpure01(): Bool & Impure =
        let iterA = IteratorOld.repeat(0, 0);
        let iterB = IteratorOld.repeat(0, 0);
        IteratorOld.zipWith((x, y) -> x + y as & Impure, iterA, iterB) |> IteratorOld.toList == List.repeat(0, 0)

    @test
    def zipWithImpure02(): Bool & Impure =
        let iterA = IteratorOld.repeat(1, 1000);
        let iterB = IteratorOld.repeat(0, 0);
        IteratorOld.zipWith((x, y) -> x + y as & Impure, iterA, iterB) |> IteratorOld.toList == List.repeat(0, 0)

    @test
    def zipWithImpure03(): Bool & Impure =
        let iterA = IteratorOld.repeat(0, 0);
        let iterB = IteratorOld.repeat(1, 1000);
        IteratorOld.zipWith((x, y) -> x + y as & Impure, iterA, iterB) |> IteratorOld.toList == List.repeat(0, 0)

    @test
    def zipWithImpure04(): Bool & Impure =
        let iterA = IteratorOld.range(0, 1000);
        let iterB = IteratorOld.range(0, 1000);
        IteratorOld.zipWith((x, y) -> x + y as & Impure, iterA, iterB) |> IteratorOld.toList == List.range(0, 1000) |> List.map(x -> 2 * x)


    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool & Impure =
        Nil: List[Int32] |> IteratorOld.toIter |> IteratorOld.foldLeft((acc, n) -> n :: acc, Nil: List[Int32]) == Nil: List[Int32]

    @test
    def foldLeft02(): Bool & Impure =
        IteratorOld.range(0, 1000) |> IteratorOld.foldLeft((acc, n) -> n :: acc, Nil: List[Int32]) ==
            List.range(0, 1000) |> List.reverse

    @test
    def foldLeft03(): Bool & Impure =
        let iter = IteratorOld.range(0, 1000);
        iter |> IteratorOld.foldLeft((_, _) -> true, true);
        iter |> IteratorOld.isEmpty


    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool & Impure =
        Nil: List[Int32] |> IteratorOld.toIter |> IteratorOld.foldRight((n, acc) -> n :: acc, Nil: List[Int32]) == Nil: List[Int32]

    @test
    def foldRight02(): Bool & Impure =
        IteratorOld.range(0, 1000) |> IteratorOld.foldRight((n, acc) -> n :: acc, Nil: List[Int32]) ==
            List.range(0, 1000)

    @test
    def foldRight03(): Bool & Impure =
        let iter = IteratorOld.range(0, 1000);
        iter |> IteratorOld.foldRight((_, _) -> true, true);
        iter |> IteratorOld.isEmpty


    /////////////////////////////////////////////////////////////////////////////
    // takeWhile (pure)                                                        //
    /////////////////////////////////////////////////////////////////////////////
/*
    @test
    def takeWhilePure01(): Bool & Impure =
        IteratorOld.repeat(0, 0) |> IteratorOld.takeWhile(x -> x > 100) |> IteratorOld.toList == Nil: List[Int32]

    @test
    def takeWhilePure02(): Bool & Impure =
        IteratorOld.range(0, 1000) |> IteratorOld.takeWhile(x -> x > 100) |> IteratorOld.toList == Nil: List[Int32]

    @test
    def takeWhilePure03(): Bool & Impure =
        IteratorOld.range(0, 1000) |> IteratorOld.takeWhile(x -> x < 100) |> IteratorOld.toList == List.range(0, 100)

    @test
    def takeWhilePure04(): Bool & Impure =
        IteratorOld.repeat(0, 0) |> IteratorOld.takeWhile(x -> x < 100) |> IteratorOld.toList == Nil: List[Int32]

    @test
    def takeWhilePure05(): Bool & Impure =
        IteratorOld.repeat(10, 1000) |> IteratorOld.takeWhile(x -> x < 100) |> IteratorOld.toList == List.repeat(10, 1000)

    @test
    def takeWhilePure06(): Bool & Impure =
        let iter = (1 :: 2 :: 1 :: Nil): List[Int32] |> IteratorOld.toIter |> IteratorOld.takeWhile(x -> x == 1);
        iter |> IteratorOld.head == Some(1)
            and iter |> IteratorOld.head == None
            and iter |> IteratorOld.head == None

    @test
    def takeWhilePure07(): Bool & Impure =
        let iter = IteratorOld.range(1, 10000) |> IteratorOld.replace(10, 1) |> IteratorOld.takeWhile(x -> x == 1);
        let IteratorOld(done, _) = iter;
        iter |> IteratorOld.toList == List.repeat(1, 1)
            and done() and done() and done() and done() and done()


    /////////////////////////////////////////////////////////////////////////////
    // takeWhile (impure)                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhileImpure01(): Bool & Impure =
        IteratorOld.repeat(0, 0) |> IteratorOld.takeWhile((x -> x > 100) as & Impure) |> IteratorOld.toList == Nil: List[Int32]

    @test
    def takeWhileImpure02(): Bool & Impure =
        IteratorOld.range(0, 1000) |> IteratorOld.takeWhile((x -> x > 100) as & Impure) |> IteratorOld.toList == Nil: List[Int32]

    @test
    def takeWhileImpure03(): Bool & Impure =
        IteratorOld.range(0, 1000) |> IteratorOld.takeWhile((x -> x < 100) as & Impure) |> IteratorOld.toList == List.range(0, 100)

    @test
    def takeWhileImpure04(): Bool & Impure =
        IteratorOld.repeat(0, 0) |> IteratorOld.takeWhile((x -> x < 100) as & Impure) |> IteratorOld.toList == Nil: List[Int32]

    @test
    def takeWhileImpure05(): Bool & Impure =
        IteratorOld.repeat(10, 1000) |> IteratorOld.takeWhile({x -> x < 100 } as & Impure) |> IteratorOld.toList == List.repeat(10, 1000)
*/

/*
    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findRight01(): Bool & Impure =
        IteratorOld.repeat(0, 0) |> IteratorOld.findRight(_ -> true) == None

    @test
    def findRight02(): Bool & Impure =
        IteratorOld.range(0, 1000) |> IteratorOld.findRight(_ -> true) == Some(999)

    @test
    def findRight03(): Bool & Impure =
        IteratorOld.range(0, 1000) |> IteratorOld.findRight(x -> x == 500) == Some(500)

    @test
    def findRight04(): Bool & Impure =
        IteratorOld.range(0, 1000) |> IteratorOld.findRight(x -> x == -1) == None


    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testIsEmpty01(): Bool & Impure =
        Nil |> IteratorOld.toIter |> IteratorOld.isEmpty

    @test
    def testIsEmpty02(): Bool & Impure =
        not ((1 :: Nil) |> IteratorOld.toIter |> IteratorOld.isEmpty)

    @test
    def testIsEmpty03(): Bool & Impure =
        not ((1 :: 2 :: Nil) |> IteratorOld.toIter |> IteratorOld.isEmpty)

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testCount01(): Bool & Impure =
        Nil |> IteratorOld.toIter |> IteratorOld.count(constant(true)) == 0

    @test
    def testCount02(): Bool & Impure =
        (1 :: Nil) |> IteratorOld.toIter |> IteratorOld.count(constant(true)) == 1

    @test
    def testCount03(): Bool & Impure =
        (1 :: 2 :: Nil) |> IteratorOld.toIter |> IteratorOld.count(constant(true)) == 2

    @test
    def testCount04(): Bool & Impure =
        Nil |> IteratorOld.toIter |> IteratorOld.count(constant(false)) == 0

    @test
    def testCount05(): Bool & Impure =
        (1 :: Nil) |> IteratorOld.toIter |> IteratorOld.count(constant(false)) == 0

    @test
    def testCount06(): Bool & Impure =
        (1 :: 2 :: Nil) |> IteratorOld.toIter |> IteratorOld.count(constant(false)) == 0

    @test
    def testCount07(): Bool & Impure =
        (1 :: 2 :: 3 ::  Nil) |> IteratorOld.toIter |> IteratorOld.count(x -> x < 2) == 1


    /////////////////////////////////////////////////////////////////////////////
    // drop                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testDrop01(): Bool & Impure =
        Nil |> IteratorOld.toIter |> IteratorOld.drop(0) |> IteratorOld.isEmpty

    @test
    def testDrop02(): Bool & Impure =
        ((1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |> IteratorOld.drop(0) |> IteratorOld.toList) == 1 :: 2 :: 3 :: Nil

    @test
    def testDrop03(): Bool & Impure =
        ((1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |> IteratorOld.drop(1) |> IteratorOld.toList) == 2 :: 3 :: Nil

    @test
    def testDrop04(): Bool & Impure =
        ((1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |> IteratorOld.drop(2) |> IteratorOld.toList) == 3 :: Nil

    @test
    def testDrop05(): Bool & Impure =
        ((1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |> IteratorOld.drop(3) |> IteratorOld.toList) == Nil

    @test
    def testDrop06(): Bool & Impure =
        ((1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |> IteratorOld.drop(1000) |> IteratorOld.toList) == Nil

    /////////////////////////////////////////////////////////////////////////////
    // take                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testTake01(): Bool & Impure =
        Nil |> IteratorOld.toIter |> IteratorOld.take(0) |> IteratorOld.isEmpty

    @test
    def testTake02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |> IteratorOld.take(0) |> IteratorOld.toList == Nil

    @test
    def testTake03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |> IteratorOld.take(1) |> IteratorOld.toList == 1 :: Nil

    @test
    def testTake04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |> IteratorOld.take(2) |> IteratorOld.toList == 1 :: 2 :: Nil

    @test
    def testTake05(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |> IteratorOld.take(3) |> IteratorOld.toList == 1 :: 2 :: 3 :: Nil

    @test
    def testTake06(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |> IteratorOld.take(1000) |> IteratorOld.toList == 1 :: 2 :: 3 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // map (pure)                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testMapPure01(): Bool & Impure =
        Nil |> IteratorOld.toIter |> IteratorOld.map(x -> x + 1) |> IteratorOld.toList == Nil

    @test
    def testMapPure02(): Bool & Impure =
        (1 :: Nil) |> IteratorOld.toIter |> IteratorOld.map(x -> x + 1) |> IteratorOld.toList == 2 :: Nil

    @test
    def testMapPure03(): Bool & Impure =
        (1 :: 2 :: Nil) |> IteratorOld.toIter |> IteratorOld.map(x -> x + 1) |> IteratorOld.toList == 2 :: 3 :: Nil

    @test
    def testMapPure04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |> IteratorOld.map(x -> x + 1) |> IteratorOld.toList == 2 :: 3 :: 4 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // map (impure)                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testMapImpure01(): Bool & Impure =
        Nil |> IteratorOld.toIter |> IteratorOld.map(x -> x + 1 as & Impure) |> IteratorOld.toList == Nil

    @test
    def testMapImpure02(): Bool & Impure =
        (1 :: Nil) |> IteratorOld.toIter |> IteratorOld.map(x -> x + 1 as & Impure) |> IteratorOld.toList == 2 :: Nil

    @test
    def testMapImpure03(): Bool & Impure =
        (1 :: 2 :: Nil) |> IteratorOld.toIter |> IteratorOld.map(x -> x + 1 as & Impure) |> IteratorOld.toList == 2 :: 3 :: Nil

    @test
    def testMapImpure04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |> IteratorOld.map(x -> x + 1 as & Impure) |> IteratorOld.toList == 2 :: 3 :: 4 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // map map                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testMapMap01(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |> IteratorOld.map(x -> x + 1) |> IteratorOld.map(x -> x * 2) |> IteratorOld.toList == 4 :: 6 :: 8 :: Nil

    @test
    def testMapMap02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |> IteratorOld.map(x -> x + 1 as & Impure) |> IteratorOld.map(x -> x * 2) |> IteratorOld.toList == 4 :: 6 :: 8 :: Nil

    @test
    def testMapMap03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |> IteratorOld.map(x -> x + 1) |> IteratorOld.map(x -> x * 2 as & Impure) |> IteratorOld.toList == 4 :: 6 :: 8 :: Nil

    @test
    def testMapMap04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |> IteratorOld.map(x -> x + 1 as & Impure) |> IteratorOld.map(x -> x * 2 as & Impure) |> IteratorOld.toList == 4 :: 6 :: 8 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // map map fusion                                                          //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testMapFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |>
        IteratorOld.map(x -> {l := "a" :: deref l; x}) |>
        IteratorOld.map(x -> {l := "b" :: deref l; x}) |>
        IteratorOld.toList;
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def testMapFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |>
        IteratorOld.map(x -> {l := "a" :: deref l; x} as & Pure) |>
        IteratorOld.map(x -> {l := "b" :: deref l; x} as & Pure) |>
        IteratorOld.toList;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // filter (pure)                                                           //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testFilterPure01(): Bool & Impure =
        Nil |> IteratorOld.toIter |> IteratorOld.filter(x -> x > 100) |> IteratorOld.toList == Nil

    @test
    def testFilterPure02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |> IteratorOld.filter(x -> x > 0) |> IteratorOld.toList == 1 :: 2 :: 3 :: Nil

    @test
    def testFilterPure03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |> IteratorOld.filter(x -> x > 1) |> IteratorOld.toList == 2 :: 3 :: Nil

    @test
    def testFilterPure04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |> IteratorOld.filter(x -> x > 2) |> IteratorOld.toList == 3 :: Nil

    @test
    def testFilterPure05(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |> IteratorOld.filter(x -> x > 3) |> IteratorOld.toList == Nil

    @test
    def testFilterPure06(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |> IteratorOld.filter(x -> x > 100) |> IteratorOld.toList == Nil

    /////////////////////////////////////////////////////////////////////////////
    // filter (impure)                                                         //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testFilterImpure01(): Bool & Impure =
        Nil |> IteratorOld.toIter |> IteratorOld.filter(x -> x > 100 as & Impure) |> IteratorOld.toList == Nil

    @test
    def testFilterImpure02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |> IteratorOld.filter(x -> x > 0 as & Impure) |> IteratorOld.toList == 1 :: 2 :: 3 :: Nil

    @test
    def testFilterImpure03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |> IteratorOld.filter(x -> x > 1 as & Impure) |> IteratorOld.toList == 2 :: 3 :: Nil

    @test
    def testFilterImpure04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |> IteratorOld.filter(x -> x > 2 as & Impure) |> IteratorOld.toList == 3 :: Nil

    @test
    def testFilterImpure05(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |> IteratorOld.filter(x -> x > 3 as & Impure) |> IteratorOld.toList == Nil

    @test
    def testFilterImpure06(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |> IteratorOld.filter(x -> x > 100 as & Impure) |> IteratorOld.toList == Nil

    /////////////////////////////////////////////////////////////////////////////
    // filter filter                                                           //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testFilterFilter01(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |> IteratorOld.filter(x -> x > 1) |> IteratorOld.filter(x -> x < 3) |> IteratorOld.toList == 2 :: Nil

    @test
    def testFilterFilter02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |> IteratorOld.filter(x -> x > 1 as & Impure) |> IteratorOld.filter(x -> x < 3) |> IteratorOld.toList == 2 :: Nil

    @test
    def testFilterFilter03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |> IteratorOld.filter(x -> x > 1) |> IteratorOld.filter(x -> x < 3 as & Impure) |> IteratorOld.toList == 2 :: Nil

    @test
    def testFilterFilter04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |> IteratorOld.filter(x -> x > 1 as & Impure) |> IteratorOld.filter(x -> x < 3 as & Impure) |> IteratorOld.toList == 2 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // filter filter fusion                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testFilterFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |>
        IteratorOld.filter(_x -> {l := "a" :: deref l; true}) |>
        IteratorOld.filter(_x -> {l := "b" :: deref l; true}) |>
        IteratorOld.toList;
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def testFilterFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |>
        IteratorOld.filter(_x -> {l := "a" :: deref l; true} as & Pure) |>
        IteratorOld.filter(_x -> {l := "b" :: deref l; true} as & Pure) |>
        IteratorOld.toList;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // map filter fusion                                                       //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testMapFilterFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |>
        IteratorOld.map(    x -> {l := "a" :: deref l; x}) |>
        IteratorOld.filter(_x -> {l := "b" :: deref l; true}) |>
        IteratorOld.toList;
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def testMapFilterFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |>
        IteratorOld.map(    x -> {l := "a" :: deref l; x}    as & Pure) |>
        IteratorOld.filter(_x -> {l := "b" :: deref l; true} as & Pure) |>
        IteratorOld.toList;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // filter map fusion                                                       //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testFilterMapFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |>
        IteratorOld.filter(_x -> {l := "a" :: deref l; true}) |>
        IteratorOld.map(    x -> {l := "b" :: deref l; x}) |>
        IteratorOld.toList;
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def testFilterMapFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |>
        IteratorOld.filter(_x -> {l := "a" :: deref l; true} as & Pure) |>
        IteratorOld.map(    x -> {l := "b" :: deref l; x}    as & Pure) |>
        IteratorOld.toList;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testFindLeft01(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |> IteratorOld.find(x -> x > 0) == Some(1)

    @test
    def testFindLeft02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |> IteratorOld.find(x -> x > 1) == Some(2)

    @test
    def testFindLeft03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |> IteratorOld.find(x -> x > 2) == Some(3)

    @test
    def testFindLeft04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |> IteratorOld.find(x -> x > 3) == None

    /////////////////////////////////////////////////////////////////////////////
    // findLeft fusion                                                         //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testFindLeftFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |>
        IteratorOld.map( x -> {l := "a" :: deref l; x}) |>
        IteratorOld.find(x -> {l := "b" :: deref l; x > 1});
        List.reverse(deref l) == "a" :: "a" :: "a" :: "b" :: "b" :: Nil

    @test
    def testFindLeftFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> IteratorOld.toIter |>
        IteratorOld.map( x -> {l := "a" :: deref l; x} as & Pure) |>
        IteratorOld.find(x -> {l := "b" :: deref l; x > 1});
        List.reverse(deref l) == "a" :: "b" :: "a" :: "b" :: Nil

}
*/