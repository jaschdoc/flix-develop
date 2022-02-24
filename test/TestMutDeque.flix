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
namespace TestMutDeque {

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def size01(): Bool & Impure =
        MutDeque1.size(MutDeque1.new()) == 0

    @test
    def size02(): Bool & Impure =
        let d = MutDeque1.new();

        MutDeque1.pushBack(1, d);
        MutDeque1.pushBack(1, d);
        MutDeque1.pushBack(1, d);
        MutDeque1.pushBack(1, d);

        MutDeque1.size(d) == 4

    @test
    def size03(): Bool & Impure =
        let d = MutDeque1.new();

        MutDeque1.pushBack(1, d);
        MutDeque1.pushBack(1, d);
        MutDeque1.pushBack(1, d);
        MutDeque1.pushBack(1, d);

        MutDeque1.pushBack(1, d);
        MutDeque1.pushBack(1, d);
        MutDeque1.pushBack(1, d);
        MutDeque1.pushBack(1, d);

        MutDeque1.size(d) == 8

    @test
    def size04(): Bool & Impure =
        let d = MutDeque1.new();

        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);

        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);

        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);

        MutDeque1.size(d) == 11

    @test
    def size05(): Bool & Impure =
        let d = MutDeque1.new();

        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);

        MutDeque1.popFront(d);
        MutDeque1.popFront(d);

        MutDeque1.size(d) == 2


    @test
    def size06(): Bool & Impure =
        let d = MutDeque1.new();

        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);

        MutDeque1.popFront(d);
        MutDeque1.popFront(d);
        MutDeque1.popFront(d);
        MutDeque1.popFront(d);

        MutDeque1.size(d) == 0

    @test
    def size07(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushFront(1, d);
        MutDeque1.popBack(d);
        MutDeque1.size(d) == 0

    @test
    def size08(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(1, d);
        MutDeque1.popFront(d);
        MutDeque1.size(d) == 0


    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isEmpty01(): Bool & Impure =
        MutDeque1.isEmpty(MutDeque1.new())

    @test
    def isEmpty02(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(1, d);
        not (MutDeque1.isEmpty(d))

    @test
    def isEmpty03(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(1, d);
        MutDeque1.popBack(d);
        MutDeque1.isEmpty(d)

    @test
    def isEmpty04(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushFront(1, d);
        not (MutDeque1.isEmpty(d))

    @test
    def isEmpty05(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushFront(1, d);
        MutDeque1.popFront(d);
        MutDeque1.isEmpty(d)


    /////////////////////////////////////////////////////////////////////////////
    // popFront                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def popFront01(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.popFront(d) == None: Option[Int32]

    @test
    def popFront02(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushFront(1, d);
        MutDeque1.popFront(d) == Some(1)

    @test
    def popFront03(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(2, d);
        MutDeque1.pushFront(3, d);
        MutDeque1.popFront(d) == Some(3)     and
            MutDeque1.popFront(d) == Some(2) and
            MutDeque1.popFront(d) == Some(1) and
            MutDeque1.popFront(d) == None

    @test
    def popFront04(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(1, d);
        MutDeque1.pushBack(2, d);
        MutDeque1.pushBack(3, d);
        MutDeque1.popFront(d) == Some(1)     and
            MutDeque1.popFront(d) == Some(2) and
            MutDeque1.popFront(d) == Some(3) and
            MutDeque1.popFront(d) == None

    @test
    def popFront05(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushFront(1, d);
        MutDeque1.pushBack(2, d);
        MutDeque1.popFront(d) == Some(1)     and
            MutDeque1.popFront(d) == Some(2) and
            MutDeque1.popFront(d) == None

    @test
    def popFront06(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(1, d);
        MutDeque1.pushFront(2, d);
        MutDeque1.popFront(d) == Some(2)     and
            MutDeque1.popFront(d) == Some(1) and
            MutDeque1.popFront(d) == None


    /////////////////////////////////////////////////////////////////////////////
    // popBack                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def popBack01(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.popBack(d) == None: Option[Int32]

    @test
    def popBack02(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(1, d);
        MutDeque1.popBack(d) == Some(1)

    @test
    def popBack03(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(1, d);
        MutDeque1.pushBack(2, d);
        MutDeque1.pushBack(3, d);
        MutDeque1.popBack(d) == Some(3)     and
            MutDeque1.popBack(d) == Some(2) and
            MutDeque1.popBack(d) == Some(1) and
            MutDeque1.popBack(d) == None

    @test
    def popBack04(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(2, d);
        MutDeque1.pushFront(3, d);
        MutDeque1.popBack(d) == Some(1)     and
            MutDeque1.popBack(d) == Some(2) and
            MutDeque1.popBack(d) == Some(3) and
            MutDeque1.popBack(d) == None

    @test
    def popBack05(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(1, d);
        MutDeque1.pushFront(2, d);
        MutDeque1.popBack(d) == Some(1)     and
            MutDeque1.popBack(d) == Some(2) and
            MutDeque1.popBack(d) == None

    @test
    def popBack06(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushFront(1, d);
        MutDeque1.pushBack(2, d);
        MutDeque1.popBack(d) == Some(2)     and
            MutDeque1.popBack(d) == Some(1) and
            MutDeque1.popBack(d) == None


    /////////////////////////////////////////////////////////////////////////////
    // pushFrontPushBack (expansion)                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def pushFrontPushBack01(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.pushBack(2, d);
        MutDeque1.pushBack(2, d);
        MutDeque1.pushBack(2, d);
        MutDeque1.pushBack(2, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.pushBack(2, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.pushBack(2, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.pushBack(2, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.pushBack(2, d);
        MutDeque1.pushBack(2, d);
        MutDeque1.pushBack(2, d);
        // [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2]

        MutDeque1.popBack(d) == Some(2) and
        MutDeque1.popFront(d) == Some(1) and
        MutDeque1.popBack(d) == Some(2) and
        MutDeque1.popBack(d) == Some(2) and
        MutDeque1.popBack(d) == Some(2) and
        MutDeque1.popBack(d) == Some(2) and
        MutDeque1.popBack(d) == Some(2) and
        MutDeque1.popBack(d) == Some(2) and
        MutDeque1.popBack(d) == Some(2) and
        MutDeque1.popBack(d) == Some(2) and
        MutDeque1.popBack(d) == Some(2) and
        MutDeque1.popBack(d) == Some(1) and
        MutDeque1.popBack(d) == Some(1) and
        MutDeque1.popBack(d) == Some(1) and
        MutDeque1.popBack(d) == Some(1) and
        MutDeque1.popBack(d) == Some(1) and
        MutDeque1.popBack(d) == Some(1) and
        MutDeque1.popBack(d) == Some(1) and
        MutDeque1.popBack(d) == Some(1) and
        MutDeque1.popBack(d) == Some(1) and
        MutDeque1.popBack(d) == None and
        MutDeque1.popFront(d) == None

    /////////////////////////////////////////////////////////////////////////////
    // sameElements                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sameElements01(): Bool & Impure =
        let d = MutDeque1.new(): MutDeque[Int32];
        let d2 = MutDeque1.new(): MutDeque[Int32];
        MutDeque1.sameElements(d, d2)

    @test
    def sameElements02(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(1, d);

        let d2 = MutDeque1.new();
        MutDeque1.pushFront(1, d2);

        MutDeque1.sameElements(d, d2)

    @test
    def sameElements03(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(1, d);
        MutDeque1.pushBack(2, d);

        let d2 = MutDeque1.new();
        MutDeque1.pushFront(1, d2);
        MutDeque1.pushFront(2, d2);

        not MutDeque1.sameElements(d, d2)

    @test
    def sameElements04(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack("a", d);
        MutDeque1.pushBack("b", d);
        MutDeque1.pushBack("c", d);

        let d2 = MutDeque1.new();
        MutDeque1.pushFront("b", d2);
        MutDeque1.pushBack("c", d2);
        MutDeque1.pushFront("a", d2);

        MutDeque1.sameElements(d, d2)

    @test
    def sameElements05(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack("a", d);
        MutDeque1.pushBack("b", d);
        MutDeque1.pushBack("c", d);

        let d2 = MutDeque1.new();
        MutDeque1.pushFront("a", d2);
        MutDeque1.pushFront("b", d2);
        MutDeque1.pushBack("c", d2);

        not MutDeque1.sameElements(d, d2)


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sum01(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.sum(d) == 0

    @test
    def sum02(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(1, d);
        MutDeque1.sum(d) == 1

    @test
    def sum03(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(1, d);
        MutDeque1.pushBack(2, d);
        MutDeque1.pushBack(3, d);
        MutDeque1.sum(d) == 6

    @test
    def sum04(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(1, d);
        MutDeque1.pushBack(2, d);
        MutDeque1.pushBack(3, d);
        MutDeque1.pushBack(-3, d);
        MutDeque1.sum(d) == 3

    @test
    def sum05(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(1, d);
        MutDeque1.pushBack(2, d);
        MutDeque1.pushBack(3, d);
        MutDeque1.pushFront(-3, d);
        MutDeque1.sum(d) == 3

    @test
    def sum06(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(-1, d);
        MutDeque1.pushBack(-2, d);
        MutDeque1.pushFront(-3, d);
        MutDeque1.pushFront(-4, d);
        MutDeque1.sum(d) == -10

    @test
    def sum07(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(10, d);
        MutDeque1.pushBack(-10, d);
        MutDeque1.sum(d) == 0


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumWith01(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.sumWith(x -> x + 1, d) == 0

    @test
    def sumWith02(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(1, d);
        MutDeque1.sumWith(x -> x + 1, d) == 2

    @test
    def sumWith03(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(1, d);
        MutDeque1.pushBack(2, d);
        MutDeque1.pushBack(3, d);
        MutDeque1.sumWith(x -> x + 1, d) == 9

    @test
    def sumWith04(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(1, d);
        MutDeque1.pushBack(2, d);
        MutDeque1.pushBack(3, d);
        MutDeque1.pushBack(-3, d);
        MutDeque1.sumWith(x -> x + 1, d) == 7

    @test
    def sumWith05(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(1, d);
        MutDeque1.pushBack(2, d);
        MutDeque1.pushBack(3, d);
        MutDeque1.pushFront(-3, d);
        MutDeque1.sumWith(x -> x + 1, d) == 7

    @test
    def sumWith06(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(-1, d);
        MutDeque1.pushBack(-2, d);
        MutDeque1.pushFront(-3, d);
        MutDeque1.pushFront(-4, d);
        MutDeque1.sumWith(x -> x + 1, d) == -6

    @test
    def sumWith07(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(10, d);
        MutDeque1.pushBack(-10, d);
        MutDeque1.sumWith(x -> x + 1, d) == 2


    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool & Impure =
        MutDeque1.foldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, MutDeque1.new()) == 100

    @test
    def foldLeft02(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(1, d);
        MutDeque1.foldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, d) == 198

    @test
    def foldLeft03(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushFront(1, d);
        MutDeque1.foldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, d) == 198

    @test
    def foldLeft04(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushFront(1, d);
        MutDeque1.pushBack(2, d);
        MutDeque1.foldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, d) == 196

    @test
    def foldLeft05(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(2, d);
        MutDeque1.pushBack(3, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.foldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, d) == 386


    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool & Impure =
        MutDeque1.foldRight((e, i) -> { (i - e) * (e rem 2 + 1) }, 100, MutDeque1.new()) == 100

    @test
    def foldRight02(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(1, d);
        let r = MutDeque1.foldRight((e, i) -> { (i - e) * (e rem 2 + 1) }, 100, d);
        r == 198

    @test
    def foldRight03(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushFront(1, d);
        let r = MutDeque1.foldRight((e, i) -> { (i - e) * (e rem 2 + 1) }, 100, d);
        r == 198

    @test
    def foldRight04(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushFront(1, d);
        MutDeque1.pushBack(2, d);
        let r = MutDeque1.foldRight((e, i) -> { (i - e) * (e rem 2 + 1) }, 100, d);
        r == 194

    @test
    def foldRight05(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(2, d);
        MutDeque1.pushBack(3, d);
        MutDeque1.pushFront(1, d);
        let r = MutDeque1.foldRight((e, i) -> { (i - e) * (e rem 2 + 1) }, 100, d);
        r == 382


    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toList01(): Bool & Impure =
        MutDeque1.toList(MutDeque1.new(): MutDeque[Int32]) == Nil: List[Int32]

    @test
    def toList02(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(1, d);
        MutDeque1.toList(d) == 1 :: Nil

    @test
    def toList03(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(1, d);
        MutDeque1.pushBack(2, d);
        MutDeque1.toList(d) == 1 :: 2 :: Nil

    @test
    def toList04(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(1, d);
        MutDeque1.pushBack(2, d);
        MutDeque1.pushBack(3, d);
        MutDeque1.toList(d) == 1 :: 2 :: 3 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinWith01(): Bool & Impure =
        (Nil: List[Int32]) |> List.toMutDeque |>
            MutDeque.joinWith(x -> "${x + 1}", ",") == ""

    @test
    def joinWith02(): Bool & Impure =
        (1 :: Nil) |> List.toMutDeque |>
            MutDeque.joinWith(x -> "${x + 1}", ",") == "2"

    @test
    def joinWith03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toMutDeque |>
            MutDeque.joinWith(x -> "${x + 1}", ",") == "2,3,4"

    @test
    def joinWith04(): Bool & Impure =
        ("1" :: "2" :: "3" :: Nil) |> List.toMutDeque |>
            MutDeque.joinWith(x -> x + x, ",") == "11,22,33"

}
*/
