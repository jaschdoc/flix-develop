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

namespace TestMMutDeque {

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def size01(): Bool = region r {
        MMutDeque.size(new MMutDeque(r)) == 0
    }

    @test
    def size02(): Bool = region r {
        let d = new MMutDeque(r);

        MMutDeque.pushBack(1, d);
        MMutDeque.pushBack(1, d);
        MMutDeque.pushBack(1, d);
        MMutDeque.pushBack(1, d);

        MMutDeque.size(d) == 4
    }

    @test
    def size03(): Bool = region r {
        let d = new MMutDeque(r);

        MMutDeque.pushBack(1, d);
        MMutDeque.pushBack(1, d);
        MMutDeque.pushBack(1, d);
        MMutDeque.pushBack(1, d);

        MMutDeque.pushBack(1, d);
        MMutDeque.pushBack(1, d);
        MMutDeque.pushBack(1, d);
        MMutDeque.pushBack(1, d);

        MMutDeque.size(d) == 8
    }

    @test
    def size04(): Bool = region r {
        let d = new MMutDeque(r);

        MMutDeque.pushFront(1, d);
        MMutDeque.pushFront(1, d);
        MMutDeque.pushFront(1, d);
        MMutDeque.pushFront(1, d);

        MMutDeque.pushFront(1, d);
        MMutDeque.pushFront(1, d);
        MMutDeque.pushFront(1, d);
        MMutDeque.pushFront(1, d);

        MMutDeque.pushFront(1, d);
        MMutDeque.pushFront(1, d);
        MMutDeque.pushFront(1, d);

        MMutDeque.size(d) == 11
    }

    @test
    def size05(): Bool = region r {
        let d = new MMutDeque(r);

        MMutDeque.pushFront(1, d);
        MMutDeque.pushFront(1, d);
        MMutDeque.pushFront(1, d);
        MMutDeque.pushFront(1, d);

        MMutDeque.popFront(d);
        MMutDeque.popFront(d);

        MMutDeque.size(d) == 2

    }

    @test
    def size06(): Bool = region r {
        let d = new MMutDeque(r);

        MMutDeque.pushFront(1, d);
        MMutDeque.pushFront(1, d);
        MMutDeque.pushFront(1, d);
        MMutDeque.pushFront(1, d);

        MMutDeque.popFront(d);
        MMutDeque.popFront(d);
        MMutDeque.popFront(d);
        MMutDeque.popFront(d);

        MMutDeque.size(d) == 0
    }

    @test
    def size07(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushFront(1, d);
        MMutDeque.popBack(d);
        MMutDeque.size(d) == 0
    }

    @test
    def size08(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);
        MMutDeque.popFront(d);
        MMutDeque.size(d) == 0
    }


    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isEmpty01(): Bool = region r {
        MMutDeque.isEmpty(new MMutDeque(r))
    }

    @test
    def isEmpty02(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);
        not (MMutDeque.isEmpty(d))
    }

    @test
    def isEmpty03(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);
        MMutDeque.popBack(d);
        MMutDeque.isEmpty(d)
    }

    @test
    def isEmpty04(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushFront(1, d);
        not (MMutDeque.isEmpty(d))
    }

    @test
    def isEmpty05(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushFront(1, d);
        MMutDeque.popFront(d);
        MMutDeque.isEmpty(d)
    }


    /////////////////////////////////////////////////////////////////////////////
    // popFront                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def popFront01(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.popFront(d) == None: Option[Int32]
    }

    @test
    def popFront02(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushFront(1, d);
        MMutDeque.popFront(d) == Some(1)
    }

    @test
    def popFront03(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushFront(1, d);
        MMutDeque.pushFront(2, d);
        MMutDeque.pushFront(3, d);
        MMutDeque.popFront(d) == Some(3)     and
            MMutDeque.popFront(d) == Some(2) and
            MMutDeque.popFront(d) == Some(1) and
            MMutDeque.popFront(d) == None
    }

    @test
    def popFront04(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);
        MMutDeque.pushBack(2, d);
        MMutDeque.pushBack(3, d);
        MMutDeque.popFront(d) == Some(1)     and
            MMutDeque.popFront(d) == Some(2) and
            MMutDeque.popFront(d) == Some(3) and
            MMutDeque.popFront(d) == None
    }

    @test
    def popFront05(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushFront(1, d);
        MMutDeque.pushBack(2, d);
        MMutDeque.popFront(d) == Some(1)     and
            MMutDeque.popFront(d) == Some(2) and
            MMutDeque.popFront(d) == None
    }

    @test
    def popFront06(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);
        MMutDeque.pushFront(2, d);
        MMutDeque.popFront(d) == Some(2)     and
            MMutDeque.popFront(d) == Some(1) and
            MMutDeque.popFront(d) == None
    }


    /////////////////////////////////////////////////////////////////////////////
    // popBack                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def popBack01(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.popBack(d) == None: Option[Int32]
    }

    @test
    def popBack02(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);
        MMutDeque.popBack(d) == Some(1)
    }

    @test
    def popBack03(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);
        MMutDeque.pushBack(2, d);
        MMutDeque.pushBack(3, d);
        MMutDeque.popBack(d) == Some(3)     and
            MMutDeque.popBack(d) == Some(2) and
            MMutDeque.popBack(d) == Some(1) and
            MMutDeque.popBack(d) == None
    }

    @test
    def popBack04(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushFront(1, d);
        MMutDeque.pushFront(2, d);
        MMutDeque.pushFront(3, d);
        MMutDeque.popBack(d) == Some(1)     and
            MMutDeque.popBack(d) == Some(2) and
            MMutDeque.popBack(d) == Some(3) and
            MMutDeque.popBack(d) == None
    }

    @test
    def popBack05(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);
        MMutDeque.pushFront(2, d);
        MMutDeque.popBack(d) == Some(1)     and
            MMutDeque.popBack(d) == Some(2) and
            MMutDeque.popBack(d) == None
    }

    @test
    def popBack06(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushFront(1, d);
        MMutDeque.pushBack(2, d);
        MMutDeque.popBack(d) == Some(2)     and
            MMutDeque.popBack(d) == Some(1) and
            MMutDeque.popBack(d) == None
    }


    /////////////////////////////////////////////////////////////////////////////
    // pushFrontPushBack (expansion)                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def pushFrontPushBack01(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushFront(1, d);
        MMutDeque.pushFront(1, d);
        MMutDeque.pushFront(1, d);
        MMutDeque.pushBack(2, d);
        MMutDeque.pushBack(2, d);
        MMutDeque.pushBack(2, d);
        MMutDeque.pushBack(2, d);
        MMutDeque.pushFront(1, d);
        MMutDeque.pushFront(1, d);
        MMutDeque.pushBack(2, d);
        MMutDeque.pushFront(1, d);
        MMutDeque.pushFront(1, d);
        MMutDeque.pushBack(2, d);
        MMutDeque.pushFront(1, d);
        MMutDeque.pushBack(2, d);
        MMutDeque.pushFront(1, d);
        MMutDeque.pushFront(1, d);
        MMutDeque.pushBack(2, d);
        MMutDeque.pushBack(2, d);
        MMutDeque.pushBack(2, d);
        // [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2]

        MMutDeque.popBack(d) == Some(2) and
        MMutDeque.popFront(d) == Some(1) and
        MMutDeque.popBack(d) == Some(2) and
        MMutDeque.popBack(d) == Some(2) and
        MMutDeque.popBack(d) == Some(2) and
        MMutDeque.popBack(d) == Some(2) and
        MMutDeque.popBack(d) == Some(2) and
        MMutDeque.popBack(d) == Some(2) and
        MMutDeque.popBack(d) == Some(2) and
        MMutDeque.popBack(d) == Some(2) and
        MMutDeque.popBack(d) == Some(2) and
        MMutDeque.popBack(d) == Some(1) and
        MMutDeque.popBack(d) == Some(1) and
        MMutDeque.popBack(d) == Some(1) and
        MMutDeque.popBack(d) == Some(1) and
        MMutDeque.popBack(d) == Some(1) and
        MMutDeque.popBack(d) == Some(1) and
        MMutDeque.popBack(d) == Some(1) and
        MMutDeque.popBack(d) == Some(1) and
        MMutDeque.popBack(d) == Some(1) and
        MMutDeque.popBack(d) == None and
        MMutDeque.popFront(d) == None
    }


    /////////////////////////////////////////////////////////////////////////////
    // sameElements                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sameElements01(): Bool = region r {
        let d = new MMutDeque(r): MMutDeque[Int32, _];
        let d2 = new MMutDeque(r): MMutDeque[Int32, _];
        MMutDeque.sameElements(d, d2)
    }

    @test
    def sameElements02(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);

        let d2 = new MMutDeque(r);
        MMutDeque.pushFront(1, d2);

        MMutDeque.sameElements(d, d2)
    }

    @test
    def sameElements03(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);
        MMutDeque.pushBack(2, d);

        let d2 = new MMutDeque(r);
        MMutDeque.pushFront(1, d2);
        MMutDeque.pushFront(2, d2);

        not MMutDeque.sameElements(d, d2)
    }

    @test
    def sameElements04(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack("a", d);
        MMutDeque.pushBack("b", d);
        MMutDeque.pushBack("c", d);

        let d2 = new MMutDeque(r);
        MMutDeque.pushFront("b", d2);
        MMutDeque.pushBack("c", d2);
        MMutDeque.pushFront("a", d2);

        MMutDeque.sameElements(d, d2)
    }

    @test
    def sameElements05(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack("a", d);
        MMutDeque.pushBack("b", d);
        MMutDeque.pushBack("c", d);

        let d2 = new MMutDeque(r);
        MMutDeque.pushFront("a", d2);
        MMutDeque.pushFront("b", d2);
        MMutDeque.pushBack("c", d2);

        not MMutDeque.sameElements(d, d2)
    }


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sum01(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.sum(d) == 0
    }

    @test
    def sum02(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);
        MMutDeque.sum(d) == 1
    }

    @test
    def sum03(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);
        MMutDeque.pushBack(2, d);
        MMutDeque.pushBack(3, d);
        MMutDeque.sum(d) == 6
    }

    @test
    def sum04(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);
        MMutDeque.pushBack(2, d);
        MMutDeque.pushBack(3, d);
        MMutDeque.pushBack(-3, d);
        MMutDeque.sum(d) == 3
    }

    @test
    def sum05(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);
        MMutDeque.pushBack(2, d);
        MMutDeque.pushBack(3, d);
        MMutDeque.pushFront(-3, d);
        MMutDeque.sum(d) == 3
    }

    @test
    def sum06(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(-1, d);
        MMutDeque.pushBack(-2, d);
        MMutDeque.pushFront(-3, d);
        MMutDeque.pushFront(-4, d);
        MMutDeque.sum(d) == -10
    }

    @test
    def sum07(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(10, d);
        MMutDeque.pushBack(-10, d);
        MMutDeque.sum(d) == 0
    }


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumWith01(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.sumWith(x -> x + 1, d) == 0
    }

    @test
    def sumWith02(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);
        MMutDeque.sumWith(x -> x + 1, d) == 2
    }

    @test
    def sumWith03(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);
        MMutDeque.pushBack(2, d);
        MMutDeque.pushBack(3, d);
        MMutDeque.sumWith(x -> x + 1, d) == 9
    }

    @test
    def sumWith04(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);
        MMutDeque.pushBack(2, d);
        MMutDeque.pushBack(3, d);
        MMutDeque.pushBack(-3, d);
        MMutDeque.sumWith(x -> x + 1, d) == 7
    }

    @test
    def sumWith05(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);
        MMutDeque.pushBack(2, d);
        MMutDeque.pushBack(3, d);
        MMutDeque.pushFront(-3, d);
        MMutDeque.sumWith(x -> x + 1, d) == 7
    }

    @test
    def sumWith06(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(-1, d);
        MMutDeque.pushBack(-2, d);
        MMutDeque.pushFront(-3, d);
        MMutDeque.pushFront(-4, d);
        MMutDeque.sumWith(x -> x + 1, d) == -6
    }

    @test
    def sumWith07(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(10, d);
        MMutDeque.pushBack(-10, d);
        MMutDeque.sumWith(x -> x + 1, d) == 2
    }


    /////////////////////////////////////////////////////////////////////////////
    // product                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def product01(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.product(d) == 0
    }

    @test
    def product02(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);
        MMutDeque.product(d) == 1
    }

    @test
    def product03(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);
        MMutDeque.pushBack(2, d);
        MMutDeque.pushBack(3, d);
        MMutDeque.product(d) == 6
    }

    @test
    def product04(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);
        MMutDeque.pushBack(2, d);
        MMutDeque.pushBack(3, d);
        MMutDeque.pushBack(-3, d);
        MMutDeque.product(d) == -18
    }

    @test
    def product05(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);
        MMutDeque.pushBack(2, d);
        MMutDeque.pushBack(3, d);
        MMutDeque.pushFront(-3, d);
        MMutDeque.product(d) == -18
    }

    @test
    def product06(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(-1, d);
        MMutDeque.pushBack(-2, d);
        MMutDeque.pushFront(-3, d);
        MMutDeque.pushFront(-4, d);
        MMutDeque.product(d) == 24
    }

    @test
    def product07(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(10, d);
        MMutDeque.pushBack(-10, d);
        MMutDeque.product(d) == -100
    }


    /////////////////////////////////////////////////////////////////////////////
    // productWith                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productWith01(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.productWith(x -> x + 1, d) == 0
    }

    @test
    def productWith02(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);
        MMutDeque.productWith(x -> x + 1, d) == 2
    }

    @test
    def productWith03(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);
        MMutDeque.pushBack(2, d);
        MMutDeque.pushBack(3, d);
        MMutDeque.productWith(x -> x + 1, d) == 24
    }

    @test
    def productWith04(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);
        MMutDeque.pushBack(2, d);
        MMutDeque.pushBack(3, d);
        MMutDeque.pushBack(-3, d);
        MMutDeque.productWith(x -> x + 1, d) == -48
    }

    @test
    def productWith05(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);
        MMutDeque.pushBack(2, d);
        MMutDeque.pushBack(3, d);
        MMutDeque.pushFront(-3, d);
        MMutDeque.productWith(x -> x + 1, d) == -48
    }

    @test
    def productWith06(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(-2, d);
        MMutDeque.pushBack(-3, d);
        MMutDeque.pushFront(-4, d);
        MMutDeque.pushFront(-5, d);
        MMutDeque.productWith(x -> x + 1, d) == 24
    }

    @test
    def productWith07(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(10, d);
        MMutDeque.pushBack(-10, d);
        MMutDeque.productWith(x -> x + 1, d) == -99
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool = region r {
        MMutDeque.foldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, new MMutDeque(r)) == 100
    }

    @test
    def foldLeft02(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);
        MMutDeque.foldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, d) == 198
    }

    @test
    def foldLeft03(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushFront(1, d);
        MMutDeque.foldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, d) == 198
    }

    @test
    def foldLeft04(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushFront(1, d);
        MMutDeque.pushBack(2, d);
        MMutDeque.foldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, d) == 196
    }

    @test
    def foldLeft05(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(2, d);
        MMutDeque.pushBack(3, d);
        MMutDeque.pushFront(1, d);
        MMutDeque.foldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, d) == 386
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool = region r {
        MMutDeque.foldRight((e, i) -> (i - e) * (e rem 2 + 1), 100, new MMutDeque(r)) == 100
    }

    @test
    def foldRight02(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);
        MMutDeque.foldRight((e, i) -> (i - e) * (e rem 2 + 1), 100, d) == 198
    }

    @test
    def foldRight03(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushFront(1, d);
        MMutDeque.foldRight((e, i) -> (i - e) * (e rem 2 + 1), 100, d) == 198
    }

    @test
    def foldRight04(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushFront(1, d);
        MMutDeque.pushBack(2, d);
        MMutDeque.foldRight((e, i) -> (i - e) * (e rem 2 + 1), 100, d) == 194
    }

    @test
    def foldRight05(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(2, d);
        MMutDeque.pushBack(3, d);
        MMutDeque.pushFront(1, d);
        MMutDeque.foldRight((e, i) -> (i - e) * (e rem 2 + 1), 100, d) == 382
    }


    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toList01(): Bool = region r {
        MMutDeque.toList(new MMutDeque(r): MMutDeque[Int32, _]) == Nil: List[Int32]
    }

    @test
    def toList02(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);
        MMutDeque.toList(d) == 1 :: Nil
    }

    @test
    def toList03(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);
        MMutDeque.pushBack(2, d);
        MMutDeque.toList(d) == 1 :: 2 :: Nil
    }

    @test
    def toList04(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);
        MMutDeque.pushBack(2, d);
        MMutDeque.pushBack(3, d);
        MMutDeque.toList(d) == 1 :: 2 :: 3 :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def join01(): Bool = region r {
        new MMutDeque(r) |> MMutDeque.join(",") == ""
    }

    @test
    def join02(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);
        MMutDeque.join(",", d) == "1"
    }

    @test
    def join03(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);
        MMutDeque.pushBack(2, d);
        MMutDeque.pushBack(3, d);
        MMutDeque.join(",", d) == "1,2,3"
    }

    @test
    def join04(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack("1", d);
        MMutDeque.pushBack("2", d);
        MMutDeque.pushBack("3", d);
        MMutDeque.join(",", d) == "1,2,3"
    }


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinWith01(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.joinWith(x -> "${x + 1}", ",", d) == ""
    }

    @test
    def joinWith02(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);
        MMutDeque.joinWith(x -> "${x + 1}", ",", d) == "2"
    }

    @test
    def joinWith03(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);
        MMutDeque.pushBack(2, d);
        MMutDeque.pushBack(3, d);
        MMutDeque.joinWith(x -> "${x + 1}", ",", d) == "2,3,4"
    }

    @test
    def joinWith04(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack("1", d);
        MMutDeque.pushBack("2", d);
        MMutDeque.pushBack("3", d);
        MMutDeque.joinWith(x -> x + x, ",", d) == "11,22,33"
    }


    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def iterator01(): Bool = region r {
        match new MMutDeque(r) |> Iterable.iterator {
            case Iterator(done, _) => done() == true
        }
    }

    @test
    def iterator02(): Bool = region r {
        let m = new MMutDeque(r);
        MMutDeque.pushBack(1, m);
        MMutDeque.pushBack(2, m);

        match m |> Iterable.iterator {
            case Iterator(done, _) => done() == false
        }
    }

    @test
    def iterator03(): Bool = region r {
        let m = new MMutDeque(r);
        MMutDeque.pushBack(1, m);

        match m |> Iterable.iterator {
            case Iterator(_, next) => next() == 1
        }
    }

    @test
    def iterator04(): Bool = region r {
        let m = new MMutDeque(r);
        MMutDeque.pushBack(1, m);

        match m |> Iterable.iterator {
            case Iterator(done, next) =>
                next();
                done() == true
        }
    }

    @test
    def iterator05(): Bool = region r {
        let m = new MMutDeque(r);
        MMutDeque.pushFront(2, m);
        MMutDeque.pushFront(1, m);

        match m |> Iterable.iterator {
            case Iterator(_, next) =>
                next();
                next() == 2
        }
    }

    /////////////////////////////////////////////////////////////////////////////
    // foreach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foreach01(): Bool = region r {
        let d = new MMutDeque(r);
        let sb = StringBuilder.new();
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MMutDeque.foreach(fn, d);
        StringBuilder.toString(sb) == ""
    }

    @test
    def foreach02(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(0, d);

        let sb = StringBuilder.new();
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MMutDeque.foreach(fn, d);
        StringBuilder.toString(sb) == "F"
    }

    @test
    def foreach03(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);
        let sb = StringBuilder.new();
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MMutDeque.foreach(fn, d);
        StringBuilder.toString(sb) == "T"
    }

    @test
    def foreach04(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(0, d);
        MMutDeque.pushBack(1, d);
        let sb = StringBuilder.new();
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MMutDeque.foreach(fn, d);
        StringBuilder.toString(sb) == "FT"
    }

    @test
    def foreach05(): Bool = region r {
        let d = new MMutDeque(r);
        MMutDeque.pushBack(1, d);
        MMutDeque.pushFront(0, d);
        let sb = StringBuilder.new();
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MMutDeque.foreach(fn, d);
        StringBuilder.toString(sb) == "FT"
    }

}
