/*
 * Copyright 2021 Stephen Tetley
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
namespace TestChain {


    /////////////////////////////////////////////////////////////////////////////
    // empty                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def empty01(): Bool =
        let c: Chain[Int32] = Chain1.empty();
        Chain1.toList(c) == Nil

    /////////////////////////////////////////////////////////////////////////////
    // singleton                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def singleton01(): Bool =
        let c = Chain1.singleton(1);
        Chain1.toList(c) == 1 :: Nil

    @test
    def singleton02(): Bool =
        let c: Chain[Option[Int32]] = Chain1.singleton(None);
        Chain1.toList(c) == None :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // ap                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def ap01(): Bool = Chain1.ap(Chain1.empty(), Chain1.empty()) == Chain1.empty() : Chain[Int32]

    @test
    def ap02(): Bool = Chain1.ap(Chain1.singleton(x -> x+1), Chain1.empty()) == Chain1.empty()

    @test
    def ap03(): Bool = Chain1.ap(Chain1.empty(), Chain1.singleton(5)) == Chain1.empty() : Chain[Int32]

    @test
    def ap04(): Bool = Chain1.ap(Chain1.singleton(x -> x+1), Chain1.singleton(5)) == Chain1.singleton(6)

    @test
    def ap05(): Bool = Chain1.ap(Chain1.singleton(x -> x+1), List.toChain(0 :: 5 :: Nil)) == List.toChain(1 :: 6 :: Nil)

    @test
    def ap06(): Bool = Chain1.ap(List.toChain((x -> x+1) :: (x -> x*2) :: Nil), List.toChain(0 :: 4 :: Nil)) == List.toChain(1 :: 5 :: 0 :: 8 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isEmpty01(): Bool = Chain1.isEmpty(Chain1.empty()) == true

    @test
    def isEmpty02(): Bool = Chain1.isEmpty(Chain1.singleton(1)) == false

    @test
    def isEmpty03(): Bool = Chain1.isEmpty(List.toChain(1 :: 2 :: Nil)) == false

    /////////////////////////////////////////////////////////////////////////////
    // cons                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def cons01(): Bool = Chain1.cons(1, Chain1.empty()) == Chain1.singleton(1)

    @test
    def cons02(): Bool = Chain1.cons(1, Chain1.singleton(2)) == List.toChain(1 :: 2 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // snoc                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def snoc01(): Bool = Chain1.snoc(Chain1.empty(), 1) == Chain1.singleton(1)

    @test
    def snoc02(): Bool = Chain1.snoc(Chain1.singleton(1), 2) == List.toChain(1 :: 2 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def head01(): Bool = Chain1.head(Chain1.empty(): Chain[Unit]) == None

    @test
    def head02(): Bool = Chain1.head(Chain1.singleton(1)) == Some(1)

    @test
    def head03(): Bool = Chain1.head(List.toChain(2 :: 1 :: Nil)) == Some(2)

    @test
    def head04(): Bool = Chain1.head(List.toChain(3 :: 2 :: 1 :: Nil)) == Some(3)

    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def last01(): Bool = Chain1.last(Chain1.empty(): Chain[Unit]) == None

    @test
    def last02(): Bool = Chain1.last(Chain1.singleton(1)) == Some(1)

    @test
    def last03(): Bool = Chain1.last(List.toChain(1 :: 2 :: Nil)) == Some(2)

    @test
    def last04(): Bool = Chain1.last(List.toChain(1 :: 2 :: 3 :: Nil)) == Some(3)

    /////////////////////////////////////////////////////////////////////////////
    // init                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def init01(): Bool = Chain1.init(Chain1.empty(): Chain[Unit]) == None

    @test
    def init02(): Bool = Chain1.init(Chain1.singleton(0)) == Some(Chain1.empty())

    @test
    def init03(): Bool = Chain1.init(List.toChain(0 :: 1 :: Nil)) == Some(Chain1.singleton(0))

    @test
    def init04(): Bool = Chain1.init(List.toChain(2 :: 1 :: 0 :: Nil)) == Some(List.toChain(2 :: 1 :: Nil))

    @test
    def init05(): Bool = Chain1.init(List.toChain('a' :: 'b' :: 'c' :: 'd' :: Nil)) == Some(List.toChain('a' :: 'b' :: 'c' :: Nil))

    @test
    def init06(): Bool = Chain1.init(List.toChain("test1" :: "test2" :: "test3" :: Nil)) == Some(List.toChain("test1" :: "test2" :: Nil))

    @test
    def init07(): Bool = Chain1.init(List.toChain(2 :: 1 :: 0 :: -1 :: Nil)) == Some(List.toChain(2 :: 1 :: 0 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def length01(): Bool = Chain1.length(Chain1.empty()) == 0

    @test
    def length02(): Bool = Chain1.length(Chain1.singleton(1)) == 1

    @test
    def length03(): Bool = Chain1.length(List.toChain(1 :: 2 :: Nil)) == 2

    @test
    def length04(): Bool = Chain1.length(List.toChain(1 :: 2 :: 3 :: Nil)) == 3

    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def append01(): Bool = Chain1.append(Chain1.empty(): Chain[Unit], Chain1.empty(): Chain[Unit]) == Chain1.empty()

    @test
    def append02(): Bool = Chain1.append(Chain1.empty(), Chain1.singleton(1)) == Chain1.singleton(1)

    @test
    def append03(): Bool = Chain1.append(Chain1.empty(), List.toChain(1 :: 2 :: Nil)) == List.toChain(1 :: 2 :: Nil)

    @test
    def append04(): Bool = Chain1.append(Chain1.singleton(1), Chain1.empty()) == Chain1.singleton(1)

    @test
    def append05(): Bool = Chain1.append(List.toChain(1 :: 2 :: Nil), Chain1.empty()) == List.toChain(1 :: 2 :: Nil)

    @test
    def append06(): Bool = Chain1.append(Chain1.singleton(1), Chain1.singleton(2)) == List.toChain(1 :: 2 :: Nil)

    @test
    def append07(): Bool = Chain1.append(List.toChain(1 :: 2 :: Nil), Chain1.singleton(3)) == List.toChain(1 :: 2 :: 3 :: Nil)

    @test
    def append08(): Bool = Chain1.append(Chain1.singleton(1), List.toChain(2 :: 3 :: Nil)) == List.toChain(1 :: 2 :: 3 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // viewLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def viewLeft01(): Bool = Chain1.viewLeft(Chain1.empty(): Chain[Int32]) == Chain/ViewLeft.NoneLeft

    @test
    def viewLeft02(): Bool = Chain1.viewLeft(Chain1.singleton(1)) == Chain/ViewLeft.SomeLeft(1, Chain1.empty())

    @test
    def viewLeft03(): Bool = Chain1.viewLeft(List.toChain(1 :: 2 :: Nil)) == Chain/ViewLeft.SomeLeft(1, Chain1.singleton(2))

    @test
    def viewLeft04(): Bool = Chain1.viewLeft(List.toChain(1 :: 2 :: 3 :: Nil)) == Chain/ViewLeft.SomeLeft(1, List.toChain(2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // viewRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def viewRight01(): Bool = Chain1.viewRight(Chain1.empty(): Chain[Int32]) == Chain/ViewRight.NoneRight

    @test
    def viewRight02(): Bool = Chain1.viewRight(Chain1.singleton(1)) == Chain/ViewRight.SomeRight(Chain1.empty(), 1)

    @test
    def viewRight03(): Bool = Chain1.viewRight(List.toChain(1 :: 2 :: Nil)) == Chain/ViewRight.SomeRight(Chain1.singleton(1), 2)

    @test
    def viewRight04(): Bool = Chain1.viewRight(List.toChain(1 :: 2 :: 3 :: Nil)) == Chain/ViewRight.SomeRight(List.toChain(1 :: 2 :: Nil), 3)

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def memberOf01(): Bool = Chain1.memberOf(0, Chain1.empty()) == false

    @test
    def memberOf02(): Bool = Chain1.memberOf(0, Chain1.singleton(1)) == false

    @test
    def memberOf03(): Bool = Chain1.memberOf(0, Chain1.singleton(0)) == true

    @test
    def memberOf04(): Bool = Chain1.memberOf(0, List.toChain(1 :: 2 :: Nil)) == false

    @test
    def memberOf05(): Bool = Chain1.memberOf(1, List.toChain(1 :: 2 :: Nil)) == true

    @test
    def memberOf06(): Bool = Chain1.memberOf(2, List.toChain(1 :: 2 :: Nil)) == true

    @test
    def memberOf07(): Bool = Chain1.memberOf(3, List.toChain(1 :: 2 :: Nil)) == false

    /////////////////////////////////////////////////////////////////////////////
    // indexOf                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indexOf01(): Bool = Chain1.indexOf(0, Chain1.empty()) == -1

    @test
    def indexOf02(): Bool = Chain1.indexOf(0, Chain1.singleton(1)) == -1

    @test
    def indexOf03(): Bool = Chain1.indexOf(1, Chain1.singleton(1)) == 0

    @test
    def indexOf04(): Bool = Chain1.indexOf(0, List.toChain(1 :: 2 :: Nil)) == -1

    @test
    def indexOf05(): Bool = Chain1.indexOf(1, List.toChain(1 :: 2 :: Nil)) == 0

    @test
    def indexOf06(): Bool = Chain1.indexOf(2, List.toChain(1 :: 2 :: Nil)) == 1

    /////////////////////////////////////////////////////////////////////////////
    // find                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def find01(): Bool = Chain1.find(i -> i > 2, Chain1.empty()) == None

    @test
    def find02(): Bool = Chain1.find(i -> i > 2, Chain1.singleton(1)) == None

    @test
    def find03(): Bool = Chain1.find(i -> i > 2, Chain1.singleton(3)) == Some(3)

    @test
    def find04(): Bool = Chain1.find(i -> i > 2, List.toChain(1 :: 2 :: Nil)) == None

    @test
    def find05(): Bool = Chain1.find(i -> i > 2, List.toChain(6 :: -6 :: Nil)) == Some(6)

    @test
    def find06(): Bool = Chain1.find(i -> i > 2, List.toChain(-6 :: 6 :: Nil)) == Some(6)

    @test
    def find07(): Bool = Chain1.find(i -> i > 2, List.toChain(6 :: 7 :: Nil)) == Some(6)

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findLeft01(): Bool = Chain1.findLeft(i -> i > 2, Chain1.empty()) == None

    @test
    def findLeft02(): Bool = Chain1.findLeft(i -> i > 2, Chain1.singleton(1)) == None

    @test
    def findLeft03(): Bool = Chain1.findLeft(i -> i > 2, Chain1.singleton(3)) == Some(3)

    @test
    def findLeft04(): Bool = Chain1.findLeft(i -> i > 2, List.toChain(1 :: 2 :: Nil)) == None

    @test
    def findLeft05(): Bool = Chain1.findLeft(i -> i > 2, List.toChain(6 :: -6 :: Nil)) == Some(6)

    @test
    def findLeft06(): Bool = Chain1.findLeft(i -> i > 2, List.toChain(-6 :: 6 :: Nil)) == Some(6)

    @test
    def findLeft07(): Bool = Chain1.findLeft(i -> i > 2, List.toChain(6 :: 7 :: Nil)) == Some(6)

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findRight01(): Bool = Chain1.findRight(i -> i > 2, Chain1.empty()) == None

    @test
    def findRight02(): Bool = Chain1.findRight(i -> i > 2, Chain1.singleton(1)) == None

    @test
    def findRight03(): Bool = Chain1.findRight(i -> i > 2, Chain1.singleton(3)) == Some(3)

    @test
    def findRight04(): Bool = Chain1.findRight(i -> i > 2, List.toChain(1 :: 2 :: Nil)) == None

    @test
    def findRight05(): Bool = Chain1.findRight(i -> i > 2, List.toChain(6 :: -6 :: Nil)) == Some(6)

    @test
    def findRight06(): Bool = Chain1.findRight(i -> i > 2, List.toChain(-6 :: 6 :: Nil)) == Some(6)

    @test
    def findRight07(): Bool = Chain1.findRight(i -> i > 2, List.toChain(6 :: 7 :: Nil)) == Some(7)

    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def range01(): Bool = Chain1.range(1, 0) == Chain1.empty()

    @test
    def range02(): Bool = Chain1.range(1, 1) == Chain1.empty()

    @test
    def range03(): Bool = Chain1.range(1, 2) == Chain1.singleton(1)

    @test
    def range04(): Bool = Chain1.range(1, 3) == List.toChain(1 :: 2 :: Nil)

    @test
    def range05(): Bool = Chain1.range(1, 4) == List.toChain(1 :: 2 :: 3 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // repeat                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def repeat01(): Bool = Chain1.repeat(-1, 1) == Chain1.empty()

    @test
    def repeat02(): Bool = Chain1.repeat(0, 1) == Chain1.empty()

    @test
    def repeat03(): Bool = Chain1.repeat(1, 1) == Chain1.singleton(1)

    @test
    def repeat04(): Bool = Chain1.repeat(2, 1) == List.toChain(1 :: 1 :: Nil)

    @test
    def repeat05(): Bool = Chain1.repeat(3, 1) == List.toChain(1 :: 1 :: 1 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // scan                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def scan01(): Bool = Chain1.scan((i, b) -> if (b) i+1 else i+2, 1, Chain1.empty()) == Chain1.singleton(1)

    @test
    def scan02(): Bool = Chain1.scan((i, b) -> if (b) i+1 else i+2, 1, Chain1.singleton(false)) == List.toChain(1 :: 3 :: Nil)

    @test
    def scan03(): Bool = Chain1.scan((i, b) -> if (b) i+1 else i+2, 1, Chain1.singleton(true)) == List.toChain(1 :: 2 :: Nil)

    @test
    def scan04(): Bool = Chain1.scan((i, b) -> if (b) i+1 else i+2, 1, List.toChain(false :: false :: Nil)) == List.toChain(1 :: 3 :: 5 :: Nil)

    @test
    def scan05(): Bool = Chain1.scan((i, b) -> if (b) i+1 else i+2, 1, List.toChain(false :: true :: Nil)) == List.toChain(1 :: 3 :: 4 :: Nil)

    @test
    def scan06(): Bool = Chain1.scan((i, b) -> if (b) i+1 else i+2, 1, List.toChain(true :: false :: Nil)) == List.toChain(1 :: 2 :: 4 :: Nil)

    @test
    def scan07(): Bool = Chain1.scan((i, b) -> if (b) i+1 else i+2, 1, List.toChain(true :: true :: Nil)) == List.toChain(1 :: 2 :: 3 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // scanLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def scanLeft01(): Bool = Chain1.scanLeft((i, b) -> if (b) i+1 else i+2, 1, Chain1.empty()) == Chain1.singleton(1)

    @test
    def scanLeft02(): Bool = Chain1.scanLeft((i, b) -> if (b) i+1 else i+2, 1, Chain1.singleton(false)) == List.toChain(1 :: 3 :: Nil)

    @test
    def scanLeft03(): Bool = Chain1.scanLeft((i, b) -> if (b) i+1 else i+2, 1, Chain1.singleton(true)) == List.toChain(1 :: 2 :: Nil)

    @test
    def scanLeft04(): Bool = Chain1.scanLeft((i, b) -> if (b) i+1 else i+2, 1, List.toChain(false :: false :: Nil)) == List.toChain(1 :: 3 :: 5 :: Nil)

    @test
    def scanLeft05(): Bool = Chain1.scanLeft((i, b) -> if (b) i+1 else i+2, 1, List.toChain(false :: true :: Nil)) == List.toChain(1 :: 3 :: 4 :: Nil)

    @test
    def scanLeft06(): Bool = Chain1.scanLeft((i, b) -> if (b) i+1 else i+2, 1, List.toChain(true :: false :: Nil)) == List.toChain(1 :: 2 :: 4 :: Nil)

    @test
    def scanLeft07(): Bool = Chain1.scanLeft((i, b) -> if (b) i+1 else i+2, 1, List.toChain(true :: true :: Nil)) == List.toChain(1 :: 2 :: 3 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // scanRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def scanRight01(): Bool = Chain1.scanRight((b, i) -> if (b) i+1 else i+2, 1, Chain1.empty()) == Chain1.singleton(1)

    @test
    def scanRight02(): Bool = Chain1.scanRight((b, i) -> if (b) i+1 else i+2, 1, Chain1.singleton(false)) == List.toChain(3 :: 1 :: Nil)

    @test
    def scanRight03(): Bool = Chain1.scanRight((b, i) -> if (b) i+1 else i+2, 1, Chain1.singleton(true)) == List.toChain(2 :: 1 :: Nil)

    @test
    def scanRight04(): Bool = Chain1.scanRight((b, i) -> if (b) i+1 else i+2, 1, List.toChain(false :: false :: Nil)) == List.toChain(5 :: 3 :: 1 :: Nil)

    @test
    def scanRight05(): Bool = Chain1.scanRight((b, i) -> if (b) i+1 else i+2, 1, List.toChain(false :: true :: Nil)) == List.toChain(4 :: 2 :: 1 :: Nil)

    @test
    def scanRight06(): Bool = Chain1.scanRight((b, i) -> if (b) i+1 else i+2, 1, List.toChain(true :: false :: Nil)) == List.toChain(4 :: 3 :: 1 :: Nil)

    @test
    def scanRight07(): Bool = Chain1.scanRight((b, i) -> if (b) i+1 else i+2, 1, List.toChain(true :: true :: Nil)) == List.toChain(3 :: 2 :: 1 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map01(): Bool = Chain1.map(i -> i > 2, Chain1.empty()) == Chain1.empty()

    @test
    def map02(): Bool = Chain1.map(i -> i > 2, Chain1.singleton(1)) == Chain1.singleton(false)

    @test
    def map03(): Bool = Chain1.map(i -> i > 2, Chain1.singleton(3)) == Chain1.singleton(true)

    @test
    def map04(): Bool = Chain1.map(i -> i > 2, List.toChain(1 :: 2 :: Nil)) == List.toChain(false :: false :: Nil)

    @test
    def map05(): Bool = Chain1.map(i -> i > 2, List.toChain(1 :: 8 :: Nil)) == List.toChain(false :: true :: Nil)

    @test
    def map06(): Bool = Chain1.map(i -> i > 2, List.toChain(8 :: 1 :: Nil)) == List.toChain(true :: false :: Nil)

    @test
    def map07(): Bool = Chain1.map(i -> i > 2, List.toChain(7 :: 8 :: Nil)) == List.toChain(true :: true :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithIndex01(): Bool = Chain1.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, Chain1.empty()) == Chain1.empty()

    @test
    def mapWithIndex02(): Bool = Chain1.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, Chain1.singleton(1)) == Chain1.singleton(false)

    @test
    def mapWithIndex03(): Bool = Chain1.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, Chain1.singleton(3)) == Chain1.singleton(true)

    @test
    def mapWithIndex04(): Bool = Chain1.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, List.toChain(1 :: 2 :: Nil)) == List.toChain(false :: true :: Nil)

    @test
    def mapWithIndex05(): Bool = Chain1.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, List.toChain(1 :: 8 :: Nil)) == List.toChain(false :: false :: Nil)

    @test
    def mapWithIndex06(): Bool = Chain1.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, List.toChain(8 :: 1 :: Nil)) == List.toChain(true :: true :: Nil)

    @test
    def mapWithIndex07(): Bool = Chain1.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, List.toChain(7 :: 8 :: Nil)) == List.toChain(true :: false :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // flatMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatMap01(): Bool = Chain1.flatMap(i -> Chain1.repeat(i, i), Chain1.empty()) == Chain1.empty()

    @test
    def flatMap02(): Bool = Chain1.flatMap(i -> Chain1.repeat(i, i), Chain1.singleton(0)) == Chain1.empty()

    @test
    def flatMap03(): Bool = Chain1.flatMap(i -> Chain1.repeat(i, i), Chain1.singleton(1)) == Chain1.singleton(1)

    @test
    def flatMap04(): Bool = Chain1.flatMap(i -> Chain1.repeat(i, i), Chain1.singleton(2)) == List.toChain(2 :: 2 :: Nil)

    @test
    def flatMap05(): Bool = Chain1.flatMap(i -> Chain1.repeat(i, i), List.toChain(1 :: 2 :: Nil)) == List.toChain(1 :: 2 :: 2 :: Nil)

    @test
    def flatMap06(): Bool = Chain1.flatMap(i -> Chain1.repeat(i, i), List.toChain(2 :: 3 :: Nil)) == List.toChain(2 :: 2 :: 3 :: 3 :: 3 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reverse01(): Bool = Chain1.reverse(Chain1.empty(): Chain[Unit]) == Chain1.empty()

    @test
    def reverse02(): Bool = Chain1.reverse(Chain1.singleton(1)) == Chain1.singleton(1)

    @test
    def reverse03(): Bool = Chain1.reverse(List.toChain(1 :: 2 :: Nil)) == List.toChain(2 :: 1 :: Nil)

    @test
    def reverse04(): Bool = Chain1.reverse(List.toChain(1 :: 1 :: Nil)) == List.toChain(1 :: 1 :: Nil)

    @test
    def reverse05(): Bool = Chain1.reverse(List.toChain(1 :: 2 :: 3 :: Nil)) == List.toChain(3 :: 2 :: 1 :: Nil)

    @test
    def reverse06(): Bool = Chain1.reverse(List.toChain(1 :: 2 :: 3 :: 4 :: Nil)) == List.toChain(4 :: 3 :: 2 :: 1 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // intersperse                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def intersperse01(): Bool = Chain1.intersperse(11, Chain1.empty()) == Chain1.empty()

    @test
    def intersperse02(): Bool = Chain1.intersperse(11, Chain1.singleton(1)) == Chain1.singleton(1)

    @test
    def intersperse03(): Bool = Chain1.intersperse(11, List.toChain(1 :: 2 :: Nil)) == List.toChain(1 :: 11 :: 2 :: Nil)

    @test
    def intersperse04(): Bool = Chain1.intersperse(11, List.toChain(1 :: 2 :: 3 :: Nil)) == List.toChain(1 :: 11 :: 2 :: 11 :: 3 :: Nil)

    @test
    def intersperse05(): Bool = Chain1.intersperse(11, List.toChain(1 :: 2 :: 3 :: 4 :: Nil)) == List.toChain(1 :: 11 :: 2 :: 11 :: 3 :: 11 :: 4 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def count01(): Bool = Chain1.count(i -> i > 3, Chain1.empty()) == 0

    @test
    def count02(): Bool = Chain1.count(i -> i > 3, Chain1.singleton(1)) == 0

    @test
    def count03(): Bool = Chain1.count(i -> i > 3, Chain1.singleton(4)) == 1

    @test
    def count04(): Bool = Chain1.count(i -> i > 3, List.toChain(1 :: 2 :: Nil)) == 0

    @test
    def count05(): Bool = Chain1.count(i -> i > 3, List.toChain(1 :: 8 :: Nil)) == 1

    @test
    def count06(): Bool = Chain1.count(i -> i > 3, List.toChain(8 :: 1 :: Nil)) == 1

    @test
    def count07(): Bool = Chain1.count(i -> i > 3, List.toChain(6 :: 7 :: Nil)) == 2


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sum01(): Bool =
        List.toChain(Nil) |> Chain1.sum == 0

    @test
    def sum02(): Bool =
        List.toChain(1 :: Nil) |> Chain1.sum == 1

    @test
    def sum03(): Bool =
        List.toChain(1 :: 2 :: 3 :: Nil) |> Chain1.sum == 6

    @test
    def sum04(): Bool =
        List.toChain(1 :: 2 :: 3 :: -3 :: Nil) |> Chain1.sum == 3

    @test
    def sum05(): Bool =
        List.toChain(-1 :: -2 :: -3 :: -4 :: Nil) |> Chain1.sum == -10

    @test
    def sum06(): Bool =
        List.toChain(10 :: -10 :: Nil) |> Chain1.sum == 0

    @test
    def sum07(): Bool =
        List.range(1, 101) |> List.toChain |> Chain1.sum == 5050


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumWith01(): Bool =
        List.toChain(Nil) |> Chain1.sumWith(x -> x + 1) == 0

    @test
    def sumWith02(): Bool =
        List.toChain(1 :: Nil) |> Chain1.sumWith(x -> x + 1) == 2

    @test
    def sumWith03(): Bool =
        List.toChain(1 :: 2 :: 3 :: Nil) |> Chain1.sumWith(x -> x + 1) == 9

    @test
    def sumWith04(): Bool =
        List.toChain(1 :: 2 :: 3 :: -3 :: Nil) |> Chain1.sumWith(x -> x + 1) == 7

    @test
    def sumWith05(): Bool =
        List.toChain(-1 :: -2 :: -3 :: -4 :: Nil) |> Chain1.sumWith(x -> x + 1) == -6

    @test
    def sumWith06(): Bool =
        List.toChain(10 :: -10 :: Nil) |> Chain1.sumWith(x -> x + 1) == 2


    /////////////////////////////////////////////////////////////////////////////
    // product                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def product01(): Bool =
        List.toChain(Nil) |> Chain1.product == 0

    @test
    def product02(): Bool =
        List.toChain(1 :: Nil) |> Chain1.product == 1

    @test
    def product03(): Bool =
        List.toChain(1 :: 2 :: 3 :: Nil) |> Chain1.product == 6

    @test
    def product04(): Bool =
        List.toChain(1 :: 2 :: 3 :: -3 :: Nil) |> Chain1.product == -18

    @test
    def product05(): Bool =
        List.toChain(-1 :: -2 :: -3 :: -4 :: Nil) |> Chain1.product == 24

    @test
    def product06(): Bool =
        List.toChain(10 :: -10 :: Nil) |> Chain1.product == -100


    /////////////////////////////////////////////////////////////////////////////
    // productWith                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productWith01(): Bool =
        List.toChain(Nil) |> Chain1.productWith(x -> x + 1) == 0

    @test
    def productWith02(): Bool =
        List.toChain(1 :: Nil) |> Chain1.productWith(x -> x + 1) == 2

    @test
    def productWith03(): Bool =
        List.toChain(1 :: 2 :: 3 :: Nil) |> Chain1.productWith(x -> x + 1) == 24

    @test
    def productWith04(): Bool =
        List.toChain(1 :: 2 :: 3 :: -3 :: Nil) |> Chain1.productWith(x -> x + 1) == -48

    @test
    def productWith05(): Bool =
        List.toChain(-2 :: -3 :: -4 :: -5 :: Nil) |> Chain1.productWith(x -> x + 1) == 24

    @test
    def productWith06(): Bool =
        List.toChain(10 :: -10 :: Nil) |> Chain1.productWith(x -> x + 1) == -99


    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatten01(): Bool = Chain1.flatten(Chain1.empty(): Chain[Chain[Unit]]) == Chain1.empty()

    @test
    def flatten02(): Bool = Chain1.flatten(Chain1.singleton(Chain1.empty())): Chain[Unit] == Chain1.empty()

    @test
    def flatten03(): Bool = Chain1.flatten(Chain1.singleton(Chain1.singleton(1))) == Chain1.singleton(1)

    @test
    def flatten04(): Bool = Chain1.flatten(Chain1.singleton(List.toChain(1 :: 2 :: Nil))) == List.toChain(1 :: 2 :: Nil)

    @test
    def flatten05(): Bool = Chain1.flatten(List.toChain(Chain1.empty() :: Chain1.empty() :: Nil)): Chain[Unit] == Chain1.empty()

    @test
    def flatten06(): Bool = Chain1.flatten(List.toChain(Chain1.singleton(1) :: Chain1.empty() :: Nil)) == Chain1.singleton(1)

    @test
    def flatten07(): Bool = Chain1.flatten(List.toChain(Chain1.empty() :: Chain1.singleton(1) :: Nil)) == Chain1.singleton(1)

    @test
    def flatten08(): Bool = Chain1.flatten(List.toChain(Chain1.singleton(1) :: Chain1.singleton(2) :: Nil)) == List.toChain(1 :: 2 :: Nil)

    @test
    def flatten09(): Bool = Chain1.flatten(List.toChain(List.toChain(1 :: 2 :: Nil) :: List.toChain(3 :: 4 :: 5 :: Nil) :: Nil)) == List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: Nil)

    @test
    def flatten10(): Bool = Chain1.flatten(List.toChain(Chain1.singleton(1) :: List.toChain(2 :: 3 :: Nil) :: Chain1.singleton(4) :: Nil)) == List.toChain(1 :: 2 :: 3 :: 4 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def exists01(): Bool = Chain1.exists(i -> i > 3, Chain1.empty()) == false

    @test
    def exists02(): Bool = Chain1.exists(i -> i > 3, Chain1.singleton(1)) == false

    @test
    def exists03(): Bool = Chain1.exists(i -> i > 3, Chain1.singleton(5)) == true

    @test
    def exists04(): Bool = Chain1.exists(i -> i > 3, List.toChain(1 :: 2 :: Nil)) == false

    @test
    def exists05(): Bool = Chain1.exists(i -> i > 3, List.toChain(1 :: 6 :: Nil)) == true

    @test
    def exists06(): Bool = Chain1.exists(i -> i > 3, List.toChain(6 :: 1 :: Nil)) == true

    @test
    def exists07(): Bool = Chain1.exists(i -> i > 3, List.toChain(16 :: 6 :: Nil)) == true

    @test
    def exists08(): Bool = Chain1.exists(i -> i > 3, List.toChain(1 :: -9 :: 3 :: Nil)) == false

    @test
    def exists09(): Bool = Chain1.exists(i -> i > 3, List.toChain(1 :: 9 :: 3 :: Nil)) == true

    /////////////////////////////////////////////////////////////////////////////
    // forall                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forall01(): Bool = Chain1.forall(i -> i > 3, Chain1.empty()) == true

    @test
    def forall02(): Bool = Chain1.forall(i -> i > 3, Chain1.singleton(1)) == false

    @test
    def forall03(): Bool = Chain1.forall(i -> i > 3, Chain1.singleton(5)) == true

    @test
    def forall04(): Bool = Chain1.forall(i -> i > 3, List.toChain(1 :: 2 :: Nil)) == false

    @test
    def forall05(): Bool = Chain1.forall(i -> i > 3, List.toChain(1 :: 6 :: Nil)) == false

    @test
    def forall06(): Bool = Chain1.forall(i -> i > 3, List.toChain(6 :: 1 :: Nil)) == false

    @test
    def forall07(): Bool = Chain1.forall(i -> i > 3, List.toChain(16 :: 6 :: Nil)) == true

    @test
    def forall08(): Bool = Chain1.forall(i -> i > 3, List.toChain(1 :: -9 :: 3 :: Nil)) == false

    @test
    def forall09(): Bool = Chain1.forall(i -> i > 3, List.toChain(1 :: 9 :: 3 :: Nil)) == false

    @test
    def forall10(): Bool = Chain1.forall(i -> i > 3, List.toChain(11 :: 9 :: 31 :: Nil)) == true

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filter01(): Bool = Chain1.filter(i -> i > 3, Chain1.empty()) == Chain1.empty()

    @test
    def filter02(): Bool = Chain1.filter(i -> i > 3, Chain1.singleton(2)) == Chain1.empty()

    @test
    def filter03(): Bool = Chain1.filter(i -> i > 3, Chain1.singleton(4)) == Chain1.singleton(4)

    @test
    def filter04(): Bool = Chain1.filter(i -> i > 3, List.toChain(1 :: 3 :: Nil)) == Chain1.empty()

    @test
    def filter05(): Bool = Chain1.filter(i -> i > 3, List.toChain(1 :: 8 :: Nil)) == Chain1.singleton(8)

    @test
    def filter06(): Bool = Chain1.filter(i -> i > 3, List.toChain(8 :: 1 :: Nil)) == Chain1.singleton(8)

    @test
    def filter07(): Bool = Chain1.filter(i -> i > 3, List.toChain(8 :: 9 :: Nil)) == List.toChain(8 :: 9 :: Nil)

    @test
    def filter08(): Bool = Chain1.filter(i -> i > 3, List.toChain(1 :: 4 :: 11 :: 2 :: -22 :: 17 :: Nil)) == List.toChain(4 :: 11 :: 17 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool = Chain1.foldLeft((i, e) -> (i - e)*(e rem 2 + 1), 100, Chain1.empty()) == 100

    @test
    def foldLeft02(): Bool = Chain1.foldLeft((i, e) -> (i - e)*(e rem 2 + 1), 100, Chain1.singleton(1)) == 198

    @test
    def foldLeft03(): Bool = Chain1.foldLeft((i, e) -> (i - e)*(e rem 2 + 1), 100, List.toChain(1 :: 2 :: Nil)) == 196

    @test
    def foldLeft04(): Bool = Chain1.foldLeft((i, e) -> (i - e)*(e rem 2 + 1), 100, List.toChain(1 :: 2 :: 3 :: Nil)) == 386

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool = Chain1.foldRight((e, i) -> (i - e)*(e rem 2 + 1), 100, Chain1.empty()) == 100

    @test
    def foldRight02(): Bool = Chain1.foldRight((e, i) -> (i - e)*(e rem 2 + 1), 100, Chain1.singleton(1)) == 198

    @test
    def foldRight03(): Bool = Chain1.foldRight((e, i) -> (i - e)*(e rem 2 + 1), 100, List.toChain(1 :: 2 :: Nil)) == 194

    @test
    def foldRight04(): Bool = Chain1.foldRight((e, i) -> (i - e)*(e rem 2 + 1), 100, List.toChain(1 :: 2 :: 3 :: Nil)) == 382

    /////////////////////////////////////////////////////////////////////////////
    // dropLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropLeft01(): Bool = Chain1.dropLeft(-1, Chain1.empty(): Chain[Unit]) == Chain1.empty()

    @test
    def dropLeft02(): Bool = Chain1.dropLeft(0, Chain1.empty(): Chain[Unit]) == Chain1.empty()

    @test
    def dropLeft03(): Bool = Chain1.dropLeft(1, Chain1.empty(): Chain[Unit]) == Chain1.empty()

    @test
    def dropLeft04(): Bool = Chain1.dropLeft(-1, Chain1.singleton(1)) == Chain1.singleton(1)

    @test
    def dropLeft05(): Bool = Chain1.dropLeft(0, Chain1.singleton(1)) == Chain1.singleton(1)

    @test
    def dropLeft06(): Bool = Chain1.dropLeft(1, Chain1.singleton(1)) == Chain1.empty()

    @test
    def dropLeft07(): Bool = Chain1.dropLeft(2, Chain1.singleton(1)) == Chain1.empty()

    @test
    def dropLeft08(): Bool = Chain1.dropLeft(0, List.toChain(1 :: 2 :: Nil)) == List.toChain(1 :: 2 :: Nil)

    @test
    def dropLeft09(): Bool = Chain1.dropLeft(1, List.toChain(1 :: 2 :: Nil)) == Chain1.singleton(2)

    @test
    def dropLeft10(): Bool = Chain1.dropLeft(2, List.toChain(1 :: 2 :: Nil)) == Chain1.empty()

    @test
    def dropLeft11(): Bool = Chain1.dropLeft(2, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)) == List.toChain(3 :: 4 :: 5 :: 6 :: Nil)

    @test
    def dropLeft12(): Bool = Chain1.dropLeft(4, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)) == List.toChain(5 :: 6 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // dropRight                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropRight01(): Bool = Chain1.dropRight(-1, Chain1.empty(): Chain[Unit]) == Chain1.empty()

    @test
    def dropRight02(): Bool = Chain1.dropRight(0, Chain1.empty(): Chain[Unit]) == Chain1.empty()

    @test
    def dropRight03(): Bool = Chain1.dropRight(1, Chain1.empty(): Chain[Unit]) == Chain1.empty()

    @test
    def dropRight04(): Bool = Chain1.dropRight(-1, Chain1.singleton(1)) == Chain1.singleton(1)

    @test
    def dropRight05(): Bool = Chain1.dropRight(0, Chain1.singleton(1)) == Chain1.singleton(1)

    @test
    def dropRight06(): Bool = Chain1.dropRight(1, Chain1.singleton(1)) == Chain1.empty()

    @test
    def dropRight07(): Bool = Chain1.dropRight(2, Chain1.singleton(1)) == Chain1.empty()

    @test
    def dropRight08(): Bool = Chain1.dropRight(0, List.toChain(1 :: 2 :: Nil)) == List.toChain(1 :: 2 :: Nil)

    @test
    def dropRight09(): Bool = Chain1.dropRight(1, List.toChain(1 :: 2 :: Nil)) == Chain1.singleton(1)

    @test
    def dropRight10(): Bool = Chain1.dropRight(2, List.toChain(1 :: 2 :: Nil)) == Chain1.empty()

    @test
    def dropRight11(): Bool = Chain1.dropRight(2, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)) == List.toChain(1 :: 2 :: 3 :: 4 :: Nil)

    @test
    def dropRight12(): Bool = Chain1.dropRight(4, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)) == List.toChain(1 :: 2 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // dropWhileLeft                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhileLeft01(): Bool = Chain1.dropWhileLeft(i -> i > 3, Chain1.empty()) == Chain1.empty()

    @test
    def dropWhileLeft02(): Bool = Chain1.dropWhileLeft(i -> i > 3, Chain1.singleton(1)) == Chain1.singleton(1)

    @test
    def dropWhileLeft03(): Bool = Chain1.dropWhileLeft(i -> i > 3, Chain1.singleton(4)) == Chain1.empty()

    @test
    def dropWhileLeft04(): Bool = Chain1.dropWhileLeft(i -> i > 3, List.toChain(1 :: 2 :: Nil)) == List.toChain(1 :: 2 :: Nil)

    @test
    def dropWhileLeft05(): Bool = Chain1.dropWhileLeft(i -> i > 3, List.toChain(1 :: 5 :: Nil)) == List.toChain(1 :: 5 :: Nil)

    @test
    def dropWhileLeft06(): Bool = Chain1.dropWhileLeft(i -> i > 3, List.toChain(5 :: 1 :: Nil)) == Chain1.singleton(1)

    @test
    def dropWhileLeft07(): Bool = Chain1.dropWhileLeft(i -> i > 3, List.toChain(5 :: 8 :: Nil)) == Chain1.empty()

    @test
    def dropWhileLeft08(): Bool = Chain1.dropWhileLeft(i -> i > 3, List.toChain(4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil)) ==
                            List.toChain(-3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // dropWhileRight                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhileRight01(): Bool = Chain1.dropWhileRight(i -> i > 3, Chain1.empty()) == Chain1.empty()

    @test
    def dropWhileRight02(): Bool = Chain1.dropWhileRight(i -> i > 3, Chain1.singleton(1)) == Chain1.singleton(1)

    @test
    def dropWhileRight03(): Bool = Chain1.dropWhileRight(i -> i > 3, Chain1.singleton(4)) == Chain1.empty()

    @test
    def dropWhileRight04(): Bool = Chain1.dropWhileRight(i -> i > 3, List.toChain(1 :: 2 :: Nil)) == List.toChain(1 :: 2 :: Nil)

    @test
    def dropWhileRight05(): Bool = Chain1.dropWhileRight(i -> i > 3, List.toChain(1 :: 5 :: Nil)) == Chain1.singleton(1)

    @test
    def dropWhileRight06(): Bool = Chain1.dropWhileRight(i -> i > 3, List.toChain(5 :: 1 :: Nil)) == List.toChain(5 :: 1 :: Nil)

    @test
    def dropWhileRight07(): Bool = Chain1.dropWhileRight(i -> i > 3, List.toChain(5 :: 8 :: Nil)) == Chain1.empty()

    @test
    def dropWhileRight08(): Bool = Chain1.dropWhileRight(i -> i > 3, List.toChain(4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil)) ==
                            List.toChain(4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // takeLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeLeft01(): Bool = Chain1.takeLeft(-1, Chain1.empty(): Chain[Unit]) == Chain1.empty()

    @test
    def takeLeft02(): Bool = Chain1.takeLeft(0, Chain1.empty(): Chain[Unit]) == Chain1.empty()

    @test
    def takeLeft03(): Bool = Chain1.takeLeft(1, Chain1.empty(): Chain[Unit]) == Chain1.empty()

    @test
    def takeLeft04(): Bool = Chain1.takeLeft(-1, Chain1.singleton(1)) == Chain1.empty()

    @test
    def takeLeft05(): Bool = Chain1.takeLeft(0, Chain1.singleton(1)) == Chain1.empty()

    @test
    def takeLeft06(): Bool = Chain1.takeLeft(1, Chain1.singleton(1)) == Chain1.singleton(1)

    @test
    def takeLeft07(): Bool = Chain1.takeLeft(2, Chain1.singleton(1)) == Chain1.singleton(1)

    @test
    def takeLeft08(): Bool = Chain1.takeLeft(0, List.toChain(1 :: 2 :: Nil)) == Chain1.empty()

    @test
    def takeLeft09(): Bool = Chain1.takeLeft(1, List.toChain(1 :: 2 :: Nil)) == Chain1.singleton(1)

    @test
    def takeLeft10(): Bool = Chain1.takeLeft(2, List.toChain(1 :: 2 :: Nil)) == List.toChain(1 :: 2 :: Nil)

    @test
    def takeLeft11(): Bool = Chain1.takeLeft(2, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)) == List.toChain(1 :: 2 :: Nil)

    @test
    def takeLeft12(): Bool = Chain1.takeLeft(4, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)) == List.toChain(1 :: 2 :: 3 :: 4 :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // takeRight                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeRight01(): Bool = Chain1.takeRight(-1, Chain1.empty(): Chain[Unit]) == Chain1.empty()

    @test
    def takeRight02(): Bool = Chain1.takeRight(0, Chain1.empty(): Chain[Unit]) == Chain1.empty()

    @test
    def takeRight03(): Bool = Chain1.takeRight(1, Chain1.empty(): Chain[Unit]) == Chain1.empty()

    @test
    def takeRight04(): Bool = Chain1.takeRight(-1, Chain1.singleton(1)) == Chain1.empty()

    @test
    def takeRight05(): Bool = Chain1.takeRight(0, Chain1.singleton(1)) == Chain1.empty()

    @test
    def takeRight06(): Bool = Chain1.takeRight(1, Chain1.singleton(1)) == Chain1.singleton(1)

    @test
    def takeRight07(): Bool = Chain1.takeRight(2, Chain1.singleton(1)) == Chain1.singleton(1)

    @test
    def takeRight08(): Bool = Chain1.takeRight(0, List.toChain(1 :: 2 :: Nil)) == Chain1.empty()

    @test
    def takeRight09(): Bool = Chain1.takeRight(1, List.toChain(1 :: 2 :: Nil)) == Chain1.singleton(2)

    @test
    def takeRight10(): Bool = Chain1.takeRight(2, List.toChain(1 :: 2 :: Nil)) == List.toChain(1 :: 2 :: Nil)

    @test
    def takeRight11(): Bool = Chain1.takeRight(2, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)) == List.toChain(5 :: 6 :: Nil)

    @test
    def takeRight12(): Bool = Chain1.takeRight(4, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)) == List.toChain(3 :: 4 :: 5 :: 6 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // takeWhileLeft                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhileLeft01(): Bool = Chain1.takeWhileLeft(i -> i > 3, Chain1.empty(): Chain[Int32]) == Chain1.empty()

    @test
    def takeWhileLeft02(): Bool = Chain1.takeWhileLeft(i -> i > 3, Chain1.singleton(1)) == Chain1.empty()

    @test
    def takeWhileLeft03(): Bool = Chain1.takeWhileLeft(i -> i > 3, Chain1.singleton(4)) == Chain1.singleton(4)

    @test
    def takeWhileLeft04(): Bool = Chain1.takeWhileLeft(i -> i > 3, List.toChain(1 :: 2 :: Nil)) == Chain1.empty()

    @test
    def takeWhileLeft05(): Bool = Chain1.takeWhileLeft(i -> i > 3, List.toChain(1 :: 5 :: Nil)) == Chain1.empty()

    @test
    def takeWhileLeft06(): Bool = Chain1.takeWhileLeft(i -> i > 3, List.toChain(5 :: 1 :: Nil)) == Chain1.singleton(5)

    @test
    def takeWhileLeft07(): Bool = Chain1.takeWhileLeft(i -> i > 3, List.toChain(5 :: 8 :: Nil)) == List.toChain(5 :: 8 :: Nil)

    @test
    def takeWhileLeft08(): Bool = Chain1.takeWhileLeft(i -> i > 3, List.toChain(4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil)) ==
                            List.toChain(4 :: 6 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // takeWhileRight                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhileRight01(): Bool = Chain1.takeWhileRight(i -> i > 3, Chain1.empty(): Chain[Int32]) == Chain1.empty()

    @test
    def takeWhileRight02(): Bool = Chain1.takeWhileRight(i -> i > 3, Chain1.singleton(1)) == Chain1.empty()

    @test
    def takeWhileRight03(): Bool = Chain1.takeWhileRight(i -> i > 3, Chain1.singleton(4)) == Chain1.singleton(4)

    @test
    def takeWhileRight04(): Bool = Chain1.takeWhileRight(i -> i > 3, List.toChain(1 :: 2 :: Nil)) == Chain1.empty()

    @test
    def takeWhileRight05(): Bool = Chain1.takeWhileRight(i -> i > 3, List.toChain(1 :: 5 :: Nil)) == Chain1.singleton(5)

    @test
    def takeWhileRight06(): Bool = Chain1.takeWhileRight(i -> i > 3, List.toChain(5 :: 1 :: Nil)) == Chain1.empty()

    @test
    def takeWhileRight07(): Bool = Chain1.takeWhileRight(i -> i > 3, List.toChain(5 :: 8 :: Nil)) == List.toChain(5 :: 8 :: Nil)

    @test
    def takeWhileRight08(): Bool = Chain1.takeWhileRight(i -> i > 3, List.toChain(4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil)) ==
                            Chain1.singleton(7)

    /////////////////////////////////////////////////////////////////////////////
    // filterMap                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterMap01(): Bool = Chain1.filterMap(i -> if (i rem 2 == 0) Some(i/2) else None, Chain1.empty()) == Chain1.empty()

    @test
    def filterMap02(): Bool = Chain1.filterMap(i -> if (i rem 2 == 0) Some(i/2) else None, Chain1.singleton(1)) == Chain1.empty()

    @test
    def filterMap03(): Bool = Chain1.filterMap(i -> if (i rem 2 == 0) Some(i/2) else None, Chain1.singleton(2)) == Chain1.singleton(1)

    @test
    def filterMap04(): Bool = Chain1.filterMap(i -> if (i rem 2 == 0) Some(i/2) else None, List.toChain(1 :: 3 :: Nil)) == Chain1.empty()

    @test
    def filterMap05(): Bool = Chain1.filterMap(i -> if (i rem 2 == 0) Some(i/2) else None, List.toChain(1 :: 4 :: Nil)) == Chain1.singleton(2)

    @test
    def filterMap06(): Bool = Chain1.filterMap(i -> if (i rem 2 == 0) Some(i/2) else None, List.toChain(6 :: -1 :: Nil)) == Chain1.singleton(3)

    @test
    def filterMap07(): Bool = Chain1.filterMap(i -> if (i rem 2 == 0) Some(i/2) else None, List.toChain(8 :: 6 :: Nil)) == List.toChain(4 :: 3 :: Nil)

    @test
    def filterMap08(): Bool = Chain1.filterMap(i -> if (i rem 2 == 0) Some(i/2) else None, List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 10 :: Nil)) ==
                            List.toChain(0 :: 1 :: 2 :: 5 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // findMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findMap01(): Bool = Chain1.findMap(i -> if (i rem 2 == 0) Some(i/2) else None, Chain1.empty()) == None

    @test
    def findMap02(): Bool = Chain1.findMap(i -> if (i rem 2 == 0) Some(i/2) else None, Chain1.singleton(1)) == None

    @test
    def findMap03(): Bool = Chain1.findMap(i -> if (i rem 2 == 0) Some(i/2) else None, Chain1.singleton(2)) == Some(1)

    @test
    def findMap04(): Bool = Chain1.findMap(i -> if (i rem 2 == 0) Some(i/2) else None, List.toChain(1 :: 3 :: Nil)) == None

    @test
    def findMap05(): Bool = Chain1.findMap(i -> if (i rem 2 == 0) Some(i/2) else None, List.toChain(1 :: 4 :: Nil)) == Some(2)

    @test
    def findMap06(): Bool = Chain1.findMap(i -> if (i rem 2 == 0) Some(i/2) else None, List.toChain(6 :: -1 :: Nil)) == Some(3)

    @test
    def findMap07(): Bool = Chain1.findMap(i -> if (i rem 2 == 0) Some(i/2) else None, List.toChain(8 :: 6 :: Nil)) == Some(4)

    @test
    def findMap08(): Bool = Chain1.findMap(i -> if (i rem 2 == 0) Some(i/2) else None, List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 10 :: Nil)) == Some(0)

    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zip01(): Bool = Chain1.zip(Chain1.empty(): Chain[Unit], Chain1.empty(): Chain[Unit]) == Chain1.empty()

    @test
    def zip02(): Bool = Chain1.zip(Chain1.singleton(1), Chain1.empty(): Chain[Unit]) == Chain1.empty()

    @test
    def zip03(): Bool = Chain1.zip(Chain1.empty(): Chain[Unit], Chain1.singleton(2)) == Chain1.empty()

    @test
    def zip04(): Bool = Chain1.zip(Chain1.singleton(1), Chain1.singleton(2)) == Chain1.singleton((1, 2))

    @test
    def zip05(): Bool = Chain1.zip(List.toChain(1 :: 3 :: Nil), List.toChain(2 :: 4 :: Nil)) == List.toChain((1, 2) :: (3, 4) :: Nil)

    @test
    def zip06(): Bool = Chain1.zip(List.toChain(1 :: 3 :: 5 :: Nil), List.toChain(2 :: 4 :: 6 :: Nil)) == List.toChain((1, 2) :: (3, 4) :: (5, 6) :: Nil)

    @test
    def zip07(): Bool = Chain1.zip(List.toChain(1 :: 3 :: 5 :: 7 :: Nil), List.toChain(2 :: 4 :: 6 :: 8 :: Nil)) == List.toChain((1, 2) :: (3, 4) :: (5, 6) :: (7, 8) :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // zipWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWith01(): Bool = Chain1.zipWith((a, b) -> if (b) a+1 else a, Chain1.empty(), Chain1.empty()) == Chain1.empty()

    @test
    def zipWith02(): Bool = Chain1.zipWith((a, b) -> if (b) a+1 else a, Chain1.singleton(1), Chain1.empty()) == Chain1.empty()

    @test
    def zipWith03(): Bool = Chain1.zipWith((a, b) -> if (b) a+1 else a, Chain1.empty(), Chain1.singleton(true)) == Chain1.empty()

    @test
    def zipWith04(): Bool = Chain1.zipWith((a, b) -> if (b) a+1 else a, Chain1.singleton(1), Chain1.singleton(true)) == Chain1.singleton(2)

    @test
    def zipWith05(): Bool = Chain1.zipWith((a, b) -> if (b) a+1 else a, Chain1.singleton(1), Chain1.singleton(false)) == Chain1.singleton(1)

    @test
    def zipWith06(): Bool = Chain1.zipWith((a, b) -> if (b) a+1 else a, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil),
                          List.toChain(false :: true :: true :: false :: false :: true :: true :: true :: Nil)) ==
                          List.toChain(1 :: 3 :: 4 :: 4 :: 5 :: 7 :: 8 :: 9 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // unzip                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unzip01(): Bool = Chain1.unzip(Chain1.empty(): Chain[(Unit, Unit)]) == (Chain1.empty(), Chain1.empty())

    @test
    def unzip02(): Bool = Chain1.unzip(Chain1.singleton((1, true))) == (Chain1.singleton(1), Chain1.singleton(true))

    @test
    def unzip03(): Bool = Chain1.unzip(List.toChain((1, true) :: (2, true) :: Nil)) == (List.toChain(1 :: 2 :: Nil), List.toChain(true :: true :: Nil))

    @test
    def unzip04(): Bool = Chain1.unzip(List.toChain((1, true) :: (2, true) :: (3, false) :: Nil)) ==
                        (List.toChain(1 :: 2 :: 3 :: Nil), List.toChain(true :: true :: false :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // mapAccumLeft                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapAccumLeft01(): Bool = Chain1.mapAccumLeft((s, x) -> (s+1, x+s), 0, Chain1.empty()) == (0, Chain1.empty())

    @test
    def mapAccumLeft02(): Bool = Chain1.mapAccumLeft((s, x) -> (s+1, x+s), 0, Chain1.singleton(1)) == (1, Chain1.singleton(1))

    @test
    def mapAccumLeft03(): Bool = Chain1.mapAccumLeft((s, x) -> (s+1, x+s), 0, List.toChain(1 :: 1 :: Nil)) == (2, List.toChain(1 :: 2 :: Nil))

    @test
    def mapAccumLeft04(): Bool = Chain1.mapAccumLeft((s, x) -> (s+1, x+s), 0, List.toChain(1 :: 1 :: 1 :: Nil)) == (3, List.toChain(1 :: 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // mapAccumRight                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapAccumRight01(): Bool = Chain1.mapAccumRight((s, x) -> (s+1, x+s), 0, Chain1.empty()) == (0, Chain1.empty())

    @test
    def mapAccumRight02(): Bool = Chain1.mapAccumRight((s, x) -> (s+1, x+s), 0, Chain1.singleton(1)) == (1, Chain1.singleton(1))

    @test
    def mapAccumRight03(): Bool = Chain1.mapAccumRight((s, x) -> (s+1, x+s), 0, List.toChain(1 :: 1 :: Nil)) == (2, List.toChain(2 :: 1 :: Nil))

    @test
    def mapAccumRight04(): Bool = Chain1.mapAccumRight((s, x) -> (s+1, x+s), 0, List.toChain(1 :: 1 :: 1 :: Nil)) == (3, List.toChain(3 :: 2 :: 1 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // foreach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foreach01(): Bool & Impure =
        let r = ref 21;
        Chain1.foreach(x -> r := x, Chain1.empty());
        21 == deref r

    @test
    def foreach02(): Bool & Impure =
        let r = ref 21;
        Chain1.foreach(x -> r := x, List.toChain(1 :: 2 :: 3 :: Nil));
        3 == deref r

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toList01(): Bool =
        Chain1.toList(Chain1.empty(): Chain[Int32]) == Nil: List[Int32]

    @test
    def toList02(): Bool =
        Chain1.toList(Chain1.singleton(1)) == 1 :: Nil

    @test
    def toList03(): Bool =
        Chain1.toList(List.toChain(1 :: 2 :: Nil)) == 1 :: 2 :: Nil

    @test
    def toList04(): Bool =
        Chain1.toList(List.toChain(1 :: 2 :: 3 :: Nil)) == 1 :: 2 :: 3 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // toMutList                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMutList01(): Bool & Impure =
        Chain1.toMutList(Chain1.empty(): Chain[Int32]) `MutList.sameElements` List.toMutList(Nil): MutList[Int32]

    @test
    def toMutList02(): Bool & Impure =
        Chain1.toMutList(Chain1.singleton(1)) `MutList.sameElements` List.toMutList(1 :: Nil)

    @test
    def toMutList03(): Bool & Impure =
        Chain1.toMutList(List.toChain(1 :: 2 :: Nil)) `MutList.sameElements` List.toMutList(1 :: 2 :: Nil)

    @test
    def toMutList04(): Bool & Impure =
        Chain1.toMutList(List.toChain(1 :: 2 :: 3 :: Nil)) `MutList.sameElements` List.toMutList(1 :: 2 :: 3 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toSet01(): Bool =
        Chain1.toSet(Chain1.empty(): Chain[Int32]) == Set#{}: Set[Int32]

    @test
    def toSet02(): Bool =
        Chain1.toSet(Chain1.singleton(1)) == Set#{1}

    @test
    def toSet03(): Bool =
        Chain1.toSet(List.toChain(1 :: 2 :: Nil)) == Set#{1, 2}

    @test
    def toSet04(): Bool =
        Chain1.toSet(List.toChain(1 :: 2 :: 3 :: Nil)) == Set#{1, 2, 3}

    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMap01(): Bool =
        Chain1.toMap(Chain1.empty(): Chain[(Int32, Int32)]) == Map#{}: Map[Int32, Int32]

    @test
    def toMap02(): Bool =
        Chain1.toMap(Chain1.singleton((1, 1))) == Map#{1 => 1}

    @test
    def toMap03(): Bool =
        Chain1.toMap(List.toChain((1, 1) :: (2, 2) :: Nil)) == Map#{1 => 1, 2 => 2}

    @test
    def toMap04(): Bool =
        Chain1.toMap(List.toChain((1, 1) :: (2, 2) :: (3, 3) :: Nil)) == Map#{1 => 1, 2 => 2, 3 => 3}

    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toArray01(): Bool & Impure =
        let c: Chain[Int32] = Chain1.empty();
        let a: Array[Int32] = Chain1.toArray(c);
        Array.sameElements(a, []: Array[Int32])

    @test
    def toArray02(): Bool & Impure =
        let a = Chain1.toArray(Chain1.singleton(1));
        Array.sameElements(a, [1])

    @test
    def toArray03(): Bool & Impure =
        let a = Chain1.toArray(List.toChain(1 :: 2 :: Nil));
        Array.sameElements(a, [1,2])

    @test
    def toArray04(): Bool & Impure =
        let a = Chain1.toArray(List.toChain(1 :: 2 :: 3 :: Nil));
        Array.sameElements(a, [1,2,3])

    /////////////////////////////////////////////////////////////////////////////
    // sort                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    def testSortVsSortWith(c: Chain[Int32]) : Bool =
        Chain1.sort(c) == Chain1.sortWith(cmp, c)

    @test
    def sort01(): Bool = testSortVsSortWith(Chain1.empty(): Chain[Int32])

    @test
    def sort02(): Bool = testSortVsSortWith(Chain1.singleton(0))

    @test
    def sort03(): Bool = testSortVsSortWith(List.toChain(0 :: 1 :: Nil))

    @test
    def sort04(): Bool = testSortVsSortWith(List.toChain(1 :: 0 :: Nil))

    @test
    def sort05(): Bool = testSortVsSortWith(List.toChain(1 :: 1 :: Nil))

    @test
    def sort06(): Bool = testSortVsSortWith(List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil))

    @test
    def sort07(): Bool = testSortVsSortWith(List.toChain(5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil))

    @test
    def sort08(): Bool = testSortVsSortWith(List.toChain(5 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil))

    @test
    def sort09(): Bool = testSortVsSortWith(List.toChain(2 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // sortBy                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    def testSortByVsSort(a: Chain[Int32]) : Bool =
        (Chain1.sortBy(identity, a) == Chain1.sort(a)) and
        (Chain1.sortBy(x -> 4*x+7, a) == Chain1.sort(a)) and
        (Chain1.sortBy(x -> -x, a) == Chain1.sortWith(flip(cmp),a))

    @test
    def sortBy01(): Bool = testSortByVsSort(Chain1.empty(): Chain[Int32])

    @test
    def sortBy02(): Bool = testSortByVsSort(Chain1.singleton(0))

    @test
    def sortBy03(): Bool = testSortByVsSort(List.toChain(0 :: 1 :: Nil))

    @test
    def sortBy04(): Bool = testSortByVsSort(List.toChain(1 :: 0 :: Nil))

    @test
    def sortBy05(): Bool = testSortByVsSort(List.toChain(1 :: 1 :: Nil))

    @test
    def sortBy06(): Bool = testSortByVsSort(List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil))

    @test
    def sortBy07(): Bool = testSortByVsSort(List.toChain(5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil))

    @test
    def sortBy08(): Bool = testSortByVsSort(List.toChain(5 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil))

    @test
    def sortBy09(): Bool = testSortByVsSort(List.toChain(2 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // sortWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    def cmp(x: Int32, y: Int32): Comparison =
        if (x < y) LessThan
        else if (x == y) EqualTo
        else GreaterThan

    @test
    def sortWith01(): Bool =
        Chain1.sortWith(cmp, Chain1.empty(): Chain[Int32]) == Chain1.empty()

    @test
    def sortWith02(): Bool =
        Chain1.sortWith(cmp, Chain1.singleton(0)) == Chain1.singleton(0)

    @test
    def sortWith03(): Bool =
        Chain1.sortWith(cmp, List.toChain(0 :: 1 :: Nil)) == List.toChain(0 :: 1 :: Nil)

    @test
    def sortWith04(): Bool =
        Chain1.sortWith(cmp, List.toChain(1 :: 0 :: Nil)) == List.toChain(0 :: 1 :: Nil)

    @test
    def sortWith05(): Bool =
        Chain1.sortWith(cmp, List.toChain(1 :: 1 :: Nil)) == List.toChain(1 :: 1 :: Nil)

    @test
    def sortWith06(): Bool =
        Chain1.sortWith(cmp, List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil)) == List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil)

    @test
    def sortWith07(): Bool =
        Chain1.sortWith(cmp, List.toChain(5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil)) == List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil)

    @test
    def sortWith08(): Bool =
        Chain1.sortWith(cmp, List.toChain(5 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil)) == List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil)

    @test
    def sortWith09(): Bool =
        Chain1.sortWith(cmp, List.toChain(2 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil)) == List.toChain(0 :: 1 :: 2 :: 2 :: 3 :: 4 :: Nil)

    @test
    def sortWith10(): Bool =
        Chain1.sortWith(flip(cmp), List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil)) == List.toChain(5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil)

    @test
    def sortWith11(): Bool =
        Chain1.sortWith(flip(cmp), List.toChain(5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil)) == List.toChain(5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil)

    @test
    def sortWith12(): Bool =
        Chain1.sortWith(flip(cmp), List.toChain(5 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil)) == List.toChain(5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil)

    @test
    def sortWith13(): Bool =
        Chain1.sortWith(flip(cmp), List.toChain(2 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil)) == List.toChain(4 :: 3 :: 2 :: 2 :: 1 :: 0 :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def join01(): Bool =
        Nil: List[Int32] |> List.toChain |> Chain1.join(",") == ""

    @test
    def join02(): Bool =
        (1 :: Nil) |> List.toChain |> Chain1.join(",") == "1"

    @test
    def join03(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toChain |> Chain1.join(",") == "1,2,3"

    @test
    def join04(): Bool =
        ("1" :: "2" :: "3" :: Nil) |> List.toChain |> Chain1.join(",") == "1,2,3"


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinWith01(): Bool =
        (Nil: List[Int32]) |> List.toChain |>
            Chain1.joinWith(x -> "${x + 1}", ",") == ""

    @test
    def joinWith02(): Bool =
        (1 :: Nil) |> List.toChain |>
            Chain1.joinWith(x -> "${x + 1}", ",") == "2"

    @test
    def joinWith03(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toChain |>
            Chain1.joinWith(x -> "${x + 1}", ",") == "2,3,4"

    @test
    def joinWith04(): Bool =
        ("1" :: "2" :: "3" :: Nil) |> List.toChain |>
            Chain1.joinWith(x -> x + x, ",") == "11,22,33"

}
*/
