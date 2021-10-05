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

namespace TestStream {

    /////////////////////////////////////////////////////////////////////////////
    // empty                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def empty01(): Bool =
        Stream1.empty(): Stream[Unit, Unit] |> Stream1.toList == Nil

    @test
    def empty02(): Bool =
        not (Stream1.empty(): Stream[Int32, Int32] |> Stream1.toList == 1 :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isEmpty01(): Bool =
        Stream1.empty(): Stream[Int32, Unit] |> Stream1.isEmpty

    @test
    def isEmpty02(): Bool =
        not (List1.toStream(1 :: Nil) |> Stream1.isEmpty)


    /////////////////////////////////////////////////////////////////////////////
    // toStream & toList                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toStream01(): Bool =
        List1.toStream(Nil): Stream[Unit, List[Unit]] |> Stream1.toList == Nil

    @test
    def toStream02(): Bool =
        List1.toStream(1 :: Nil) |> Stream1.toList == 1 :: Nil
    
    @test
    def toStream03(): Bool =
        List1.toStream(1 :: 2 :: 3 :: Nil) |> Stream1.toList == 1 :: 2 :: 3 :: Nil

    @test
    def toStream04(): Bool =
        List.range(-1000, 1000) |>
            List1.toStream |>
            Stream1.toList == List.range(-1000, 1000)


    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def range01(): Bool =
        Stream1.range(0, 0) |> Stream1.isEmpty

    @test
    def range02(): Bool =
        Stream1.range(100, 100) |> Stream1.isEmpty

    @test
    def range03(): Bool =
        Stream1.range(1, 0) |> Stream1.isEmpty

    @test
    def range04(): Bool =
        not (Stream1.range(0, 100) |> Stream1.isEmpty)

    @test
    def range05(): Bool =
        Stream1.range(0, 100) |> Stream1.toList == List.range(0, 100)

    @test
    def range06(): Bool =
        not (Stream1.range(-100, 100) |> Stream1.isEmpty)

    @test
    def range07(): Bool =
        Stream1.range(-100, 100) |> Stream1.toList == List.range(-100, 100)


    /////////////////////////////////////////////////////////////////////////////
    // filter (pure)                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterPure01(): Bool =
        Nil |> List.toStream |> Stream1.filter(x -> x > 100) |> Stream1.toList == Nil

    @test
    def filterPure02(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.filter(x -> x > 0) |> Stream1.toList == 1 :: 2 :: 3 :: Nil

    @test
    def filterPure03(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.filter(x -> x > 1) |> Stream1.toList == 2 :: 3 :: Nil

    @test
    def filterPure04(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.filter(x -> x > 2) |> Stream1.toList == 3 :: Nil

    @test
    def filterPure05(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.filter(x -> x > 3) |> Stream1.toList == Nil

    @test
    def filterPure06(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.filter(x -> x > 100) |> Stream1.toList == Nil


    /////////////////////////////////////////////////////////////////////////////
    // filter (impure)                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterImpure01(): Bool & Impure =
        Nil |> List.toStream |> Stream1.filter(x -> x > 100 as & Impure) |> Stream1.toList == Nil

    @test
    def filterImpure02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.filter(x -> x > 0 as & Impure) |> Stream1.toList == 1 :: 2 :: 3 :: Nil

    @test
    def filterImpure03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.filter(x -> x > 1 as & Impure) |> Stream1.toList == 2 :: 3 :: Nil

    @test
    def filterImpure04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.filter(x -> x > 2 as & Impure) |> Stream1.toList == 3 :: Nil

    @test
    def filterImpure05(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.filter(x -> x > 3 as & Impure) |> Stream1.toList == Nil

    @test
    def filterImpure06(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.filter(x -> x > 100 as & Impure) |> Stream1.toList == Nil


    /////////////////////////////////////////////////////////////////////////////
    // filter filter                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterFilter01(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.filter(x -> x > 1) |> Stream1.filter(x -> x < 3) |> Stream1.toList == 2 :: Nil

    @test
    def filterFilter02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.filter(x -> x > 1 as & Impure) |> Stream1.filter(x -> x < 3) |> Stream1.toList == 2 :: Nil

    @test
    def filterFilter03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.filter(x -> x > 1) |> Stream1.filter(x -> x < 3 as & Impure) |> Stream1.toList == 2 :: Nil

    @test
    def filterFilter04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.filter(x -> x > 1 as & Impure) |> Stream1.filter(x -> x < 3 as & Impure) |> Stream1.toList == 2 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // filter filter fusion                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toStream |>
        Stream1.filter(_ -> { l := "a" :: deref l; true }) |>
        Stream1.filter(_ -> { l := "b" :: deref l; true });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def filterFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toStream |>
        Stream1.filter(_x -> { l := "a" :: deref l; true } as & Pure) |>
        Stream1.filter(_x -> { l := "b" :: deref l; true } as & Pure) |>
        Stream1.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // map (pure)                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapPure01(): Bool =
        Nil |> List.toStream |> Stream1.map(x -> x + 1) |> Stream1.toList == Nil

    @test
    def mapPure02(): Bool =
        (1 :: Nil) |> List.toStream |> Stream1.map(x -> x + 1) |> Stream1.toList == 2 :: Nil

    @test
    def mapPure03(): Bool =
        (1 :: 2 :: Nil) |> List.toStream |> Stream1.map(x -> x + 1) |> Stream1.toList == 2 :: 3 :: Nil

    @test
    def mapPure04(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.map(x -> x + 1) |> Stream1.toList == 2 :: 3 :: 4 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // map (impure)                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapImpure01(): Bool & Impure =
        Nil |> List.toStream |> Stream1.map(x -> x + 1 as & Impure) |> Stream1.toList == Nil

    @test
    def mapImpure02(): Bool & Impure =
        (1 :: Nil) |> List.toStream |> Stream1.map(x -> x + 1 as & Impure) |> Stream1.toList == 2 :: Nil

    @test
    def mapImpure03(): Bool & Impure =
        (1 :: 2 :: Nil) |> List.toStream |> Stream1.map(x -> x + 1 as & Impure) |> Stream1.toList == 2 :: 3 :: Nil

    @test
    def mapImpure04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.map(x -> x + 1 as & Impure) |> Stream1.toList == 2 :: 3 :: 4 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // map map                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapMap01(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.map(x -> x + 1) |> Stream1.map(x -> x * 2) |> Stream1.toList == 4 :: 6 :: 8 :: Nil

    @test
    def mapMap02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.map(x -> x + 1 as & Impure) |> Stream1.map(x -> x * 2) |> Stream1.toList == 4 :: 6 :: 8 :: Nil

    @test
    def mapMap03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.map(x -> x + 1) |> Stream1.map(x -> x * 2 as & Impure) |> Stream1.toList == 4 :: 6 :: 8 :: Nil

    @test
    def mapMap04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.map(x -> x + 1 as & Impure) |> Stream1.map(x -> x * 2 as & Impure) |> Stream1.toList == 4 :: 6 :: 8 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // map map fusion                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toStream |>
        Stream1.map(x -> { l := "a" :: deref l; x }) |>
        Stream1.map(x -> { l := "b" :: deref l; x });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def mapFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toStream |>
        Stream1.map(x -> { l := "a" :: deref l; x } as & Pure) |>
        Stream1.map(x -> { l := "b" :: deref l; x } as & Pure) |>
        Stream1.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // map filter fusion                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapFilterFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toStream |>
        Stream1.map(   x -> { l := "a" :: deref l; x }) |>
        Stream1.filter(_ -> { l := "b" :: deref l; true });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def mapFilterFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toStream |>
        Stream1.map(   x -> { l := "a" :: deref l; x }    as & Pure) |>
        Stream1.filter(_ -> { l := "b" :: deref l; true } as & Pure) |>
        Stream1.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // filter map fusion                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterMapFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toStream |>
        Stream1.filter(_ -> { l := "a" :: deref l; true }) |>
        Stream1.map(   x -> { l := "b" :: deref l; x    });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def filterMapFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toStream |>
        Stream1.filter(_ -> { l := "a" :: deref l; true } as & Pure) |>
        Stream1.map(   x -> { l := "b" :: deref l; x    } as & Pure) |>
        Stream1.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)

    
    /////////////////////////////////////////////////////////////////////////////    
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool =
    	Stream1.foldLeft((i, e) -> (i - e) * (e % 2 + 1), 100, Stream.empty(): Stream[Int32, Int32]) == 100

    @test
    def foldLeft02(): Bool =
    	(1 :: Nil) |> List.toStream |> Stream1.foldLeft((i, e) -> (i - e) * (e % 2 + 1), 100) == 198

    @test
    def foldLeft03(): Bool =
    	(1 :: 2 :: Nil) |> List.toStream |> Stream1.foldLeft((i, e) -> (i - e) * (e % 2 + 1), 100) == 196

    @test
    def foldLeft04(): Bool =
    	(1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.foldLeft((i, e) -> (i - e) * (e % 2 + 1), 100) == 386

    @test
    def foldLeft05(): Bool =
    	(1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.foldLeft((acc, x) -> x :: acc, Nil) == 3 :: 2 :: 1 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool =
    	Stream1.foldRight((e, i) -> (i - e) * (e % 2 + 1), 100, Stream.empty(): Stream[Int32, Int32]) == 100

    @test
    def foldRight02(): Bool =
    	(1 :: Nil) |> List.toStream |> Stream1.foldRight((e, i) -> (i - e) * (e % 2 + 1), 100) == 198

    @test
    def foldRight03(): Bool =
    	(1 :: 2 :: Nil) |> List.toStream |> Stream1.foldRight((e, i) -> (i - e) * (e % 2 + 1), 100) == 194

    @test
    def foldRight04(): Bool =
    	(1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.foldRight((e, i) -> (i - e) * (e % 2 + 1), 100) == 382

    @test
    def foldRight05(): Bool =
    	(1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.foldRight((x, acc) -> x :: acc, Nil) == 1 :: 2 :: 3 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // take                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def take01(): Bool =
        (Stream1.empty()): Stream[Int32, Unit] |> Stream1.take(1000) |> Stream1.isEmpty

    @test
    def take02(): Bool =
        (Stream1.empty()): Stream[Int32, Unit] |> Stream1.take(0) |> Stream1.isEmpty

    @test
    def take03(): Bool =
        List.range(0, 1000) |> List.toStream |> Stream1.take(0) |> Stream1.isEmpty

    @test
    def take04(): Bool =
        List.range(0, 1000)
            |> List.toStream
            |> Stream1.take(500)
            |> Stream1.toList == List.range(0, 500)

    @test
    def take05(): Bool =
        List.range(0, 1000)
            |> List.toStream
            |> Stream1.take(1)
            |> Stream1.toList == List.range(0, 1)


    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def exists01(): Bool =
        not (Stream1.exists(i -> i > 3, Stream1.empty()))

    @test
    def exists02(): Bool =
        not ((1 :: Nil) |> List.toStream |> Stream1.exists(i -> i > 3))

    @test
    def exists03(): Bool =
        (5 :: Nil) |> List.toStream |> Stream1.exists(i -> i > 3)

    @test
    def exists04(): Bool = 
        not ((1 :: 2 :: Nil) |> List.toStream |> Stream1.exists(i -> i > 3))

    @test
    def exists05(): Bool =
        (1 :: 6 :: Nil) |> List.toStream |> Stream1.exists(i -> i > 3)

    @test
    def exists06(): Bool =
        (6 :: 1 :: Nil) |> List.toStream |> Stream1.exists(i -> i > 3)

    @test
    def exists07(): Bool =
        (16 :: 6 :: Nil) |> List.toStream |> Stream1.exists(i -> i > 3)

    @test
    def exists08(): Bool =
        not ((1 :: -9 :: 3 :: Nil) |> List.toStream |> Stream1.exists(i -> i > 3))

    @test
    def exists09(): Bool =
        (1 :: 9 :: 3 :: Nil) |> List.toStream |> Stream1.exists(i -> i > 3)


    /////////////////////////////////////////////////////////////////////////////
    // forall                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forall01(): Bool =
        Stream1.forall(i -> i > 3, Stream1.empty())

    @test
    def forall02(): Bool =
        not ((1 :: Nil) |> List.toStream |> Stream1.forall(i -> i > 3))

    @test
    def forall03(): Bool =
        (5 :: Nil) |> List.toStream |> Stream1.forall(i -> i > 3)

    @test
    def forall04(): Bool =
        not ((1 :: 2 :: Nil) |> List.toStream |> Stream1.forall(i -> i > 3))

    @test
    def forall05(): Bool =
        not ((1 :: 6 :: Nil) |> List.toStream |> Stream1.forall(i -> i > 3))

    @test
    def forall06(): Bool =
        not ((6 :: 1 :: Nil) |> List.toStream |> Stream1.forall(i -> i > 3))

    @test
    def forall07(): Bool =
        (16 :: 6 :: Nil) |> List.toStream |> Stream1.forall(i -> i > 3)

    @test
    def forall08(): Bool =
        not ((1 :: -9 :: 3 :: Nil) |> List.toStream |> Stream1.forall(i -> i > 3))

    @test
    def forall09(): Bool =
        not ((1 :: 9 :: 3 :: Nil) |> List.toStream |> Stream1.forall(i -> i > 3))

    @test
    def forall10(): Bool =
        (11 :: 9 :: 31 :: Nil) |> List.toStream |> Stream1.forall(i -> i > 3)


    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def append01(): Bool =
        Stream1.append(Stream1.empty(), Stream1.empty()) |> Stream1.toList == Nil

    @test
    def append02(): Bool =
        Stream1.append(
            Stream1.empty(),
            List.toStream(1 :: Nil)) |> Stream1.toList == 1 :: Nil

    @test
    def append03(): Bool =
        Stream1.append(
            Stream1.empty(),
            List.toStream(1 :: 2 :: Nil)) |> Stream1.toList == 1 :: 2 :: Nil

    @test
    def append04(): Bool =
        Stream1.append(
            List.toStream(1 :: Nil),
            Stream1.empty()) |> Stream1.toList == 1 :: Nil

    @test
    def append05(): Bool =
        Stream1.append(
            List.toStream(1 :: 2 :: Nil),
            Stream1.empty()) |> Stream1.toList == 1 :: 2 :: Nil

    @test
    def append06(): Bool =
        Stream1.append(
            List.toStream(1 :: Nil),
            List.toStream(2 :: Nil)) |> Stream1.toList == 1 :: 2 :: Nil

    @test
    def append07(): Bool =
        Stream1.append(
            List.toStream(1 :: 2 :: Nil),
            List.toStream(3 :: Nil)) |> Stream1.toList == 1 :: 2 :: 3 :: Nil

    @test
    def append08(): Bool =
        Stream1.append(
            List.toStream(1 :: Nil),
            List.toStream(2 :: 3 :: Nil)) |> Stream1.toList == 1 :: 2 :: 3 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////
/*
    @test
    def zip01(): Bool =
        Stream1.zip(
            Stream1.empty(): Stream[Unit, Unit],
            Stream1.empty(): Stream[Unit, Unit]) |> Stream1.toList == Nil

    @test
    def zip02(): Bool =
        Stream1.zip(
            List.toStream(1 :: Nil),
            List.toStream(Nil: List[Unit])) |> Stream1.toList == Nil

    @test
    def zip03(): Bool =
        Stream1.zip(
            List.toStream(Nil: List[Unit]),
            List.toStream(2 :: Nil)) |> Stream1.toList == Nil

    @test
    def zip04(): Bool =
        Stream1.zip(
            List.toStream(1 :: Nil),
            List.toStream(2 :: Nil)) |> Stream1.toList == (1, 2) :: Nil

    @test
    def zip05(): Bool =
        Stream1.zip(
            List.toStream(1 :: 3 :: Nil),
            List.toStream(2 :: 4 :: Nil)) |> Stream1.toList == (1, 2) :: (3, 4) :: Nil

    @test
    def zip06(): Bool = 
        Stream1.zip(
            List.toStream(1 :: 3 :: 5 :: Nil),
            List.toStream(2 :: 4 :: 6 :: Nil)) |> Stream1.toList == (1, 2) :: (3, 4) :: (5, 6) :: Nil

    @test
    def zip07(): Bool =
        Stream1.zip(
            List.toStream(1 :: 3 :: 5 :: 7 :: Nil),
            List.toStream(2 :: 4 :: 6 :: 8 :: Nil)) |> Stream1.toList == (1, 2) :: (3, 4) :: (5, 6) :: (7, 8) :: Nil
            */
/*

    /////////////////////////////////////////////////////////////////////////////
    // flatMap (pure)                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatMapPure01(): Bool =
        Stream1.flatMap(i -> Stream1.range(i, i + 100), Stream1.empty()) |> Stream.toList == Nil

    @test
    def flatMapPure02(): Bool =
        List.toStream(0 :: Nil) |> Stream1.flatMap(i -> Stream1.range(i, i + 100)) |> Stream.toList == List.range(0, 100)

    @test
    def flatMapPure03(): Bool =
        List.toStream(1 :: Nil) |> Stream1.flatMap(i -> Stream1.range(i, i + 2)) |> Stream.toList == 1 :: 2 :: Nil

    @test
    def flatMapPure04(): Bool =
        List.toStream(2 :: Nil) |> Stream1.flatMap(i -> Stream1.range(i, i + 2)) |> Stream.toList == 2 :: 3 :: Nil

    @test
    def flatMapPure05(): Bool =
        List.toStream(1 :: 2 :: Nil) |> Stream1.flatMap(i -> Stream1.range(i, i + 2)) |> Stream.toList == 1 :: 2 :: 2 :: 3 :: Nil

    @test
    def flatMapPure06(): Bool =
        List.toStream(2 :: 3 :: Nil) |> Stream1.flatMap(i -> Stream1.range(i, i + 2)) |> Stream.toList == 2 :: 3 :: 3 :: 4 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // flatMap (impure)                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatMapImpure01(): Bool & Impure =
        Stream1.flatMap(i -> Stream1.range(i, i + 100) as & Impure, Stream1.empty()) |> Stream.toList == Nil

    @test
    def flatMapImpure02(): Bool & Impure =
        List.toStream(0 :: Nil) |> Stream1.flatMap(i -> Stream1.range(i, i + 100) as & Impure) |> Stream.toList == List.range(0, 100)

    @test
    def flatMapImpure03(): Bool & Impure =
        List.toStream(1 :: Nil) |> Stream1.flatMap(i -> Stream1.range(i, i + 2) as & Impure) |> Stream.toList == 1 :: 2 :: Nil

    @test
    def flatMapImpure04(): Bool & Impure =
        List.toStream(2 :: Nil) |> Stream1.flatMap(i -> Stream1.range(i, i + 2) as & Impure) |> Stream.toList == 2 :: 3 :: Nil

    @test
    def flatMapImpure05(): Bool & Impure =
        List.toStream(1 :: 2 :: Nil) |> Stream1.flatMap(i -> Stream1.range(i, i + 2) as & Impure) |> Stream.toList == 1 :: 2 :: 2 :: 3 :: Nil

    @test
    def flatMapImpure06(): Bool & Impure =
        List.toStream(2 :: 3 :: Nil) |> Stream1.flatMap(i -> Stream1.range(i, i + 2) as & Impure) |> Stream.toList == 2 :: 3 :: 3 :: 4 :: Nil
*/
}