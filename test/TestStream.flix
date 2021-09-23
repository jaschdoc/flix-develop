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

}