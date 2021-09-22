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

}