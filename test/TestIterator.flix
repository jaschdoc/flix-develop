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

namespace TestIterator {

    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def range01(): Bool & Impure =
        Iterator1.range(0, 0) |> Iterator1.toList == Nil

    @test
    def range02(): Bool & Impure =
        Iterator1.range(10, 0) |> Iterator1.toList == Nil

    @test
    def range03(): Bool & Impure =
        Iterator1.range(1, 4) |> Iterator1.toList == 1 :: 2 :: 3 :: Nil

    @test
    def range04(): Bool & Impure =
        Iterator1.range(-1, 2) |> Iterator1.toList == -1 :: 0 :: 1 :: Nil

    @test
    def range05(): Bool & Impure =
        Iterator1.range(-100, 200) |> Iterator1.toList == List.range(-100, 200)


    /////////////////////////////////////////////////////////////////////////////
    // repeat                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def repeat01(): Bool & Impure =
        Iterator1.repeat(0, 10) |> Iterator1.toList == Nil

    @test
    def repeat02(): Bool & Impure =
        Iterator1.repeat(-100, 2) |> Iterator1.toList == Nil

    @test
    def repeat03(): Bool & Impure =
        Iterator1.repeat(100, 2) |> Iterator1.toList == List.repeat(100, 2)

    @test
    def repeat04(): Bool & Impure =
        Iterator1.repeat(3, 1) |> Iterator1.toList == 1 :: 1 :: 1 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toArray01(): Bool & Impure =
        (Nil: List[Int32] |> List1.toIterator |> Iterator1.toArray) == []

    @test
    def toArray02(): Bool & Impure =
        Iterator1.range(-10, -5) |> Iterator1.toArray == [-10, -9, -8, -7, -6]

    @test
    def toArray03(): Bool & Impure =
        Iterator1.repeat(3, 1) |> Iterator1.toArray == [1, 1, 1]

    @test
    def toArray04(): Bool & Impure =
        Iterator1.range(-100, 200) |> Iterator1.toArray == Array.range(-100, 200)


    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMap01(): Bool & Impure =
        Nil: List[(String, Int32)] |> List1.toIterator |> Iterator1.toMap == Map.empty()

    @test
    def toMap02(): Bool & Impure =
        (("A", 1) :: ("B", 2) :: ("C", 3) :: ("D", 4) :: Nil)|>
        List1.toIterator |>
        Iterator1.toMap == Map#{"A" -> 1, "B" -> 2, "C" -> 3, "D" -> 4}

    @test
    def toMap03(): Bool & Impure =
        let map = Map#{"1" -> 2, "2" -> 3, "3" -> 4};
        map |> Map.toList |> List1.toIterator |> Iterator1.toMap == map


    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toSet01(): Bool & Impure =
        List1.toIterator(Nil: List[Int32]) |> Iterator1.toSet == Set.empty()

    @test
    def toSet02(): Bool & Impure =
        Set#{} |> Set.toList |> List1.toIterator |> Iterator1.toSet == Set.empty()

    @test
    def toSet03(): Bool & Impure =
        Set#{"a", "b", "c", "d"} |> Set.toList |> List1.toIterator |> Iterator1.toSet == Set#{"a", "b", "c", "d"}

    @test
    def toSet04(): Bool & Impure =
        Set.range(0, 10000) |> Set.toList |> List1.toIterator |> Iterator1.toSet == Set.range(0, 10000)


    /////////////////////////////////////////////////////////////////////////////
    // toNel                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toNel01(): Bool & Impure =
        Nil |> List1.toIterator |> Iterator1.toNel == None

    @test
    def toNel02(): Bool & Impure =
        (1 :: Nil) |> List1.toIterator |> Iterator1.toNel == Some(Nel(1, Nil))

    @test
    def toNel03(): Bool & Impure =
        Iterator1.range(0, 1000) |> Iterator1.toNel == Some(Nel(0, List.range(1, 1000)))


    /////////////////////////////////////////////////////////////////////////////
    // filter (pure)                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterPure01(): Bool & Impure =
        Nil |> List.toIterator |> Iterator1.filter(i -> i > 3) == Nil

    @test
    def filterPure02(): Bool & Impure =
        (2 :: Nil) |> List.toIterator |> Iterator1.filter(i -> i > 3) == Nil

    @test
    def filterPure03(): Bool & Impure =
        (4 :: Nil) |> List.toIterator |> Iterator1.filter(i -> i > 3) == 4 :: Nil

    @test
    def filterPure04(): Bool & Impure =
        (1 :: 3 :: Nil) |> List.toIterator |> Iterator1.filter(i -> i > 3) == Nil

    @test
    def filterPure05(): Bool & Impure =
        (1 :: 8 :: Nil) |> List.toIterator |> Iterator1.filter(i -> i > 3) == 8 :: Nil

    @test
    def filterPure06(): Bool & Impure =
        (8 :: 1 :: Nil) |> List.toIterator |> Iterator1.filter(i -> i > 3) == 8 :: Nil

    @test
    def filterPure07(): Bool & Impure =
        (8 :: 9 :: Nil) |> List.toIterator |> Iterator1.filter(i -> i > 3) == 8 :: 9 :: Nil

    @test
    def filterPure08(): Bool & Impure =
        (1 :: 4 :: 11 :: 2 :: -22 :: 17 :: Nil) |> List.toIterator |> Iterator1.filter(i -> i > 3) == 4 :: 11 :: 17 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // filter (impure)                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterImpure01(): Bool & Impure =
        Nil |> List.toIterator |> Iterator1.filter(i -> i > 3 as & Impure) == Nil

    @test
    def filterImpure02(): Bool & Impure =
        (2 :: Nil) |> List.toIterator |> Iterator1.filter(i -> i > 3 as & Impure) == Nil

    @test
    def filterImpure03(): Bool & Impure =
        (4 :: Nil) |> List.toIterator |> Iterator1.filter(i -> i > 3 as & Impure) == 4 :: Nil

    @test
    def filterImpure04(): Bool & Impure =
        (1 :: 3 :: Nil) |> List.toIterator |> Iterator1.filter(i -> i > 3 as & Impure) == Nil

    @test
    def filterImpure05(): Bool & Impure =
        (1 :: 8 :: Nil) |> List.toIterator |> Iterator1.filter(i -> i > 3 as & Impure) == 8 :: Nil

    @test
    def filterImpure06(): Bool & Impure =
        (8 :: 1 :: Nil) |> List.toIterator |> Iterator1.filter(i -> i > 3 as & Impure) == 8 :: Nil

    @test
    def filterImpure07(): Bool & Impure =
        (8 :: 9 :: Nil) |> List.toIterator |> Iterator1.filter(i -> i > 3 as & Impure) == 8 :: 9 :: Nil

    @test
    def filterImpure08(): Bool & Impure =
        (1 :: 4 :: 11 :: 2 :: -22 :: 17 :: Nil) |> List.toIterator |> Iterator1.filter(i -> i > 3 as & Impure) == 4 :: 11 :: 17 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // map (pure)                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapPure01(): Bool & Impure =
        Nil |> List1.toIterator |>
            Iterator1.map(i -> i > 2) == Nil

    @test
    def mapPure02(): Bool & Impure =
        (1 :: Nil) |> List1.toIterator |>
            Iterator1.map(i -> i > 2) == false :: Nil

    @test
    def mapPure03(): Bool & Impure =
        (3 :: Nil) |> List1.toIterator |>
            Iterator1.map(i -> i > 2) == true :: Nil

    @test
    def mapPure04(): Bool & Impure =
        (1 :: 2 :: Nil) |> List1.toIterator |>
            Iterator1.map(i -> i > 2) == false :: false :: Nil

    @test
    def mapPure05(): Bool & Impure =
        (1 :: 8 :: Nil) |> List1.toIterator |>
            Iterator1.map(i -> i > 2) == false :: true :: Nil

    @test
    def mapPure06(): Bool & Impure =
        (8 :: 1 :: Nil) |> List1.toIterator |>
            Iterator1.map(i -> i > 2) == true :: false :: Nil

    @test
    def mapPure07(): Bool & Impure =
        (7 :: 8 :: Nil) |> List1.toIterator |>
            Iterator1.map(i -> i > 2) == true :: true :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // map (impure)                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapImpure01(): Bool & Impure =
        Nil |> List1.toIterator |>
            Iterator1.map(i -> i > 2 as & Impure) == Nil

    @test
    def mapImpure02(): Bool & Impure =
        (1 :: Nil) |> List1.toIterator |>
            Iterator1.map(i -> i > 2 as & Impure) == false :: Nil

    @test
    def mapImpure03(): Bool & Impure =
        (3 :: Nil) |> List1.toIterator |>
            Iterator1.map(i -> i > 2 as & Impure) == true :: Nil

    @test
    def mapImpure04(): Bool & Impure =
        (1 :: 2 :: Nil) |> List1.toIterator |>
            Iterator1.map(i -> i > 2 as & Impure) == false :: false :: Nil

    @test
    def mapImpure05(): Bool & Impure =
        (1 :: 8 :: Nil) |> List1.toIterator |>
            Iterator1.map(i -> i > 2 as & Impure) == false :: true :: Nil

    @test
    def mapImpure06(): Bool & Impure =
        (8 :: 1 :: Nil) |> List1.toIterator |>
            Iterator1.map(i -> i > 2 as & Impure) == true :: false :: Nil

    @test
    def mapImpure07(): Bool & Impure =
        (7 :: 8 :: Nil) |> List1.toIterator |>
            Iterator1.map(i -> i > 2 as & Impure) == true :: true :: Nil


}
