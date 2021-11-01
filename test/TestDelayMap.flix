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


namespace TestDelayMap {

    /////////////////////////////////////////////////////////////////////////////
    // insertWith                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def insertWith01(): Bool =
        DelayMap1.insertWith((v1, v2) -> v1 + v2, 1, 3, DelayMap1.empty()) |> DelayMap1.toList == (1, 3) :: Nil

    @test
    def insertWith02(): Bool =
        List1.toDelayMap((1, 4) :: Nil) |> DelayMap1.insertWith((v1, v2) -> v1 + v2, 1, 3) |> DelayMap1.toList == (1, 7) :: Nil

    @test
    def insertWith03(): Bool =
        List1.toDelayMap((1, 4) :: Nil) |> DelayMap1.insertWith((v1, v2) -> v1 + v2, 2, 3) |> DelayMap1.toList == (1, 4) :: (2, 3) :: Nil

    @test
    def insertWith04(): Bool =
        List1.toDelayMap((1, 4) :: (5, -2) :: Nil) |> DelayMap1.insertWith((v1, v2) -> v1 + v2, 1, 1) |> DelayMap1.toList == (1, 5) :: (5, -2) :: Nil

    @test
    def insertWith05(): Bool =
        List1.toDelayMap((1, 4) :: (5, -2) :: Nil) |> DelayMap1.insertWith((v1, v2) -> v1 + v2, 5, 1) |> DelayMap1.toList == (1, 4) :: (5, -1) :: Nil

    @test
    def insertWith06(): Bool =
        List1.toDelayMap((1, 4) :: (5, -2) :: Nil) |> DelayMap1.insertWith((v1, v2) -> v1 + v2, 4, -2) |> DelayMap1.toList |> List.sortBy(t -> fst(t)) == (1, 4) :: (4, -2) :: (5, -2):: Nil


    /////////////////////////////////////////////////////////////////////////////
    // insertWithKey insertWithKey                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def insertWithInsertWith01(): Bool =
        (("a", 1) :: Nil)                                    |>
            List1.toDelayMap                                 |>
            DelayMap1.insertWith((v1, v2) -> v1 + v2, "b", 1) |>
            DelayMap1.insertWith((v1, v2) -> v1 + v2, "b", 3) |>
            DelayMap1.toList == ("a", 1) :: ("b", 4) :: Nil

    @test
    def insertWithInsertWith02(): Bool & Impure =
        (("a", 1) :: Nil)                                                |>
            List1.toDelayMap                                             |>
            DelayMap1.insertWith((v1, v2) -> v1 + v2, "b" as & Impure, 1) |>
            DelayMap1.insertWith((v1, v2) -> v1 + v2, "b", 3)             |>
            DelayMap1.toList == ("a", 1) :: ("b", 4) :: Nil
    @test
    def insertWithInsertWith03(): Bool & Impure =
        (("a", 1) :: Nil)                                                |>
            List1.toDelayMap                                             |>
            DelayMap1.insertWith((v1, v2) -> v1 + v2, "b", 1)             |>
            DelayMap1.insertWith((v1, v2) -> v1 + v2, "b" as & Impure, 3) |>
            DelayMap1.toList == ("a", 1) :: ("b", 4) :: Nil

    @test
    def insertWithInsertWith04(): Bool & Impure =
        (("a", 1) :: Nil)                                                |>
            List1.toDelayMap                                             |>
            DelayMap1.insertWith((v1, v2) -> v1 + v2, "b" as & Impure, 1) |>
            DelayMap1.insertWith((v1, v2) -> v1 + v2, "b" as & Impure, 3) |>
            DelayMap1.toList == ("a", 1) :: ("b", 4) :: Nil



    /////////////////////////////////////////////////////////////////////////////
    // insertWith insertWith fusion                                            //
    /////////////////////////////////////////////////////////////////////////////
/*
    @test
    def mapFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |>
        LazyList.map(x -> { l := "a" :: deref l; x }) |>
        LazyList.map(x -> { l := "b" :: deref l; x });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def mapFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |>
        LazyList.map(x -> { l := "a" :: deref l; x } as & Pure) |>
        LazyList.map(x -> { l := "b" :: deref l; x } as & Pure) |>
        LazyList.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)

*/
}
