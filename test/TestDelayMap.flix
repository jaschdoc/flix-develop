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
        DelayMap1.insertWith((v1, v2) -> v1 + v2, 1, 3, DelayMap1.empty()) |>DelayMap1.toList == (1, 3) :: Nil

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
        List1.toDelayMap((1, 4) :: (5, -2) :: Nil) |>
        DelayMap1.insertWith((v1, v2) -> v1 + v2, 5, 1) |> DelayMap1.toList == (1, 4) :: (5, -1) :: Nil

    @test
    def insertWith06(): Bool =
        List1.toDelayMap((1, 4) :: (5, -2) :: Nil) |>
        DelayMap1.insertWith((v1, v2) -> v1 + v2, 4, -2) |>
        DelayMap1.toList |> List.sortBy(t -> fst(t)) == (1, 4) :: (4, -2) :: (5, -2):: Nil


    /////////////////////////////////////////////////////////////////////////////
    // insertWithKey insertWithKey                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def insertWithInsertWith01(): Bool =
        (("a", 1) :: Nil)                                    |>
            List1.toDelayMap                                  |>
            DelayMap1.insertWith((v1, v2) -> v1 + v2, "b", 1) |>
            DelayMap1.insertWith((v1, v2) -> v1 + v2, "b", 3) |>
            DelayMap1.toList == ("a", 1) :: ("b", 4) :: Nil

    @test
    def insertWithInsertWith02(): Bool & Impure =
        (("a", 1) :: Nil)                                                |>
            List1.toDelayMap                                              |>
            DelayMap1.insertWith((v1, v2) -> v1 + v2, "b" as & Impure, 1) |>
            DelayMap1.insertWith((v1, v2) -> v1 + v2, "b", 3)             |>
            DelayMap1.toList == ("a", 1) :: ("b", 4) :: Nil
    @test
    def insertWithInsertWith03(): Bool & Impure =
        (("a", 1) :: Nil)                                                |>
            List1.toDelayMap                                              |>
            DelayMap1.insertWith((v1, v2) -> v1 + v2, "b", 1)             |>
            DelayMap1.insertWith((v1, v2) -> v1 + v2, "b" as & Impure, 3) |>
            DelayMap1.toList == ("a", 1) :: ("b", 4) :: Nil

    @test
    def insertWithInsertWith04(): Bool & Impure =
        (("a", 1) :: Nil)                                                |>
            List1.toDelayMap                                              |>
            DelayMap1.insertWith((v1, v2) -> v1 + v2, "b" as & Impure, 1) |>
            DelayMap1.insertWith((v1, v2) -> v1 + v2, "b" as & Impure, 3) |>
            DelayMap1.toList == ("a", 1) :: ("b", 4) :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // insertWith insertWith fusion                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def insertWithFusion01(): Bool & Impure =
        let l = ref Nil;
        List1.toDelayMap((1, 1) :: Nil) |>
        DelayMap1.insertWith((v, _) -> { l := "a" :: deref l; v }, 1, 1) |>
        DelayMap1.insertWith((v, _) -> { l := "a" :: deref l; v }, 1, 1) |>
        DelayMap1.insertWith((v, _) -> { l := "a" :: deref l; v }, 1, 1) |>
        DelayMap1.insertWith((v, _) -> { l := "b" :: deref l; v }, 1, 1) |>
        DelayMap1.insertWith((v, _) -> { l := "b" :: deref l; v }, 1, 1) |>
        DelayMap1.insertWith((v, _) -> { l := "b" :: deref l; v }, 1, 1);
        List.reverse(deref l) == "a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil

    @test
    def insertWithFusion02(): Bool & Impure =
        let l = ref Nil;
        let m = List1.toDelayMap((1, 1) :: Nil) |>
        DelayMap1.insertWith((v, _) -> { l := "a" :: deref l; v } as & Pure, 1, 1) |>
        DelayMap1.insertWith((v, _) -> { l := "a" :: deref l; v } as & Pure, 1, 1) |>
        DelayMap1.insertWith((v, _) -> { l := "a" :: deref l; v } as & Pure, 1, 1) |>
        DelayMap1.insertWith((v, _) -> { l := "b" :: deref l; v } as & Pure, 1, 1) |>
        DelayMap1.insertWith((v, _) -> { l := "b" :: deref l; v } as & Pure, 1, 1) |>
        DelayMap1.insertWith((v, _) -> { l := "b" :: deref l; v } as & Pure, 1, 1);
        let _ = DelayMap1.toMap(m);
        List.reverse(deref l) == "a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // map (pure)                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapPure01(): Bool =
        Map#{}: Map[Unit, Int32] |> Map1.toDelayMap |> DelayMap1.map(v -> 3 * v) |> DelayMap1.toMap == Map#{}

    @test
    def mapPure02(): Bool =
        Map#{1 -> 4} |> Map1.toDelayMap |> DelayMap1.map(v -> 3 * v) |> DelayMap1.toMap == Map#{1 -> 12}

    @test
    def mapPure03(): Bool =
        Map#{2 -> -1, 0 -> 0} |> Map1.toDelayMap |>
            DelayMap1.map(v -> 3 * v) |> DelayMap1.toMap == Map#{2 -> -3, 0 -> 0}

    @test
    def mapPure04(): Bool =
        Map#{2 -> -1, 5 -> 15, 11 -> -9} |> Map1.toDelayMap |>
            DelayMap1.map(v -> 3 * v) |> DelayMap1.toMap == Map#{2 -> -3, 5 -> 45, 11 -> -27}

    @test
    def mapPure05(): Bool =
        Map#{2 -> -1, 5 -> 15, 11 -> -9, 8 -> 8} |> Map1.toDelayMap |>
            DelayMap1.map(v -> 3 * v) |> DelayMap1.toMap == Map#{2 -> -3, 5 -> 45, 11 -> -27, 8 -> 24}


    /////////////////////////////////////////////////////////////////////////////
    // map (impure)                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapImpure01(): Bool & Impure =
        Map#{}: Map[Unit, Int32] |> Map1.toDelayMap |>
            DelayMap1.map(v -> 3 * v as & Impure) |> DelayMap1.toMap == Map#{}

    @test
    def mapImpure02(): Bool & Impure =
        Map#{1 -> 4} |> Map1.toDelayMap |>
            DelayMap1.map(v -> 3 * v as & Impure) |> DelayMap1.toMap == Map#{1 -> 12}

    @test
    def mapImpure03(): Bool & Impure =
        Map#{2 -> -1, 0 -> 0} |> Map1.toDelayMap |>
            DelayMap1.map(v -> 3 * v as & Impure) |> DelayMap1.toMap == Map#{2 -> -3, 0 -> 0}

    @test
    def mapImpure04(): Bool & Impure =
        Map#{2 -> -1, 5 -> 15, 11 -> -9} |> Map1.toDelayMap |>
            DelayMap1.map(v -> 3 * v as & Impure) |> DelayMap1.toMap == Map#{2 -> -3, 5 -> 45, 11 -> -27}

    @test
    def mapImpure05(): Bool & Impure =
        Map#{2 -> -1, 5 -> 15, 11 -> -9, 8 -> 8} |> Map1.toDelayMap |>
            DelayMap1.map(v -> 3 * v as & Impure) |> DelayMap1.toMap == Map#{2 -> -3, 5 -> 45, 11 -> -27, 8 -> 24}


    /////////////////////////////////////////////////////////////////////////////
    // map map                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapMap01(): Bool =
        Map#{2 -> -1, 5 -> 15, 11 -> -9} |>
            Map1.toDelayMap |>
            DelayMap1.map(v -> 3 * v) |>
            DelayMap1.map(v -> 3 * v) |>
            DelayMap1.toMap == Map#{2 -> -9, 5 -> 135, 11 -> -81}

    @test
    def mapMap02(): Bool & Impure =
        Map#{2 -> -1, 5 -> 15, 11 -> -9} |>
            Map1.toDelayMap |>
            DelayMap1.map(v -> 3 * v as & Impure) |>
            DelayMap1.map(v -> 3 * v) |>
            DelayMap1.toMap == Map#{2 -> -9, 5 -> 135, 11 -> -81}

    @test
    def mapMap03(): Bool & Impure =
        Map#{2 -> -1, 5 -> 15, 11 -> -9} |>
            Map1.toDelayMap |>
            DelayMap1.map(v -> 3 * v) |>
            DelayMap1.map(v -> 3 * v as & Impure) |>
            DelayMap1.toMap == Map#{2 -> -9, 5 -> 135, 11 -> -81}

    @test
    def mapMap04(): Bool & Impure =
        Map#{2 -> -1, 5 -> 15, 11 -> -9} |>
            Map1.toDelayMap |>
            DelayMap1.map(v -> 3 * v as & Impure) |>
            DelayMap1.map(v -> 3 * v as & Impure) |>
            DelayMap1.toMap == Map#{2 -> -9, 5 -> 135, 11 -> -81}


    /////////////////////////////////////////////////////////////////////////////
    // map (fusion)                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapFusion01(): Bool & Impure =
        let l = ref Nil;
        ((1, 1) :: (2, 2) :: (3, 3) :: Nil) |> List1.toDelayMap |>
        DelayMap1.map(v -> { l := "a" :: deref l; v }) |>
        DelayMap1.map(v -> { l := "b" :: deref l; v });
        List.reverse(deref l) == "a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil

    @test
    def mapFusion02(): Bool & Impure =
        let l = ref Nil;
        let m = ((1, 1) :: (2, 2) :: (3, 3) :: Nil) |> List1.toDelayMap |>
        DelayMap1.map(v -> { l := "a" :: deref l; v } as & Pure) |>
        DelayMap1.map(v -> { l := "b" :: deref l; v } as & Pure);
        let _ = DelayMap1.toMap(m);
        List.reverse(deref l) == "a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // mapWithKey                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithKey01(): Bool =
        Map#{}: Map[Unit, Unit] |> Map1.toDelayMap |>
            DelayMap1.mapWithKey((_, v) -> v) |> DelayMap1.toMap == Map#{}

    @test
    def mapWithKey02(): Bool =
        Map#{1 -> 4} |> Map1.toDelayMap |>
            DelayMap1.mapWithKey((k, v) -> k + v) |> DelayMap1.toMap == Map#{1 -> 5}

    @test
    def mapWithKey03(): Bool =
        Map#{2 -> -1, 0 -> 0} |> Map1.toDelayMap |>
            DelayMap1.mapWithKey((k, v) -> k + v) |> DelayMap1.toMap == Map#{2 -> 1, 0 -> 0}

    @test
    def mapWithKey04(): Bool =
        Map#{2 -> -1, 5 -> 15, 11 -> -9} |> Map1.toDelayMap |>
            DelayMap1.mapWithKey((k, v) -> k + v) |> DelayMap1.toMap == Map#{2 -> 1, 5 -> 20, 11 -> 2}

    @test
    def mapWithKey05(): Bool =
        Map#{2 -> -1, 5 -> 15, 11 -> -9, 8 -> 8} |> Map1.toDelayMap |>
            DelayMap1.mapWithKey((k, v) -> k + v) |> DelayMap1.toMap == Map#{2 -> 1, 5 -> 20, 11 -> 2, 8 -> 16}


    /////////////////////////////////////////////////////////////////////////////
    // union                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def union01(): Bool =
        let m1 = Map1.toDelayMap(Map#{}: Map[Unit, Unit]);
        let m2 = Map1.toDelayMap(Map#{});
        DelayMap1.union(m1, m2) |> DelayMap1.toMap == Map#{}

    @test
    def union02(): Bool =
        let m1 = Map1.toDelayMap(Map#{1 -> 2});
        let m2 = Map1.toDelayMap(Map#{});
        DelayMap1.union(m1, m2) |> DelayMap1.toMap == Map#{1 -> 2}

    @test
    def union03(): Bool =
        let m1 = Map1.toDelayMap(Map#{});
        let m2 = Map1.toDelayMap(Map#{1 -> 2});
        DelayMap1.union(m1, m2) |> DelayMap1.toMap == Map#{1 -> 2}

    @test
    def union04(): Bool =
        let m1 = Map1.toDelayMap(Map#{});
        let m2 = Map1.toDelayMap(Map#{1 -> 2, 3 -> 4});
        DelayMap1.union(m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 3 -> 4}

    @test
    def union05(): Bool =
        let m1 = Map1.toDelayMap(Map#{1 -> 2, 3 -> 4});
        let m2 = Map1.toDelayMap(Map#{});
        DelayMap1.union(m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 3 -> 4}

    @test
    def union06(): Bool =
        let m1 = Map1.toDelayMap(Map#{1 -> 2});
        let m2 = Map1.toDelayMap(Map#{3 -> 4});
        DelayMap1.union(m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 3 -> 4}

    @test
    def union07(): Bool =
        let m1 = Map1.toDelayMap(Map#{1 -> 2});
        let m2 = Map1.toDelayMap(Map#{1 -> 5});
        DelayMap1.union(m1, m2) |> DelayMap1.toMap == Map#{1 -> 2}

    @test
    def union08(): Bool =
        let m1 = Map1.toDelayMap(Map#{});
        let m2 = Map1.toDelayMap(Map#{1 -> 2, 2 -> 3, 3 -> 4});
        DelayMap1.union(m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def union09(): Bool =
        let m1 = Map1.toDelayMap(Map#{1 -> 2, 2 -> 3, 3 -> 4});
        let m2 = Map1.toDelayMap(Map#{});
        DelayMap1.union(m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def union10(): Bool =
        let m1 = Map1.toDelayMap(Map#{1 -> 2, 2 -> 3});
        let m2 = Map1.toDelayMap(Map#{3 -> 4});
        DelayMap1.union(m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def union11(): Bool =
        let m1 = Map1.toDelayMap(Map#{3 -> 4});
        let m2 = Map1.toDelayMap(Map#{1 -> 2, 2 -> 3});
        DelayMap1.union(m1, m2) |> DelayMap1.toMap== Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def union12(): Bool =
        let m1 = Map1.toDelayMap(Map#{1 -> 2, 2 -> 3});
        let m2 = Map1.toDelayMap(Map#{2 -> 8});
        DelayMap1.union(m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 3}

    @test
    def union13(): Bool =
        let m1 = Map1.toDelayMap(Map#{1 -> 2, 2 -> 3});
        let m2 = Map1.toDelayMap(Map#{1 -> 8});
        DelayMap1.union(m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 3}

    @test
    def union14(): Bool =
        let m1 = Map1.toDelayMap(Map#{1 -> 4});
        let m2 = Map1.toDelayMap(Map#{1 -> 2, 2 -> 3});
        DelayMap1.union(m1, m2) |> DelayMap1.toMap == Map#{1 -> 4, 2 -> 3}

    @test
    def union15(): Bool =
        let m1 = Map1.toDelayMap(Map#{2 -> 7});
        let m2 = Map1.toDelayMap(Map#{1 -> 2, 2 -> 3});
        DelayMap1.union(m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 7}

    @test
    def union16(): Bool =
        let m1 = Map1.toDelayMap(Map#{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22});
        let m2 = Map1.toDelayMap(Map#{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33});
        DelayMap1.union(m1, m2) |> DelayMap1.toMap == Map#{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22, 1 -> 2, 44 -> 33}


    /////////////////////////////////////////////////////////////////////////////
    // unionWith                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unionWith01(): Bool =
        let m1 = Map1.toDelayMap(Map#{});
        let m2 = Map1.toDelayMap(Map#{});
        DelayMap1.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap1.toMap == Map#{}: Map[Int, Int]

    @test
    def unionWith02(): Bool =
        let m1 = Map1.toDelayMap(Map#{1 -> 2});
        let m2 = Map1.toDelayMap(Map#{});
        DelayMap1.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap1.toMap == Map#{1 -> 2}

    @test
    def unionWith03(): Bool =
        let m1 = Map1.toDelayMap(Map#{});
        let m2 = Map1.toDelayMap(Map#{1 -> 2});
        DelayMap1.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap1.toMap == Map#{1 -> 2}

    @test
    def unionWith04(): Bool =
        let m1 = Map1.toDelayMap(Map#{});
        let m2 = Map1.toDelayMap(Map#{1 -> 2, 3 -> 4});
        DelayMap1.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 3 -> 4}

    @test
    def unionWith05(): Bool =
        let m1 = Map1.toDelayMap(Map#{1 -> 2, 3 -> 4});
        let m2 = Map1.toDelayMap(Map#{});
        DelayMap1.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 3 -> 4}

    @test
    def unionWith06(): Bool =
        let m1 = Map1.toDelayMap(Map#{1 -> 2});
        let m2 = Map1.toDelayMap(Map#{3 -> 4});
        DelayMap1.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 3 -> 4}

    @test
    def unionWith07(): Bool =
        let m1 = Map1.toDelayMap(Map#{1 -> 2});
        let m2 = Map1.toDelayMap(Map#{1 -> 5});
        DelayMap1.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap1.toMap == Map#{1 -> -3}

    @test
    def unionWith08(): Bool =
        let m1 = Map1.toDelayMap(Map#{});
        let m2 = Map1.toDelayMap(Map#{1 -> 2, 2 -> 3, 3 -> 4});
        DelayMap1.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def unionWith09(): Bool =
        let m1 = Map1.toDelayMap(Map#{1 -> 2, 2 -> 3, 3 -> 4});
        let m2 = Map1.toDelayMap(Map#{});
        DelayMap1.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def unionWith10(): Bool =
        let m1 = Map1.toDelayMap(Map#{1 -> 2, 2 -> 3});
        let m2 = Map1.toDelayMap(Map#{3 -> 4});
        DelayMap1.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def unionWith11(): Bool =
        let m1 = Map1.toDelayMap(Map#{3 -> 4});
        let m2 = Map1.toDelayMap(Map#{1 -> 2, 2 -> 3});
        DelayMap1.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def unionWith12(): Bool =
        let m1 = Map1.toDelayMap(Map#{1 -> 2, 2 -> 3});
        let m2 = Map1.toDelayMap(Map#{2 -> 8});
        DelayMap1.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> -5}

    @test
    def unionWith13(): Bool =
        let m1 = Map1.toDelayMap(Map#{1 -> 2, 2 -> 3});
        let m2 = Map1.toDelayMap(Map#{1 -> 8});
        DelayMap1.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap1.toMap == Map#{1 -> -6, 2 -> 3}

    @test
    def unionWith14(): Bool =
        let m1 = Map1.toDelayMap(Map#{1 -> 4});
        let m2 = Map1.toDelayMap(Map#{1 -> 2, 2 -> 3});
        DelayMap1.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 3}

    @test
    def unionWith15(): Bool =
        let m1 = Map1.toDelayMap(Map#{2 -> 7});
        let m2 = Map1.toDelayMap(Map#{1 -> 2, 2 -> 3});
        DelayMap1.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 4}

    @test
    def unionWith16(): Bool =
        let m1 = Map1.toDelayMap(Map#{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22});
        let m2 = Map1.toDelayMap(Map#{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33});
        DelayMap1.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap1.toMap == Map#{2 -> -1, 11 -> 14, 9 -> 8, 15 -> 1, 1 -> 2, 44 -> 33}


    /////////////////////////////////////////////////////////////////////////////
    // unionWithKey (pure)                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unionWithKeyPure01(): Bool =
        let m1 = Map1.toDelayMap(Map#{});
        let m2 = Map1.toDelayMap(Map#{});
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap1.toMap == Map#{}: Map[Int, Int]

    @test
    def unionWithKeyPure02(): Bool =
        let m1 = Map1.toDelayMap(Map#{1 -> 2});
        let m2 = Map1.toDelayMap(Map#{});
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap1.toMap == Map#{1 -> 2}

    @test
    def unionWithKeyPure03(): Bool =
        let m1 = Map1.toDelayMap(Map#{});
        let m2 = Map1.toDelayMap(Map#{1 -> 2});
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap1.toMap == Map#{1 -> 2}

    @test
    def unionWithKeyPure04(): Bool =
        let m1 = Map1.toDelayMap(Map#{});
        let m2 = Map1.toDelayMap(Map#{1 -> 2, 3 -> 4});
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 3 -> 4}

    @test
    def unionWithKeyPure05(): Bool =
        let m1 = Map1.toDelayMap(Map#{1 -> 2, 3 -> 4});
        let m2 = Map1.toDelayMap(Map#{});
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 3 -> 4}

    @test
    def unionWithKeyPure06(): Bool =
        let m1 = Map1.toDelayMap(Map#{1 -> 2});
        let m2 = Map1.toDelayMap(Map#{3 -> 4});
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 3 -> 4}

    @test
    def unionWithKeyPure07(): Bool =
        let m1 = Map1.toDelayMap(Map#{1 -> 2});
        let m2 = Map1.toDelayMap(Map#{1 -> 5});
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap1.toMap == Map#{1 -> -2}

    @test
    def unionWithKeyPure08(): Bool =
        let m1 = Map1.toDelayMap(Map#{});
        let m2 = Map1.toDelayMap(Map#{1 -> 2, 2 -> 3, 3 -> 4});
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def unionWithKeyPure09(): Bool =
        let m1 = Map1.toDelayMap(Map#{1 -> 2, 2 -> 3, 3 -> 4});
        let m2 = Map1.toDelayMap(Map#{});
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def unionWithKeyPure10(): Bool =
        let m1 = Map1.toDelayMap(Map#{1 -> 2, 2 -> 3});
        let m2 = Map1.toDelayMap(Map#{3 -> 4});
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def unionWithKeyPure11(): Bool =
        let m1 = Map1.toDelayMap(Map#{3 -> 4});
        let m2 = Map1.toDelayMap(Map#{1 -> 2, 2 -> 3});
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def unionWithKeyPure12(): Bool =
        let m1 = Map1.toDelayMap(Map#{1 -> 2, 2 -> 3});
        let m2 = Map1.toDelayMap(Map#{2 -> 8});
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> -3}

    @test
    def unionWithKeyPure13(): Bool =
        let m1 = Map1.toDelayMap(Map#{1 -> 2, 2 -> 3});
        let m2 = Map1.toDelayMap(Map#{1 -> 8});
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap1.toMap == Map#{1 -> -5, 2 -> 3}

    @test
    def unionWithKeyPure14(): Bool =
        let m1 = Map1.toDelayMap(Map#{1 -> 4});
        let m2 = Map1.toDelayMap(Map#{1 -> 2, 2 -> 3});
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap1.toMap == Map#{1 -> 3, 2 -> 3}

    @test
    def unionWithKeyPure15(): Bool =
        let m1 = Map1.toDelayMap(Map#{2 -> 7});
        let m2 = Map1.toDelayMap(Map#{1 -> 2, 2 -> 3});
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 6}

    @test
    def unionWithKeyPure16(): Bool =
        let m1 = Map1.toDelayMap(Map#{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22});
        let m2 = Map1.toDelayMap(Map#{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33});
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap1.toMap == Map#{2 -> 1, 11 -> 14, 9 -> 8, 15 -> 16, 1 -> 2, 44 -> 33}


    /////////////////////////////////////////////////////////////////////////////
    // unionWithKey (impure)                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unionWithKeyImpure01(): Bool & Impure =
        let m1 = Map1.toDelayMap(Map#{});
        let m2 = Map1.toDelayMap(Map#{});
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, m1, m2) |> DelayMap1.toMap == Map#{}: Map[Int, Int]

    @test
    def unionWithKeyImpure02(): Bool & Impure =
        let m1 = Map1.toDelayMap(Map#{1 -> 2});
        let m2 = Map1.toDelayMap(Map#{});
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, m1, m2) |> DelayMap1.toMap == Map#{1 -> 2}

    @test
    def unionWithKeyImpure03(): Bool & Impure =
        let m1 = Map1.toDelayMap(Map#{});
        let m2 = Map1.toDelayMap(Map#{1 -> 2});
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, m1, m2) |> DelayMap1.toMap == Map#{1 -> 2}

    @test
    def unionWithKeyImpure04(): Bool & Impure =
        let m1 = Map1.toDelayMap(Map#{});
        let m2 = Map1.toDelayMap(Map#{1 -> 2, 3 -> 4});
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 3 -> 4}

    @test
    def unionWithKeyImpure05(): Bool & Impure =
        let m1 = Map1.toDelayMap(Map#{1 -> 2, 3 -> 4});
        let m2 = Map1.toDelayMap(Map#{});
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 3 -> 4}

    @test
    def unionWithKeyImpure06(): Bool & Impure =
        let m1 = Map1.toDelayMap(Map#{1 -> 2});
        let m2 = Map1.toDelayMap(Map#{3 -> 4});
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 3 -> 4}

    @test
    def unionWithKeyImpure07(): Bool & Impure =
        let m1 = Map1.toDelayMap(Map#{1 -> 2});
        let m2 = Map1.toDelayMap(Map#{1 -> 5});
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, m1, m2) |> DelayMap1.toMap == Map#{1 -> -2}

    @test
    def unionWithKeyImpure08(): Bool & Impure =
        let m1 = Map1.toDelayMap(Map#{});
        let m2 = Map1.toDelayMap(Map#{1 -> 2, 2 -> 3, 3 -> 4});
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def unionWithKeyImpure09(): Bool & Impure =
        let m1 = Map1.toDelayMap(Map#{1 -> 2, 2 -> 3, 3 -> 4});
        let m2 = Map1.toDelayMap(Map#{});
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def unionWithKeyImpure10(): Bool & Impure =
        let m1 = Map1.toDelayMap(Map#{1 -> 2, 2 -> 3});
        let m2 = Map1.toDelayMap(Map#{3 -> 4});
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def unionWithKeyImpure11(): Bool & Impure =
        let m1 = Map1.toDelayMap(Map#{3 -> 4});
        let m2 = Map1.toDelayMap(Map#{1 -> 2, 2 -> 3});
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def unionWithKeyImpure12(): Bool & Impure =
        let m1 = Map1.toDelayMap(Map#{1 -> 2, 2 -> 3});
        let m2 = Map1.toDelayMap(Map#{2 -> 8});
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> -3}

    @test
    def unionWithKeyImpure13(): Bool & Impure =
        let m1 = Map1.toDelayMap(Map#{1 -> 2, 2 -> 3});
        let m2 = Map1.toDelayMap(Map#{1 -> 8});
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, m1, m2) |> DelayMap1.toMap == Map#{1 -> -5, 2 -> 3}

    @test
    def unionWithKeyImpure14(): Bool & Impure =
        let m1 = Map1.toDelayMap(Map#{1 -> 4});
        let m2 = Map1.toDelayMap(Map#{1 -> 2, 2 -> 3});
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, m1, m2) |> DelayMap1.toMap == Map#{1 -> 3, 2 -> 3}

    @test
    def unionWithKeyImpure15(): Bool & Impure =
        let m1 = Map1.toDelayMap(Map#{2 -> 7});
        let m2 = Map1.toDelayMap(Map#{1 -> 2, 2 -> 3});
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, m1, m2) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 6}

    @test
    def unionWithKeyImpure16(): Bool & Impure =
        let m1 = Map1.toDelayMap(Map#{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22});
        let m2 = Map1.toDelayMap(Map#{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33});
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, m1, m2) |> DelayMap1.toMap == Map#{2 -> 1, 11 -> 14, 9 -> 8, 15 -> 16, 1 -> 2, 44 -> 33}


    /////////////////////////////////////////////////////////////////////////////
    // unionWithKey unionWithKey                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unionWithKeyUnionWithKey01(): Bool =
        Map1.toDelayMap(Map#{1 -> 5}) |>
            DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2, Map1.toDelayMap(Map#{1 -> 2})) |>
            DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2, Map1.toDelayMap(Map#{1 -> 3})) |>
            DelayMap1.toMap == Map#{1 -> 6}

    @test
    def unionWithKeyUnionWithKey02(): Bool & Impure =
        Map1.toDelayMap(Map#{1 -> 5}) |>
            DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, Map1.toDelayMap(Map#{1 -> 2})) |>
            DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2, Map1.toDelayMap(Map#{1 -> 3})) |>
            DelayMap1.toMap == Map#{1 -> 6}

    @test
    def unionWithKeyUnionWithKey03(): Bool & Impure =
        Map1.toDelayMap(Map#{1 -> 5}) |>
            DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2, Map1.toDelayMap(Map#{1 -> 2})) |>
            DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, Map1.toDelayMap(Map#{1 -> 3})) |>
            DelayMap1.toMap == Map#{1 -> 6}

    @test
    def unionWithKeyUnionWithKey04(): Bool & Impure =
        Map1.toDelayMap(Map#{1 -> 5}) |>
            DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, Map1.toDelayMap(Map#{1 -> 2})) |>
            DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, Map1.toDelayMap(Map#{1 -> 3})) |>
            DelayMap1.toMap == Map#{1 -> 6}


    /////////////////////////////////////////////////////////////////////////////
    // unionWithKey (fusion)                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unionWithKeyFusion01(): Bool & Impure =
        let l = ref Nil;
        Map#{1 -> 1, 2 -> 2, 3 -> 3} |> Map1.toDelayMap |>
        DelayMap1.unionWithKey((_, _, v) -> { l := "a" :: deref l; v }, Map1.toDelayMap(Map#{1 -> 1, 2 -> 2, 3 -> 3})) |>
        DelayMap1.unionWithKey((_, _, v) -> { l := "b" :: deref l; v }, Map1.toDelayMap(Map#{1 -> 1, 2 -> 2, 3 -> 3}));
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def unionWithKeyFusion02(): Bool & Impure =
        let l = ref Nil;
        let m = Map#{1 -> 1, 2 -> 2, 3 -> 3} |> Map1.toDelayMap |>
        DelayMap1.unionWithKey((_, _, v) -> { l := "a" :: deref l; v } as & Pure, Map1.toDelayMap(Map#{1 -> 1, 2 -> 2, 3 -> 3})) |>
        DelayMap1.unionWithKey((_, _, v) -> { l := "b" :: deref l; v } as & Pure, Map1.toDelayMap(Map#{1 -> 1, 2 -> 2, 3 -> 3}));
        let _ = DelayMap1.toMap(m);
        List.reverse(deref l) == "a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // adjust                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def adjust01(): Bool =
        let m = Map1.toDelayMap(Map#{});
        DelayMap1.adjust(v -> 2 * v, 1, m) |> DelayMap1.toMap == Map#{}

    @test
    def adjust02(): Bool =
        let m = Map1.toDelayMap(Map#{1 -> 4});
        DelayMap1.adjust(v -> 2 * v, 1, m) |> DelayMap1.toMap == Map#{1 -> 8}

    @test
    def adjust03(): Bool =
        let m = Map1.toDelayMap(Map#{1 -> 4});
        DelayMap1.adjust(v -> 2 * v, 2, m) |> DelayMap1.toMap == Map#{1 -> 4}

    @test
    def adjust04(): Bool =
        let m = Map1.toDelayMap(Map#{1 -> -14, 5 -> -2});
        DelayMap1.adjust(v -> 2 * v, 1, m) |> DelayMap1.toMap == Map#{1 -> -28, 5 -> -2}

    @test
    def adjust05(): Bool =
        let m = Map1.toDelayMap(Map#{1 -> 4, 5 -> -2});
        DelayMap1.adjust(v -> 2 * v, 5, m) |> DelayMap1.toMap == Map#{1 -> 4, 5 -> -4}

    @test
    def adjust06(): Bool =
        let m = Map1.toDelayMap(Map#{1 -> 4, 5 -> -2});
        DelayMap1.adjust(v -> 2 * v, 4, m) |> DelayMap1.toMap == Map#{1 -> 4, 5 -> -2}


    /////////////////////////////////////////////////////////////////////////////
    // adjustWithKey                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def adjustWithKey01(): Bool =
        let m = Map1.toDelayMap(Map#{});
        DelayMap1.adjustWithKey((k, v) -> k + v, 1, m) |> DelayMap1.toMap == Map#{}

    @test
    def adjustWithKey02(): Bool =
        let m = Map1.toDelayMap(Map#{1 -> 4});
        DelayMap1.adjustWithKey((k, v) -> k + v, 1, m) |> DelayMap1.toMap == Map#{1 -> 5}

    @test
    def adjustWithKey03(): Bool =
        let m = Map1.toDelayMap(Map#{1 -> 4});
        DelayMap1.adjustWithKey((k, v) -> k + v, 2, m) |> DelayMap1.toMap == Map#{1 -> 4}

    @test
    def adjustWithKey04(): Bool =
        let m = Map1.toDelayMap(Map#{1 -> -14, 5 -> -2});
        DelayMap1.adjustWithKey((k, v) -> k + v, 1, m) |> DelayMap1.toMap == Map#{1 -> -13, 5 -> -2}

    @test
    def adjustWithKey05(): Bool =
        let m = Map1.toDelayMap(Map#{1 -> 4, 5 -> -2});
        DelayMap1.adjustWithKey((k, v) -> k + v, 5, m) |> DelayMap1.toMap == Map#{1 -> 4, 5 -> 3}

    @test
    def adjustWithKey06(): Bool =
        let m = Map1.toDelayMap(Map#{1 -> 4, 5 -> -2});
        DelayMap1.adjustWithKey((k, v) -> k + v, 4, m) |> DelayMap1.toMap == Map#{1 -> 4, 5 -> -2}


    /////////////////////////////////////////////////////////////////////////////
    // update                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def update01(): Bool =
        let m = Map1.toDelayMap(Map#{});
        DelayMap1.update(v -> if (v rem 2 != 0) Some(2 * v) else None, 1, m) |> DelayMap1.toMap == Map#{}

    @test
    def update02(): Bool =
        let m = Map1.toDelayMap(Map#{1 -> 3});
        DelayMap1.update(v -> if (v rem 2 != 0) Some(2 * v) else None, 1, m) |> DelayMap1.toMap == Map#{1 -> 6}

    @test
    def update03(): Bool =
        let m = Map1.toDelayMap(Map#{1 -> 4});
        DelayMap1.update(v -> if (v rem 2 != 0) Some(2 * v) else None, 1, m) |> DelayMap1.toMap == Map#{1 -> 4}

    @test
    def update04(): Bool =
        let m = Map1.toDelayMap(Map#{1 -> 4});
        DelayMap1.update(v -> if (v rem 2 != 0) Some(2 * v) else None, 2, m) |> DelayMap1.toMap == Map#{1 -> 4}

    @test
    def update05(): Bool =
        let m = Map1.toDelayMap(Map#{1 -> -14, 5 -> -2});
        DelayMap1.update(v -> if (v rem 2 != 0) Some(2 * v) else None, 1, m) |> DelayMap1.toMap == Map#{1 -> -14, 5 -> -2}

    @test
    def update06(): Bool =
        let m = Map1.toDelayMap(Map#{1 -> -13, 5 -> -2});
        DelayMap1.update(v -> if (v rem 2 != 0) Some(2 * v) else None, 1, m) |> DelayMap1.toMap == Map#{1 -> -26, 5 -> -2}

    @test
    def update07(): Bool =
        let m = Map1.toDelayMap(Map#{1 -> 4, 5 -> -2});
        DelayMap1.update(v -> if (v rem 2 != 0) Some(2 * v) else None, 5, m) |> DelayMap1.toMap == Map#{1 -> 4, 5 -> -2}

    @test
    def update08(): Bool =
        let m = Map1.toDelayMap(Map#{1 -> 4, 5 -> -1});
        DelayMap1.update(v -> if (v rem 2 != 0) Some(2 * v) else None, 5, m) |> DelayMap1.toMap == Map#{1 -> 4, 5 -> -2}

    @test
    def update09(): Bool =
        let m = Map1.toDelayMap(Map#{1 -> 4, 5 -> -2});
        DelayMap1.update(v -> if (v rem 2 != 0) Some(2 * v) else None, 4, m) |> DelayMap1.toMap == Map#{1 -> 4, 5 -> -2}


    /////////////////////////////////////////////////////////////////////////////
    // updateWithKey (pure)                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def updateWithKeyPure01(): Bool =
        let m = Map1.toDelayMap(Map#{});
        DelayMap1.updateWithKey((k, v) -> if (v rem 2 != 0) Some(k + 2 * v) else None, 1, m) |>
            DelayMap1.toMap == Map#{}

    @test
    def updateWithKeyPure02(): Bool =
        let m = Map1.toDelayMap(Map#{1 -> 3});
        DelayMap1.updateWithKey((k, v) -> if (v rem 2 != 0) Some(k + 2 * v) else None, 1, m) |>
            DelayMap1.toMap == Map#{1 -> 7}

    @test
    def updateWithKeyPure03(): Bool =
        let m = Map1.toDelayMap(Map#{1 -> 4});
        DelayMap1.updateWithKey((k, v) -> if (v rem 2 != 0) Some(k + 2 * v) else None, 1, m) |>
            DelayMap1.toMap == Map#{1 -> 4}

    @test
    def updateWithKeyPure04(): Bool =
        let m = Map1.toDelayMap(Map#{1 -> 4});
        DelayMap1.updateWithKey((k, v) -> if (v rem 2 != 0) Some(k + 2 * v) else None, 2, m) |>
            DelayMap1.toMap == Map#{1 -> 4}

    @test
    def updateWithKeyPure05(): Bool =
        let m = Map1.toDelayMap(Map#{1 -> -14, 5 -> -2});
        DelayMap1.updateWithKey((k, v) -> if (v rem 2 != 0) Some(k + 2 * v) else None, 1, m) |>
            DelayMap1.toMap == Map#{1 -> -14, 5 -> -2}

    @test
    def updateWithKeyPure06(): Bool =
        let m = Map1.toDelayMap(Map#{1 -> -13, 5 -> -2});
        DelayMap1.updateWithKey((k, v) -> if (v rem 2 != 0) Some(k + 2 * v) else None, 1, m) |>
            DelayMap1.toMap == Map#{1 -> -25, 5 -> -2}

    @test
    def updateWithKeyPure07(): Bool =
        let m = Map1.toDelayMap(Map#{1 -> 4, 5 -> -2});
        DelayMap1.updateWithKey((k, v) -> if (v rem 2 != 0) Some(k + 2 * v) else None, 5, m) |>
            DelayMap1.toMap == Map#{1 -> 4, 5 -> -2}

    @test
    def updateWithKeyPure08(): Bool =
        let m = Map1.toDelayMap(Map#{1 -> 4, 5 -> -1});
        DelayMap1.updateWithKey((k, v) -> if (v rem 2 != 0) Some(k + 2 * v) else None, 5, m) |>
            DelayMap1.toMap == Map#{1 -> 4, 5 -> 3}

    @test
    def updateWithKeyPure09(): Bool =
        let m = Map1.toDelayMap(Map#{1 -> 4, 5 -> -2});
        DelayMap1.updateWithKey((k, v) -> if (v rem 2 != 0) Some(k + 2 * v) else None, 4, m) |>
            DelayMap1.toMap == Map#{1 -> 4, 5 -> -2}


    /////////////////////////////////////////////////////////////////////////////
    // updateWithKey (pure)                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def updateWithKeyImpure01(): Bool & Impure =
        let m = Map1.toDelayMap(Map#{});
        DelayMap1.updateWithKey((k, v) -> if (v rem 2 != 0) Some(k + 2 * v) else None as & Impure, 1, m) |>
            DelayMap1.toMap == Map#{}

    @test
    def updateWithKeyImpure02(): Bool & Impure =
        let m = Map1.toDelayMap(Map#{1 -> 3});
        DelayMap1.updateWithKey((k, v) -> if (v rem 2 != 0) Some(k + 2 * v) else None as & Impure, 1, m) |>
            DelayMap1.toMap == Map#{1 -> 7}

    @test
    def updateWithKeyImpure03(): Bool & Impure =
        let m = Map1.toDelayMap(Map#{1 -> 4});
        DelayMap1.updateWithKey((k, v) -> if (v rem 2 != 0) Some(k + 2 * v) else None as & Impure, 1, m) |>
            DelayMap1.toMap == Map#{1 -> 4}

    @test
    def updateWithKeyImpure04(): Bool & Impure =
        let m = Map1.toDelayMap(Map#{1 -> 4});
        DelayMap1.updateWithKey((k, v) -> if (v rem 2 != 0) Some(k + 2 * v) else None as & Impure, 2, m) |>
            DelayMap1.toMap == Map#{1 -> 4}

    @test
    def updateWithKeyImpure05(): Bool & Impure =
        let m = Map1.toDelayMap(Map#{1 -> -14, 5 -> -2});
        DelayMap1.updateWithKey((k, v) -> if (v rem 2 != 0) Some(k + 2 * v) else None as & Impure, 1, m) |>
            DelayMap1.toMap == Map#{1 -> -14, 5 -> -2}

    @test
    def updateWithKeyImpure06(): Bool & Impure =
        let m = Map1.toDelayMap(Map#{1 -> -13, 5 -> -2});
        DelayMap1.updateWithKey((k, v) -> if (v rem 2 != 0) Some(k + 2 * v) else None as & Impure, 1, m) |>
            DelayMap1.toMap == Map#{1 -> -25, 5 -> -2}

    @test
    def updateWithKeyImpure07(): Bool & Impure =
        let m = Map1.toDelayMap(Map#{1 -> 4, 5 -> -2});
        DelayMap1.updateWithKey((k, v) -> if (v rem 2 != 0) Some(k + 2 * v) else None as & Impure, 5, m) |>
            DelayMap1.toMap == Map#{1 -> 4, 5 -> -2}

    @test
    def updateWithKeyImpure08(): Bool & Impure =
        let m = Map1.toDelayMap(Map#{1 -> 4, 5 -> -1});
        DelayMap1.updateWithKey((k, v) -> if (v rem 2 != 0) Some(k + 2 * v) else None as & Impure, 5, m) |>
            DelayMap1.toMap == Map#{1 -> 4, 5 -> 3}

    @test
    def updateWithKeyImpure09(): Bool & Impure =
        let m = Map1.toDelayMap(Map#{1 -> 4, 5 -> -2});
        DelayMap1.updateWithKey((k, v) -> if (v rem 2 != 0) Some(k + 2 * v) else None as & Impure, 4, m) |>
            DelayMap1.toMap == Map#{1 -> 4, 5 -> -2}


    /////////////////////////////////////////////////////////////////////////////
    // updateWithKey updateWithKey                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def updateWithKeyUpdateWithKey01(): Bool =
        Map1.toDelayMap(Map#{1 -> 4, 5 -> -2})                                          |>
            DelayMap1.updateWithKey((k, v) -> if (v == 4) Some(k + 2 * v) else None, 1) |>
            DelayMap1.updateWithKey((k, v) -> if (k == 5) Some(k + 2 * v) else None, 5) |>
            DelayMap1.toMap == Map#{1 -> 9, 5 -> 1}

    @test
    def updateWithKeyUpdateWithKey02(): Bool & Impure =
        Map1.toDelayMap(Map#{1 -> 4, 5 -> -2})                                                      |>
            DelayMap1.updateWithKey((k, v) -> if (v == 4) Some(k + 2 * v) else None as & Impure, 1) |>
            DelayMap1.updateWithKey((k, v) -> if (k == 5) Some(k + 2 * v) else None, 5)             |>
            DelayMap1.toMap == Map#{1 -> 9, 5 -> 1}

    @test
    def updateWithKeyUpdateWithKey03(): Bool & Impure =
        Map1.toDelayMap(Map#{1 -> 4, 5 -> -2})                                                      |>
            DelayMap1.updateWithKey((k, v) -> if (v == 4) Some(k + 2 * v) else None, 1)             |>
            DelayMap1.updateWithKey((k, v) -> if (k == 5) Some(k + 2 * v) else None as & Impure, 5) |>
            DelayMap1.toMap == Map#{1 -> 9, 5 -> 1}

    @test
    def updateWithKeyUpdateWithKey04(): Bool & Impure =
        Map1.toDelayMap(Map#{1 -> 4, 5 -> -2})                                                      |>
            DelayMap1.updateWithKey((k, v) -> if (v == 4) Some(k + 2 * v) else None as & Impure, 1) |>
            DelayMap1.updateWithKey((k, v) -> if (k == 5) Some(k + 2 * v) else None as & Impure, 5) |>
            DelayMap1.toMap == Map#{1 -> 9, 5 -> 1}


    /////////////////////////////////////////////////////////////////////////////
    // updateWithKey (fusion)                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def updateWithKeyFusion01(): Bool & Impure =
        let l = ref Nil;
        Map1.toDelayMap(Map#{1 -> 1, 2 -> 2, 3 -> 3}) |>
            DelayMap1.updateWithKey((_, v) -> { l := "a" :: deref l; Some(v) }, 1) |>
            DelayMap1.updateWithKey((_, v) -> { l := "a" :: deref l; Some(v) }, 2) |>
            DelayMap1.updateWithKey((_, v) -> { l := "a" :: deref l; Some(v) }, 3) |>
            DelayMap1.updateWithKey((_, v) -> { l := "b" :: deref l; Some(v) }, 1) |>
            DelayMap1.updateWithKey((_, v) -> { l := "b" :: deref l; Some(v) }, 2) |>
            DelayMap1.updateWithKey((_, v) -> { l := "b" :: deref l; Some(v) }, 3);
        List.reverse(deref l) == "a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil

    @test
    def updateWithKeyFusion02(): Bool & Impure =
        let l = ref Nil;
        let _ = Map1.toDelayMap(Map#{1 -> 1, 2 -> 2, 3 -> 3}) |>
            DelayMap1.updateWithKey((_, v) -> { l := "a" :: deref l; Some(v) } as & Pure, 1) |>
            DelayMap1.updateWithKey((_, v) -> { l := "a" :: deref l; Some(v) } as & Pure, 2) |>
            DelayMap1.updateWithKey((_, v) -> { l := "a" :: deref l; Some(v) } as & Pure, 3) |>
            DelayMap1.updateWithKey((_, v) -> { l := "b" :: deref l; Some(v) } as & Pure, 1) |>
            DelayMap1.updateWithKey((_, v) -> { l := "b" :: deref l; Some(v) } as & Pure, 2) |>
            DelayMap1.updateWithKey((_, v) -> { l := "b" :: deref l; Some(v) } as & Pure, 3) |>
            DelayMap1.toMap;
        List.reverse(deref l) == "a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def size01(): Bool =
        Map#{} |> Map1.toDelayMap |> DelayMap1.size == 0

    @test
    def size02(): Bool =
        Map#{1 -> 2} |> Map1.toDelayMap |> DelayMap1.size == 1

    @test
    def size03(): Bool =
        Map#{1 -> 2, 2 -> 4} |> Map1.toDelayMap |> DelayMap1.size == 2

    @test
    def size04(): Bool =
        Map#{1 -> 2, 2 -> 4, 3 -> 6} |> Map1.toDelayMap |> DelayMap1.size == 3

    @test
    def size05(): Bool =
        Map#{1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8} |> Map1.toDelayMap |> DelayMap1.size == 4

    @test
    def size06(): Bool =
        Map#{1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8, 5 -> 10} |> Map1.toDelayMap |> DelayMap1.size == 5


    /////////////////////////////////////////////////////////////////////////////
    // singleton                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def singleton01(): Bool =
        DelayMap1.singleton(1, 2) |> DelayMap1.toMap == Map#{1 -> 2}

    @test
    def singleton02(): Bool =
        DelayMap1.singleton(3, -1) |> DelayMap1.toMap == Map#{3 -> -1}

    @test
    def singleton03(): Bool =
        DelayMap1.singleton(-99, -11) |> DelayMap1.toMap == Map#{-99 -> -11}

}
