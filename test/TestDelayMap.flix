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
        (("a", 1) :: Nil)                                     |>
            List1.toDelayMap                                  |>
            DelayMap1.insertWith((v1, v2) -> v1 + v2, "b", 1) |>
            DelayMap1.insertWith((v1, v2) -> v1 + v2, "b", 3) |>
            DelayMap1.toList == ("a", 1) :: ("b", 4) :: Nil

    @test
    def insertWithInsertWith02(): Bool & Impure =
        (("a", 1) :: Nil)                                                 |>
            List1.toDelayMap                                              |>
            DelayMap1.insertWith((v1, v2) -> v1 + v2, "b" as & Impure, 1) |>
            DelayMap1.insertWith((v1, v2) -> v1 + v2, "b", 3)             |>
            DelayMap1.toList == ("a", 1) :: ("b", 4) :: Nil
    @test
    def insertWithInsertWith03(): Bool & Impure =
        (("a", 1) :: Nil)                                                 |>
            List1.toDelayMap                                              |>
            DelayMap1.insertWith((v1, v2) -> v1 + v2, "b", 1)             |>
            DelayMap1.insertWith((v1, v2) -> v1 + v2, "b" as & Impure, 3) |>
            DelayMap1.toList == ("a", 1) :: ("b", 4) :: Nil

    @test
    def insertWithInsertWith04(): Bool & Impure =
        (("a", 1) :: Nil)                                                 |>
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
        let listBeforeEvaluation = deref l;
        let _ = DelayMap1.toList(m);
        listBeforeEvaluation == Nil and List.reverse(deref l) == "a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil


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
        Map#{2 -> -1, 0 -> 0} |> Map1.toDelayMap |> DelayMap1.map(v -> 3 * v) |> DelayMap1.toMap == Map#{2 -> -3, 0 -> 0}

    @test
    def mapPure04(): Bool =
        Map#{2 -> -1, 5 -> 15, 11 -> -9} |> Map1.toDelayMap |> DelayMap1.map(v -> 3 * v) |> DelayMap1.toMap == Map#{2 -> -3, 5 -> 45, 11 -> -27}

    @test
    def mapPure05(): Bool =
        Map#{2 -> -1, 5 -> 15, 11 -> -9, 8 -> 8} |> Map1.toDelayMap |> DelayMap1.map(v -> 3 * v) |> DelayMap1.toMap == Map#{2 -> -3, 5 -> 45, 11 -> -27, 8 -> 24}


    /////////////////////////////////////////////////////////////////////////////
    // map (impure)                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapImpure01(): Bool & Impure =
        Map#{}: Map[Unit, Int32] |> Map1.toDelayMap |> DelayMap1.map(v -> 3 * v as & Impure) |> DelayMap1.toMap == Map#{}

    @test
    def mapImpure02(): Bool & Impure =
        Map#{1 -> 4} |> Map1.toDelayMap |> DelayMap1.map(v -> 3 * v as & Impure) |> DelayMap1.toMap == Map#{1 -> 12}

    @test
    def mapImpure03(): Bool & Impure =
        Map#{2 -> -1, 0 -> 0} |> Map1.toDelayMap |> DelayMap1.map(v -> 3 * v as & Impure) |> DelayMap1.toMap == Map#{2 -> -3, 0 -> 0}

    @test
    def mapImpure04(): Bool & Impure =
        Map#{2 -> -1, 5 -> 15, 11 -> -9} |> Map1.toDelayMap |> DelayMap1.map(v -> 3 * v as & Impure) |> DelayMap1.toMap == Map#{2 -> -3, 5 -> 45, 11 -> -27}

    @test
    def mapImpure05(): Bool & Impure =
        Map#{2 -> -1, 5 -> 15, 11 -> -9, 8 -> 8} |> Map1.toDelayMap |> DelayMap1.map(v -> 3 * v as & Impure) |> DelayMap1.toMap == Map#{2 -> -3, 5 -> 45, 11 -> -27, 8 -> 24}


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
    // map map (fusion)                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapFusion01(): Bool & Impure =
        let l = ref Nil;
        ((1, 1) :: (2, 2) :: (3, 3) :: Nil) |> List1.toDelayMap |>
        DelayMap1.map(v -> { l := "a" :: deref l; v }) |>
        DelayMap1.map(v -> { l := "b" :: deref l; v });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def mapFusion02(): Bool & Impure =
        let l = ref Nil;
        let m = ((1, 1) :: (2, 2) :: (3, 3) :: Nil) |> List1.toDelayMap |>
        DelayMap1.map(v -> { l := "a" :: deref l; v } as & Pure) |>
        DelayMap1.map(v -> { l := "b" :: deref l; v } as & Pure);
        let listBeforeEvaluation = deref l;
        let _ = DelayMap1.toList(m);
        listBeforeEvaluation == Nil and List.reverse(deref l) == "a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // mapWithKey                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithKey01(): Bool =
        Map#{}: Map[Unit, Unit] |> Map1.toDelayMap |> DelayMap1.mapWithKey((_, v) -> v) |> DelayMap1.toMap == Map#{}

    @test
    def mapWithKey02(): Bool =
        Map#{1 -> 4} |> Map1.toDelayMap |> DelayMap1.mapWithKey((k, v) -> k + v) |> DelayMap1.toMap == Map#{1 -> 5}

    @test
    def mapWithKey03(): Bool =
        Map#{2 -> -1, 0 -> 0} |> Map1.toDelayMap |> DelayMap1.mapWithKey((k, v) -> k + v) |> DelayMap1.toMap == Map#{2 -> 1, 0 -> 0}

    @test
    def mapWithKey04(): Bool =
        Map#{2 -> -1, 5 -> 15, 11 -> -9} |> Map1.toDelayMap |> DelayMap1.mapWithKey((k, v) -> k + v) |> DelayMap1.toMap == Map#{2 -> 1, 5 -> 20, 11 -> 2}

    @test
    def mapWithKey05(): Bool =
        Map#{2 -> -1, 5 -> 15, 11 -> -9, 8 -> 8} |> Map1.toDelayMap |> DelayMap1.mapWithKey((k, v) -> k + v) |> DelayMap1.toMap
                                    == Map#{2 -> 1, 5 -> 20, 11 -> 2, 8 -> 16}


    /////////////////////////////////////////////////////////////////////////////
    // union                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def union01(): Bool =
        DelayMap1.union(Map1.toDelayMap(Map#{}: Map[Unit, Unit]), Map1.toDelayMap(Map#{})) |> DelayMap1.toMap == Map#{}

    @test
    def union02(): Bool =
        DelayMap1.union(Map1.toDelayMap(Map#{1 -> 2}), Map1.toDelayMap(Map#{})) |> DelayMap1.toMap == Map#{1 -> 2}

    @test
    def union03(): Bool =
        DelayMap1.union(Map1.toDelayMap(Map#{}), Map1.toDelayMap(Map#{1 -> 2})) |> DelayMap1.toMap == Map#{1 -> 2}

    @test
    def union04(): Bool =
        DelayMap1.union(Map1.toDelayMap(Map#{}), Map1.toDelayMap(Map#{1 -> 2, 3 -> 4})) |> DelayMap1.toMap == Map#{1 -> 2, 3 -> 4}

    @test
    def union05(): Bool =
        DelayMap1.union(Map1.toDelayMap(Map#{1 -> 2, 3 -> 4}), Map1.toDelayMap(Map#{})) |> DelayMap1.toMap == Map#{1 -> 2, 3 -> 4}

    @test
    def union06(): Bool =
        DelayMap1.union(Map1.toDelayMap(Map#{1 -> 2}), Map1.toDelayMap(Map#{3 -> 4})) |> DelayMap1.toMap == Map#{1 -> 2, 3 -> 4}

    @test
    def union07(): Bool =
        DelayMap1.union(Map1.toDelayMap(Map#{1 -> 2}), Map1.toDelayMap(Map#{1 -> 5})) |> DelayMap1.toMap == Map#{1 -> 2}

    @test
    def union08(): Bool =
        DelayMap1.union(Map1.toDelayMap(Map#{}), Map1.toDelayMap(Map#{1 -> 2, 2 -> 3, 3 -> 4})) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def union09(): Bool =
        DelayMap1.union(Map1.toDelayMap(Map#{1 -> 2, 2 -> 3, 3 -> 4}), Map1.toDelayMap(Map#{})) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def union10(): Bool =
        DelayMap1.union(Map1.toDelayMap(Map#{1 -> 2, 2 -> 3}), Map1.toDelayMap(Map#{3 -> 4})) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def union11(): Bool =
        DelayMap1.union(Map1.toDelayMap(Map#{3 -> 4}), Map1.toDelayMap(Map#{1 -> 2, 2 -> 3})) |> DelayMap1.toMap== Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def union12(): Bool =
        DelayMap1.union(Map1.toDelayMap(Map#{1 -> 2, 2 -> 3}), Map1.toDelayMap(Map#{2 -> 8})) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 3}

    @test
    def union13(): Bool =
        DelayMap1.union(Map1.toDelayMap(Map#{1 -> 2, 2 -> 3}), Map1.toDelayMap(Map#{1 -> 8})) |> DelayMap1.toMap== Map#{1 -> 2, 2 -> 3}

    @test
    def union14(): Bool =
        DelayMap1.union(Map1.toDelayMap(Map#{1 -> 4}), Map1.toDelayMap(Map#{1 -> 2, 2 -> 3})) |> DelayMap1.toMap == Map#{1 -> 4, 2 -> 3}

    @test
    def union15(): Bool =
        DelayMap1.union(Map1.toDelayMap(Map#{2 -> 7}), Map1.toDelayMap(Map#{1 -> 2, 2 -> 3})) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 7}

    @test
    def union16(): Bool =
        DelayMap1.union(Map1.toDelayMap(Map#{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22}), Map1.toDelayMap(Map#{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33})) |> DelayMap1.toMap == Map#{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22, 1 -> 2, 44 -> 33}


    /////////////////////////////////////////////////////////////////////////////
    // unionWith                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unionWith01(): Bool =
        DelayMap1.unionWith((v1, v2) -> v1 - v2, Map1.toDelayMap(Map#{}), Map1.toDelayMap(Map#{})) |> DelayMap1.toMap == Map#{}: Map[Int, Int]

    @test
    def unionWith02(): Bool =
        DelayMap1.unionWith((v1, v2) -> v1 - v2, Map1.toDelayMap(Map#{1 -> 2}), Map1.toDelayMap(Map#{})) |> DelayMap1.toMap == Map#{1 -> 2}

    @test
    def unionWith03(): Bool =
        DelayMap1.unionWith((v1, v2) -> v1 - v2, Map1.toDelayMap(Map#{}), Map1.toDelayMap(Map#{1 -> 2})) |> DelayMap1.toMap == Map#{1 -> 2}

    @test
    def unionWith04(): Bool =
        DelayMap1.unionWith((v1, v2) -> v1 - v2, Map1.toDelayMap(Map#{}), Map1.toDelayMap(Map#{1 -> 2, 3 -> 4})) |> DelayMap1.toMap == Map#{1 -> 2, 3 -> 4}

    @test
    def unionWith05(): Bool =
        DelayMap1.unionWith((v1, v2) -> v1 - v2, Map1.toDelayMap(Map#{1 -> 2, 3 -> 4}), Map1.toDelayMap(Map#{})) |> DelayMap1.toMap == Map#{1 -> 2, 3 -> 4}

    @test
    def unionWith06(): Bool =
        DelayMap1.unionWith((v1, v2) -> v1 - v2, Map1.toDelayMap(Map#{1 -> 2}), Map1.toDelayMap(Map#{3 -> 4})) |> DelayMap1.toMap == Map#{1 -> 2, 3 -> 4}

    @test
    def unionWith07(): Bool =
        DelayMap1.unionWith((v1, v2) -> v1 - v2, Map1.toDelayMap(Map#{1 -> 2}), Map1.toDelayMap(Map#{1 -> 5})) |> DelayMap1.toMap == Map#{1 -> -3}

    @test
    def unionWith08(): Bool =
        DelayMap1.unionWith((v1, v2) -> v1 - v2, Map1.toDelayMap(Map#{}), Map1.toDelayMap(Map#{1 -> 2, 2 -> 3, 3 -> 4})) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def unionWith09(): Bool =
        DelayMap1.unionWith((v1, v2) -> v1 - v2, Map1.toDelayMap(Map#{1 -> 2, 2 -> 3, 3 -> 4}), Map1.toDelayMap(Map#{})) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def unionWith10(): Bool =
        DelayMap1.unionWith((v1, v2) -> v1 - v2, Map1.toDelayMap(Map#{1 -> 2, 2 -> 3}), Map1.toDelayMap(Map#{3 -> 4})) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def unionWith11(): Bool =
        DelayMap1.unionWith((v1, v2) -> v1 - v2, Map1.toDelayMap(Map#{3 -> 4}), Map1.toDelayMap(Map#{1 -> 2, 2 -> 3})) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def unionWith12(): Bool =
        DelayMap1.unionWith((v1, v2) -> v1 - v2, Map1.toDelayMap(Map#{1 -> 2, 2 -> 3}), Map1.toDelayMap(Map#{2 -> 8})) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> -5}

    @test
    def unionWith13(): Bool =
        DelayMap1.unionWith((v1, v2) -> v1 - v2, Map1.toDelayMap(Map#{1 -> 2, 2 -> 3}), Map1.toDelayMap(Map#{1 -> 8})) |> DelayMap1.toMap == Map#{1 -> -6, 2 -> 3}

    @test
    def unionWith14(): Bool =
        DelayMap1.unionWith((v1, v2) -> v1 - v2, Map1.toDelayMap(Map#{1 -> 4}), Map1.toDelayMap(Map#{1 -> 2, 2 -> 3})) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 3}

    @test
    def unionWith15(): Bool =
        DelayMap1.unionWith((v1, v2) -> v1 - v2, Map1.toDelayMap(Map#{2 -> 7}), Map1.toDelayMap(Map#{1 -> 2, 2 -> 3})) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 4}

    @test
    def unionWith16(): Bool =
        DelayMap1.unionWith((v1, v2) -> v1 - v2, Map1.toDelayMap(Map#{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22}), Map1.toDelayMap(Map#{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33})) |> DelayMap1.toMap == Map#{2 -> -1, 11 -> 14, 9 -> 8, 15 -> 1, 1 -> 2, 44 -> 33}


    /////////////////////////////////////////////////////////////////////////////
    // unionWithKey                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unionWithKey01(): Bool =
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2, Map1.toDelayMap(Map#{}), Map1.toDelayMap(Map#{})) |> DelayMap1.toMap == Map#{}: Map[Int, Int]

    @test
    def unionWithKey02(): Bool =
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2, Map1.toDelayMap(Map#{1 -> 2}), Map1.toDelayMap(Map#{})) |> DelayMap1.toMap == Map#{1 -> 2}

    @test
    def unionWithKey03(): Bool =
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2, Map1.toDelayMap(Map#{}), Map1.toDelayMap(Map#{1 -> 2})) |> DelayMap1.toMap == Map#{1 -> 2}

    @test
    def unionWithKey04(): Bool =
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2, Map1.toDelayMap(Map#{}), Map1.toDelayMap(Map#{1 -> 2, 3 -> 4})) |> DelayMap1.toMap == Map#{1 -> 2, 3 -> 4}

    @test
    def unionWithKey05(): Bool =
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2, Map1.toDelayMap(Map#{1 -> 2, 3 -> 4}), Map1.toDelayMap(Map#{})) |> DelayMap1.toMap == Map#{1 -> 2, 3 -> 4}

    @test
    def unionWithKey06(): Bool =
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2, Map1.toDelayMap(Map#{1 -> 2}), Map1.toDelayMap(Map#{3 -> 4})) |> DelayMap1.toMap == Map#{1 -> 2, 3 -> 4}

    @test
    def unionWithKey07(): Bool =
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2, Map1.toDelayMap(Map#{1 -> 2}), Map1.toDelayMap(Map#{1 -> 5})) |> DelayMap1.toMap == Map#{1 -> -2}

    @test
    def unionWithKey08(): Bool =
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2, Map1.toDelayMap(Map#{}), Map1.toDelayMap(Map#{1 -> 2, 2 -> 3, 3 -> 4})) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def unionWithKey09(): Bool =
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2, Map1.toDelayMap(Map#{1 -> 2, 2 -> 3, 3 -> 4}), Map1.toDelayMap(Map#{})) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def unionWithKey10(): Bool =
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2, Map1.toDelayMap(Map#{1 -> 2, 2 -> 3}), Map1.toDelayMap(Map#{3 -> 4})) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def unionWithKey11(): Bool =
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2, Map1.toDelayMap(Map#{3 -> 4}), Map1.toDelayMap(Map#{1 -> 2, 2 -> 3})) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def unionWithKey12(): Bool =
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2, Map1.toDelayMap(Map#{1 -> 2, 2 -> 3}), Map1.toDelayMap(Map#{2 -> 8})) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> -3}

    @test
    def unionWithKey13(): Bool =
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2, Map1.toDelayMap(Map#{1 -> 2, 2 -> 3}), Map1.toDelayMap(Map#{1 -> 8})) |> DelayMap1.toMap == Map#{1 -> -5, 2 -> 3}

    @test
    def unionWithKey14(): Bool =
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2, Map1.toDelayMap(Map#{1 -> 4}), Map1.toDelayMap(Map#{1 -> 2, 2 -> 3})) |> DelayMap1.toMap == Map#{1 -> 3, 2 -> 3}

    @test
    def unionWithKey15(): Bool =
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2, Map1.toDelayMap(Map#{2 -> 7}), Map1.toDelayMap(Map#{1 -> 2, 2 -> 3})) |> DelayMap1.toMap == Map#{1 -> 2, 2 -> 6}

    @test
    def unionWithKey16(): Bool =
        DelayMap1.unionWithKey((k, v1, v2) -> k + v1 - v2, Map1.toDelayMap(Map#{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22}), Map1.toDelayMap(Map#{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33})) |> DelayMap1.toMap == Map#{2 -> 1, 11 -> 14, 9 -> 8, 15 -> 16, 1 -> 2, 44 -> 33}
}
