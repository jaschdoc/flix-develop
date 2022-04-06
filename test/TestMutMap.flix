/*
 * Copyright 2020 Esben Bjerre
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
namespace TestMutMap1 {
/*
    /////////////////////////////////////////////////////////////////////////////
    // sameElements                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sameElements01(): Bool = region r {
        let m1 = new MutMap1(r): MutMap1[Int32, Int32, _];
        let m2 = new MutMap1(r): MutMap1[Int32, Int32, _];
        MutMap1.sameElements(m1, m2)
    }

    @test
    def sameElements02(): Bool = region r {
        let m = new MutMap1(r);
        MutMap1.put!(1, "a", m);

        let m2 = new MutMap1(r);
        MutMap1.put!(1, "a", m2);

        MutMap1.sameElements(m, m2)
    }

    @test
    def sameElements03(): Bool = region r {
        let m = new MutMap1(r);
        MutMap1.put!(1, "a", m);
        MutMap1.put!(1, "b", m);

        let m2 = new MutMap1(r);
        MutMap1.put!(1, "a", m2);

        not MutMap1.sameElements(m, m2)
    }

    @test
    def sameElements04(): Bool = region r {
        let m = new MutMap1(r);
        MutMap1.put!(1, "a", m);
        MutMap1.put!(2, "b", m);

        let m2 = new MutMap1(r);
        MutMap1.put!(1, "a", m2);

        not MutMap1.sameElements(m, m2)
    }

    @test
    def sameElements05(): Bool = region r {
        let m = new MutMap1(r);
        MutMap1.put!(1, "a", m);

        let m2 = new MutMap1(r);
        MutMap1.put!(1, "a", m2);
        MutMap1.put!(2, "b", m2);

        not MutMap1.sameElements(m, m2)
    }

    @test
    def sameElements06(): Bool = region r {
        let m = new MutMap1(r);
        MutMap1.put!(2, "b", m);
        MutMap1.put!(3, "c", m);
        MutMap1.put!(1, "a", m);

        let m2 = new MutMap1(r);
        MutMap1.put!(1, "a", m2);
        MutMap1.put!(2, "b", m2);
        MutMap1.put!(3, "c", m2);

        MutMap1.sameElements(m, m2)
    }
*/
/*
    /////////////////////////////////////////////////////////////////////////////
    // mergeWithKey                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def merge01(): Bool = region r {
        let m1 = new MutMap1(r): MutMap1[Int32, Int32, _];
        let m2 = new MutMap1(r): MutMap1[Int32, Int32, _];
        MutMap1.mergeWithKey!((_, v, _) -> v, m1, m2);
        MutMap1.isEmpty(m2)
    }

    @test
    def merge02(): Bool = region r {
        let m1 = new MutMap1(r);
        let m2 = new MutMap1(r);
        let _ = MutMap1.put!(1, 0, m1);
        let _ = MutMap1.put!(2, 0, m2);
        MutMap1.mergeWithKey!((_, v, _) -> v, m1, m2);
        MutMap1.memberOf(1, m2) and MutMap1.memberOf(2, m2)
    }
*/

    /////////////////////////////////////////////////////////////////////////////
    // sumValues                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumValues01(): Bool = region r {
        new MutMap1(r) |> MutMap1.sumValues == 0
    }

    @test
    def sumValues02(): Bool = region r {
        MutMap1(ref Map#{1 => 1}) |>
            MutMap1.sumValues == 1
    }

    @test
    def sumValues03(): Bool = region r {
        MutMap1(ref Map#{1 => 1, 2 => 2, 3 => 3}) |>
            MutMap1.sumValues == 6
    }

    @test
    def sumValues04(): Bool = region r {
        MutMap1(ref Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3}) |>
            MutMap1.sumValues == 3
    }

    @test
    def sumValues05(): Bool = region r {
        MutMap1(ref Map#{-1 => -1, -2 => -2, -3 => -3, -4 => -4}) |>
            MutMap1.sumValues == -10
    }

    @test
    def sumValues06(): Bool = region r {
        MutMap1(ref Map#{10 => 10, -10 => -10}) |>
            MutMap1.sumValues == 0
    }


    /////////////////////////////////////////////////////////////////////////////
    // sumKeys                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumKeys01(): Bool = region r {
        new MutMap1(r) |> MutMap1.sumKeys == 0
    }

    @test
    def sumKeys02(): Bool = region r {
        MutMap1(ref Map#{1 => 1}) |>
            MutMap1.sumKeys == 1
    }

    @test
    def sumKeys03(): Bool = region r {
        MutMap1(ref Map#{1 => 1, 2 => 2, 3 => 3}) |>
            MutMap1.sumKeys == 6
    }

    @test
    def sumKeys04(): Bool = region r {
        MutMap1(ref Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3}) |>
            MutMap1.sumKeys == 3
    }

    @test
    def sumKeys05(): Bool = region r {
        MutMap1(ref Map#{-1 => -1, -2 => -2, -5 => -3, -4 => -4}) |>
            MutMap1.sumKeys == -12
    }

    @test
    def sumKeys06(): Bool = region r {
        MutMap1(ref Map#{10 => 10, -10 => -10}) |>
            MutMap1.sumKeys == 0
    }


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumWith01(): Bool = region r {
        new MutMap1(r) |> MutMap1.sumWith((k, v) -> k + v) == 0
    }

    @test
    def sumWith02(): Bool = region r {
        MutMap1(ref Map#{1 => 1}) |>
            MutMap1.sumWith((k, v) -> k + v) == 2
    }

    @test
    def sumWith03(): Bool = region r {
        MutMap1(ref Map#{1 => 1, 2 => 2, 3 => 3}) |>
            MutMap1.sumWith((k, v) -> k + v) == 12
    }

    @test
    def sumWith04(): Bool = region r {
        MutMap1(ref Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3}) |>
            MutMap1.sumWith((k, v) -> k + v) == 6
    }

    @test
    def sumWith05(): Bool = region r {
        MutMap1(ref Map#{-1 => -1, -2 => -2, -3 => -3, -4 => -4}) |>
            MutMap1.sumWith((k, v) -> k + v) == -20
    }

    @test
    def sumWith06(): Bool = region r {
        MutMap1(ref Map#{10 => 10, -10 => -10}) |>
            MutMap1.sumWith((k, v) -> k + v) == 0
    }


    /////////////////////////////////////////////////////////////////////////////
    // productValues                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productValues01(): Bool = region r {
        new MutMap1(r) |> MutMap1.productValues == 0
    }

    @test
    def productValues02(): Bool = region r {
        MutMap1(ref Map#{1 => 1}) |>
            MutMap1.productValues == 1
    }

    @test
    def productValues03(): Bool = region r {
        MutMap1(ref Map#{1 => 1, 2 => 2, 3 => 3}) |>
            MutMap1.productValues == 6
    }

    @test
    def productValues04(): Bool = region r {
        MutMap1(ref Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3}) |>
            MutMap1.productValues == -18
    }

    @test
    def productValues05(): Bool = region r {
        MutMap1(ref Map#{-1 => -1, -2 => -2, -5 => -3, -4 => -4}) |>
            MutMap1.productValues == 24
    }

    @test
    def productValues06(): Bool = region r {
        MutMap1(ref Map#{10 => 10, -10 => -10}) |>
            MutMap1.productValues == -100
    }


    /////////////////////////////////////////////////////////////////////////////
    // productKeys                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productKeys01(): Bool = region r {
        new MutMap1(r) |> MutMap1.productValues == 0
    }

    @test
    def productKeys02(): Bool = region r {
        MutMap1(ref Map#{1 => 1}) |>
            MutMap1.productKeys == 1
    }

    @test
    def productKeys03(): Bool = region r {
        MutMap1(ref Map#{1 => 1, 2 => 2, 3 => 3}) |>
            MutMap1.productKeys == 6
    }

    @test
    def productKeys04(): Bool = region r {
        MutMap1(ref Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3}) |>
            MutMap1.productKeys == -18
    }

    @test
    def productKeys05(): Bool = region r {
        MutMap1(ref Map#{-1 => -1, -2 => -2, -5 => -3, -4 => -4}) |>
            MutMap1.productKeys == 40
    }

    @test
    def productKeys06(): Bool = region r {
        MutMap1(ref Map#{10 => 10, -10 => -10}) |>
            MutMap1.productKeys == -100
    }


    /////////////////////////////////////////////////////////////////////////////
    // productWith                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productWith01(): Bool = region r {
        new MutMap1(r) |> MutMap1.productWith((k, v) -> k + v) == 0
    }

    @test
    def productWith02(): Bool = region r {
        MutMap1(ref Map#{1 => 1}) |>
            MutMap1.productWith((k, v) -> k + v) == 2
    }

    @test
    def productWith03(): Bool = region r {
        MutMap1(ref Map#{1 => 1, 2 => 2, 3 => 3}) |>
            MutMap1.productWith((k, v) -> k + v) == 48
    }

    @test
    def productWith04(): Bool = region r {
        MutMap1(ref Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3}) |>
            MutMap1.productWith((k, v) -> k + v) == -288
    }

    @test
    def productWith05(): Bool = region r {
        MutMap1(ref Map#{-1 => -1, -2 => -2, -3 => -3, -4 => -4}) |>
            MutMap1.productWith((k, v) -> k + v) == 384
    }

    @test
    def productWith06(): Bool = region r {
        MutMap1(ref Map#{10 => 10, -10 => -10}) |>
            MutMap1.productWith((k, v) -> k + v) == -400
    }


    /////////////////////////////////////////////////////////////////////////////
    // joinKeys                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinKeys01(): Bool = region r {
        new MutMap1(r): MutMap1[Int32, Int32, _] |> MutMap1.joinKeys(",") == ""
    }

    @test
    def joinKeys02(): Bool = region r {
        MutMap1(ref Map#{1 => 1}) |> MutMap1.joinKeys(",") == "1"
    }

    @test
    def joinKeys03(): Bool = region r {
        MutMap1(ref Map#{0 => 1, 1 => 2, 2 => 2}) |> MutMap1.joinKeys(",") == "0,1,2"
    }

    @test
    def joinKeys04(): Bool = region r {
        MutMap1(ref Map#{"0" => 1, "1" => 2, "2" => 2}) |> MutMap1.joinKeys(",") == "0,1,2"
    }


    /////////////////////////////////////////////////////////////////////////////
    // joinValues                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinValues01(): Bool = region r {
        new MutMap1(r): MutMap1[Int32, Int32, _] |> MutMap1.joinValues(",") == ""
    }

    @test
    def joinValues02(): Bool = region r {
        MutMap1(ref Map#{1 => 1}) |> MutMap1.joinValues(",") == "1"
    }

    @test
    def joinValues03(): Bool = region r {
        MutMap1(ref Map#{0 => 1, 1 => 2, 2 => 2}) |> MutMap1.joinValues(",") == "1,2,2"
    }

    @test
    def joinValues04(): Bool = region r {
        MutMap1(ref Map#{0 => "1", 1 => "2", 2 => "2"}) |> MutMap1.joinValues(",") == "1,2,2"
    }


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinWith01(): Bool = region r {
        new MutMap1(r): MutMap1[Int32, Int32, _] |>
            MutMap1.joinWith((k, v) -> "${k} => ${v}", ",") == ""
    }

    @test
    def joinWith02(): Bool = region r {
        MutMap1(ref Map#{1 => 1}) |>
            MutMap1.joinWith((k, v) -> "${k} => ${v}", ", ") == "1 => 1"
    }

    @test
    def joinWith03(): Bool = region r {
        MutMap1(ref Map#{0 => 1, 1 => 2, 2 => 2}) |>
            MutMap1.joinWith((k, v) -> "${k} => ${v}", ", ") == "0 => 1, 1 => 2, 2 => 2"
    }

    @test
    def joinWith04(): Bool = region r {
        MutMap1(ref Map#{0 => "1", 1 => "2", 2 => "2"}) |>
            MutMap1.joinWith((k, v) -> "${k} => ${v}", ", ") == "0 => 1, 1 => 2, 2 => 2"
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldRightLazy                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRightLazy01(): Bool = region r {
        MutMap1.foldRightLazy((v, s) -> force s + v, 0, new MutMap1(r)) == 0
    }

    @test
    def foldRightLazy02(): Bool = region r {
        let m = new MutMap1(r);
        MutMap1.put!(1, 2, m);
        MutMap1.foldRightLazy((v, s) -> force s + v, 0, m) == 2
    }

    @test
    def foldRightLazy03(): Bool = region r {
        let m = MutMap1(ref Map#{1 => 2, 3 => 4});
        MutMap1.foldRightLazy((v, s) -> force s + v, 0, m) == 6
    }

    @test
    def foldRightLazy04(): Bool = region r {
        let m = MutMap1(ref Map#{1 => 2, 3 => 4, 5 => 6});
        MutMap1.foldRightLazy((v, s) -> force s + v, 0, m) == 12
    }

}
