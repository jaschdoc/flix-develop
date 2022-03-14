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

namespace TestMutSet1 {

    /////////////////////////////////////////////////////////////////////////////
    // sameElements                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sameElements01(): Bool = region r {
        let s = MutSet1.emptyWithRegion(r): MutSet1[Int32, _];
        let s2 = MutSet1.emptyWithRegion(r): MutSet1[Int32, _];
        MutSet1.sameElements(s, s2)
    }

    @test
    def sameElements02(): Bool = region r {
        let s = MutSet1.emptyWithRegion(r);
        MutSet1.add!(1, s);

        let s2 = MutSet1.emptyWithRegion(r);
        MutSet1.add!(1, s2);

        MutSet1.sameElements(s, s2)
    }

    @test
    def sameElements03(): Bool = region r {
        let s = MutSet1.emptyWithRegion(r);
        MutSet1.add!(1, s);

        let s2 = MutSet1.emptyWithRegion(r);
        MutSet1.add!(2, s2);

        not MutSet1.sameElements(s, s2)
    }

    @test
    def sameElements04(): Bool = region r {
        let s = MutSet1.emptyWithRegion(r);
        MutSet1.add!(1, s);
        MutSet1.add!(2, s);
        MutSet1.add!(3, s);

        let s2 = MutSet1.emptyWithRegion(r);
        MutSet1.add!(2, s2);
        MutSet1.add!(3, s2);
        MutSet1.add!(1, s2);

        MutSet1.sameElements(s, s2)
    }

    @test
    def sameElements05(): Bool = region r {
        let s = MutSet1.emptyWithRegion(r);
        MutSet1.add!("a", s);
        MutSet1.add!("b", s);
        MutSet1.add!("c", s);

        let s2 = MutSet1.emptyWithRegion(r);
        MutSet1.add!("c", s2);
        MutSet1.add!("a", s2);
        MutSet1.add!("b", s2);

        MutSet1.sameElements(s, s2)
    }

    @test
    def setSameElements06(): Bool = region r {
        let s = MutSet1.emptyWithRegion(r);
        MutSet1.add!("a", s);
        MutSet1.add!("b", s);
        MutSet1.add!("c", s);

        let s2 = MutSet1.emptyWithRegion(r);
        MutSet1.add!("c", s2);
        MutSet1.add!("a", s2);

        not MutSet1.sameElements(s, s2)
    }


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sum01(): Bool = region r {
        MutSet1.emptyWithRegion(r) |> MutSet1.sum == 0
    }

    @test
    def sum02(): Bool = region r {
        let s = MutSet1.emptyWithRegion(r);
        MutSet1.add!(1, s);
        MutSet1.sum(s) == 1
    }

    @test
    def sum03(): Bool = region r {
        let s = MutSet1.emptyWithRegion(r);
        MutSet1.add!(1, s);
        MutSet1.add!(2, s);
        MutSet1.add!(3, s);
        MutSet1.sum(s) == 6
    }

    @test
    def sum04(): Bool = region r {
        let s = MutSet1.emptyWithRegion(r);
        MutSet1.add!(1, s);
        MutSet1.add!(2, s);
        MutSet1.add!(3, s);
        MutSet1.add!(-3, s);
        MutSet1.sum(s) == 3
    }

    @test
    def sum05(): Bool = region r {
        let s = MutSet1.emptyWithRegion(r);
        MutSet1.add!(1, s);
        MutSet1.add!(2, s);
        MutSet1.add!(-3, s);
        MutSet1.add!(-4, s);
        MutSet1.sum(s) == -4
    }

    @test
    def sum06(): Bool = region r {
        let s = MutSet1.emptyWithRegion(r);
        MutSet1.add!(10, s);
        MutSet1.add!(-10, s);
        MutSet1.sum(s) == 0
    }


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumWith01(): Bool = region r {
        MutSet1.emptyWithRegion(r) |> MutSet1.sumWith(x -> x + 1) == 0
    }

    @test
    def sumWith02(): Bool = region r {
        let s = MutSet1.emptyWithRegion(r);
        MutSet1.add!(1, s);
        MutSet1.sumWith(x -> x + 1, s) == 2
    }

    @test
    def sumWith03(): Bool = region r {
        let s = MutSet1.emptyWithRegion(r);
        MutSet1.add!(1, s);
        MutSet1.add!(2, s);
        MutSet1.add!(3, s);
        MutSet1.sumWith(x -> x + 1, s) == 9
    }

    @test
    def sumWith04(): Bool = region r {
        let s = MutSet1.emptyWithRegion(r);
        MutSet1.add!(1, s);
        MutSet1.add!(2, s);
        MutSet1.add!(3, s);
        MutSet1.add!(-3, s);
        MutSet1.sumWith(x -> x + 1, s) == 7
    }

    @test
    def sumWith05(): Bool = region r {
        let s = MutSet1.emptyWithRegion(r);
        MutSet1.add!(-1, s);
        MutSet1.add!(-2, s);
        MutSet1.add!(-3, s);
        MutSet1.add!(-4, s);
        MutSet1.sumWith(x -> x + 1, s) == -6
    }

    @test
    def sumWith06(): Bool = region r {
        let s = MutSet1.emptyWithRegion(r);
        MutSet1.add!(10, s);
        MutSet1.add!(-10, s);
        MutSet1.sumWith(x -> x + 1, s) == 2
    }


    /////////////////////////////////////////////////////////////////////////////
    // product                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def product01(): Bool = region r {
        MutSet1.product(MutSet1.emptyWithRegion(r)) == 0
    }

    @test
    def product02(): Bool = region r {
        let s = MutSet1.emptyWithRegion(r);
        MutSet1.add!(1, s);
        MutSet1.product(s) == 1
    }

    @test
    def product03(): Bool = region r {
        let s = MutSet1.emptyWithRegion(r);
        MutSet1.add!(1, s);
        MutSet1.add!(2, s);
        MutSet1.add!(3, s);
        MutSet1.product(s) == 6
    }

    @test
    def product04(): Bool = region r {
        let s = MutSet1.emptyWithRegion(r);
        MutSet1.add!(1, s);
        MutSet1.add!(2, s);
        MutSet1.add!(3, s);
        MutSet1.add!(-3, s);
        MutSet1.product(s) == -18
    }

    @test
    def product05(): Bool = region r {
        let s = MutSet1.emptyWithRegion(r);
        MutSet1.add!(-1, s);
        MutSet1.add!(-2, s);
        MutSet1.add!(-3, s);
        MutSet1.add!(-4, s);
        MutSet1.product(s) == 24
    }

    @test
    def product06(): Bool = region r {
        let s = MutSet1.emptyWithRegion(r);
        MutSet1.add!(10, s);
        MutSet1.add!(-10, s);
        MutSet1.product(s) == -100
    }


    /////////////////////////////////////////////////////////////////////////////
    // productWith                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productWith01(): Bool = region r {
        MutSet1.emptyWithRegion(r) |> MutSet1.productWith(x -> x + 1) == 0
    }

    @test
    def productWith02(): Bool = region r {
        let s = MutSet1.emptyWithRegion(r);
        MutSet1.add!(1, s);
        MutSet1.productWith(x -> x + 1, s) == 2
    }

    @test
    def productWith03(): Bool = region r {
        let s = MutSet1.emptyWithRegion(r);
        MutSet1.add!(1, s);
        MutSet1.add!(2, s);
        MutSet1.add!(3, s);
        MutSet1.productWith(x -> x + 1, s) == 24
    }

    @test
    def productWith04(): Bool = region r {
        let s = MutSet1.emptyWithRegion(r);
        MutSet1.add!(1, s);
        MutSet1.add!(2, s);
        MutSet1.add!(3, s);
        MutSet1.add!(-3, s);
        MutSet1.productWith(x -> x + 1, s) == -48
    }

    @test
    def productWith05(): Bool = region r {
        let s = MutSet1.emptyWithRegion(r);
        MutSet1.add!(-2, s);
        MutSet1.add!(-3, s);
        MutSet1.add!(-4, s);
        MutSet1.add!(-5, s);
        MutSet1.productWith(x -> x + 1, s) == 24
    }

    @test
    def productWith06(): Bool = region r {
        let s = MutSet1.emptyWithRegion(r);
        MutSet1.add!(10, s);
        MutSet1.add!(-10, s);
        MutSet1.productWith(x -> x + 1, s) == -99
    }


    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def join01(): Bool = region r {
        let s: MutSet1[Int32, _] = MutSet1.emptyWithRegion(r);
        MutSet1.join(",", s) == ""
    }

    @test
    def join02(): Bool = region r {
        let s = MutSet1.emptyWithRegion(r);
        MutSet1.add!(1, s);
        MutSet1.join(",", s) == "1"
    }

    @test
    def join03(): Bool = region r {
        let s = MutSet1.emptyWithRegion(r);
        MutSet1.add!(1, s);
        MutSet1.add!(2, s);
        MutSet1.add!(3, s);
        MutSet1.join(",", s) == "1,2,3"
    }

    @test
    def join04(): Bool = region r {
        let s = MutSet1.emptyWithRegion(r);
        MutSet1.add!("1", s);
        MutSet1.add!("2", s);
        MutSet1.add!("3", s);
        MutSet1.join(",", s) == "1,2,3"
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldRightLazy                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRightLazy01(): Bool = region r {
        MutSet1.foldRightLazy((e, i) -> (force i - e) * (e rem 2 + 1), 100, MutSet1.emptyWithRegion(r)) == 100
    }

    @test
    def foldRightLazy02(): Bool = region r {
        let s = MutSet1.emptyWithRegion(r);
        MutSet1.add!(1, s);
        MutSet1.foldRightLazy((e, i) -> (force i - e) * (e rem 2 + 1), 100, s) == 198
    }

    @test
    def foldRightLazy03(): Bool = region r {
        let s = MutSet1.emptyWithRegion(r);
        MutSet1.add!(2, s);
        MutSet1.add!(1, s);
        MutSet1.foldRightLazy((e, i) -> (force i - e) * (e rem 2 + 1), 100, s) == 194
    }

    @test
    def foldRightLazy04(): Bool = region r {
        let s = MutSet1.emptyWithRegion(r);
        MutSet1.add!(3, s);
        MutSet1.add!(2, s);
        MutSet1.add!(1, s);
        MutSet1.foldRightLazy((e, i) -> (force i - e) * (e rem 2 + 1), 100, s) == 382
    }

}
