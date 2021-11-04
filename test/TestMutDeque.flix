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

namespace TestMutDeque {

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def size01(): Bool & Impure =
        MutDeque1.size(MutDeque1.new()) == 0

    @test
    def size02(): Bool & Impure =
        let d = MutDeque1.new();

        MutDeque1.pushBack(1, d);
        MutDeque1.pushBack(1, d);
        MutDeque1.pushBack(1, d);
        MutDeque1.pushBack(1, d);

        MutDeque1.size(d) == 4

    @test
    def size03(): Bool & Impure =
        let d = MutDeque1.new();

        MutDeque1.pushBack(1, d);
        MutDeque1.pushBack(1, d);
        MutDeque1.pushBack(1, d);
        MutDeque1.pushBack(1, d);

        MutDeque1.pushBack(1, d);
        MutDeque1.pushBack(1, d);
        MutDeque1.pushBack(1, d);
        MutDeque1.pushBack(1, d);

        MutDeque1.size(d) == 8

    @test
    def size04(): Bool & Impure =
        let d = MutDeque1.new();

        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);

        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);

        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);

        MutDeque1.size(d) == 11

    @test
    def size05(): Bool & Impure =
        let d = MutDeque1.new();

        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);

        MutDeque1.popFront(d);
        MutDeque1.popFront(d);

        MutDeque1.size(d) == 2


    @test
    def size06(): Bool & Impure =
        let d = MutDeque1.new();

        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);

        MutDeque1.popFront(d);
        MutDeque1.popFront(d);
        MutDeque1.popFront(d);
        MutDeque1.popFront(d);

        MutDeque1.size(d) == 0

    @test
    def size07(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushFront(1, d);
        MutDeque1.popBack(d);
        MutDeque1.size(d) == 0

    @test
    def size08(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(1, d);
        MutDeque1.popFront(d);
        MutDeque1.size(d) == 0


    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isEmpty01(): Bool & Impure =
        MutDeque1.isEmpty(MutDeque1.new())

    @test
    def isEmpty02(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(1, d);
        not (MutDeque1.isEmpty(d))

    @test
    def isEmpty03(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(1, d);
        MutDeque1.popBack(d);
        MutDeque1.isEmpty(d)

    @test
    def isEmpty04(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushFront(1, d);
        not (MutDeque1.isEmpty(d))

    @test
    def isEmpty05(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushFront(1, d);
        MutDeque1.popFront(d);
        MutDeque1.isEmpty(d)


    /////////////////////////////////////////////////////////////////////////////
    // popFront                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def popFront01(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.popFront(d) == None

    @test
    def popFront02(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushFront(1, d);
        MutDeque1.popFront(d) == Some(1)

    @test
    def popFront03(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(2, d);
        MutDeque1.pushFront(3, d);
        MutDeque1.popFront(d) == Some(3)     and
            MutDeque1.popFront(d) == Some(2) and
            MutDeque1.popFront(d) == Some(1) and
            MutDeque1.popFront(d) == None

    @test
    def popFront04(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(1, d);
        MutDeque1.pushBack(2, d);
        MutDeque1.pushBack(3, d);
        MutDeque1.popFront(d) == Some(1)     and
            MutDeque1.popFront(d) == Some(2) and
            MutDeque1.popFront(d) == Some(3) and
            MutDeque1.popFront(d) == None

    @test
    def popFront05(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushFront(1, d);
        MutDeque1.pushBack(2, d);
        MutDeque1.popFront(d) == Some(1)     and
            MutDeque1.popFront(d) == Some(2) and
            MutDeque1.popFront(d) == None

    @test
    def popFront06(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(1, d);
        MutDeque1.pushFront(2, d);
        MutDeque1.popFront(d) == Some(2)     and
            MutDeque1.popFront(d) == Some(1) and
            MutDeque1.popFront(d) == None


    /////////////////////////////////////////////////////////////////////////////
    // popBack                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def popBack01(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.popBack(d) == None

    @test
    def popBack02(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(1, d);
        MutDeque1.popBack(d) == Some(1)

    @test
    def popBack03(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(1, d);
        MutDeque1.pushBack(2, d);
        MutDeque1.pushBack(3, d);
        MutDeque1.popBack(d) == Some(3)     and
            MutDeque1.popBack(d) == Some(2) and
            MutDeque1.popBack(d) == Some(1) and
            MutDeque1.popBack(d) == None

    @test
    def popBack04(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(2, d);
        MutDeque1.pushFront(3, d);
        MutDeque1.popBack(d) == Some(1)     and
            MutDeque1.popBack(d) == Some(2) and
            MutDeque1.popBack(d) == Some(3) and
            MutDeque1.popBack(d) == None

    @test
    def popBack05(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushBack(1, d);
        MutDeque1.pushFront(2, d);
        MutDeque1.popBack(d) == Some(1)     and
            MutDeque1.popBack(d) == Some(2) and
            MutDeque1.popBack(d) == None

    @test
    def popBack06(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushFront(1, d);
        MutDeque1.pushBack(2, d);
        MutDeque1.popBack(d) == Some(2)     and
            MutDeque1.popBack(d) == Some(1) and
            MutDeque1.popBack(d) == None


    /////////////////////////////////////////////////////////////////////////////
    // pushFrontPushBack (expansion)                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def pushFrontPushBack01(): Bool & Impure =
        let d = MutDeque1.new();
        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.pushBack(2, d);
        MutDeque1.pushBack(2, d);
        MutDeque1.pushBack(2, d);
        MutDeque1.pushBack(2, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.pushBack(2, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.pushBack(2, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.pushBack(2, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.pushFront(1, d);
        MutDeque1.pushBack(2, d);
        MutDeque1.pushBack(2, d);
        MutDeque1.pushBack(2, d);
        // [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2]

        MutDeque1.popBack(d) == Some(2) and
        MutDeque1.popFront(d) == Some(1) and
        MutDeque1.popBack(d) == Some(2) and
        MutDeque1.popBack(d) == Some(2) and
        MutDeque1.popBack(d) == Some(2) and
        MutDeque1.popBack(d) == Some(2) and
        MutDeque1.popBack(d) == Some(2) and
        MutDeque1.popBack(d) == Some(2) and
        MutDeque1.popBack(d) == Some(2) and
        MutDeque1.popBack(d) == Some(2) and
        MutDeque1.popBack(d) == Some(2) and
        MutDeque1.popBack(d) == Some(1) and
        MutDeque1.popBack(d) == Some(1) and
        MutDeque1.popBack(d) == Some(1) and
        MutDeque1.popBack(d) == Some(1) and
        MutDeque1.popBack(d) == Some(1) and
        MutDeque1.popBack(d) == Some(1) and
        MutDeque1.popBack(d) == Some(1) and
        MutDeque1.popBack(d) == Some(1) and
        MutDeque1.popBack(d) == Some(1) and
        MutDeque1.popBack(d) == None and
        MutDeque1.popFront(d) == None

}
