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



/*

use LazyListOld.LazyList.{Empty, LazyCons};

namespace TestLazyListOld {

    /////////////////////////////////////////////////////////////////////////////
    // repeat                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def repeat01(): Bool =
        let xs = LazyListOld.take(1, LazyListOld.repeat(1));
        LazyListOld.length(xs) == 1 and LazyListOld.head(xs) == Some(1)

    @test
    def repeat02(): Bool =
        let xs = LazyListOld.take(1, LazyListOld.repeat('a'));
        LazyListOld.length(xs) == 1 and LazyListOld.head(xs) == Some('a')

    @test
    def repeat03(): Bool =
        let xs = LazyListOld.take(1, LazyListOld.repeat("Hello, World!"));
        LazyListOld.length(xs) == 1 and LazyListOld.head(xs) == Some("Hello, World!")

    @test
    def repeat04(): Bool =
        let xs = LazyListOld.take(2, LazyListOld.repeat(42));
        LazyListOld.length(xs) == 2 and LazyListOld.head(xs) == Some(42) and LazyListOld.last(xs) == Some(42)

    @test
    def repeat05(): Bool = {
        let xs = LazyListOld.take(0, LazyListOld.repeat(42));
        LazyListOld.isEmpty(xs)
    }

    /////////////////////////////////////////////////////////////////////////////
    // from                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def from01(): Bool = {
        let xs = LazyListOld.take(0, LazyListOld.from(0));
        LazyListOld.isEmpty(xs)
    }

    @test
    def from02(): Bool =
        let xs = LazyListOld.take(1, LazyListOld.from(0));
        LazyListOld.length(xs) == 1 and LazyListOld.head(xs) == Some(0)

    @test
    def from03(): Bool =
        let xs = LazyListOld.take(2, LazyListOld.from(107));
        LazyListOld.length(xs) == 2 and LazyListOld.head(xs) == Some(107) and LazyListOld.last(xs) == Some(108)

    @test
    def from04(): Bool = {
        let xs = LazyListOld.take(3, LazyListOld.from(42));
        LazyListOld.nth(0, xs) == Some(42) and LazyListOld.nth(1, xs) == Some(43) and LazyListOld.nth(2, xs) == Some(44)
    }

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isEmpty01(): Bool =
        LazyListOld.isEmpty(LazyListOld.empty())

    @test
    def isEmpty02(): Bool =
        LazyListOld.isEmpty(LazyListOld.take(0, LazyListOld.from(5)))

    @test
    def isEmpty03(): Bool =
        LazyListOld.isEmpty(LazyListOld.take(3, LazyListOld.empty()))

    @test
    def isEmpty04(): Bool =
        not LazyListOld.isEmpty(LazyListOld.from(1))

    @test
    def isEmpty05(): Bool =
        not LazyListOld.isEmpty(LazyListOld.repeat(10))

    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def length01(): Bool =
        LazyListOld.length(LazyListOld.empty()) == 0

    @test
    def length02(): Bool =
        LazyListOld.length(LazyListOld.take(1, LazyListOld.from(0))) == 1

    @test
    def length03(): Bool =
        LazyListOld.length(LazyListOld.take(2, LazyListOld.from(3))) == 2

    @test
    def length04(): Bool =
        LazyListOld.length(LazyListOld.take(10, LazyListOld.repeat(true))) == 10

    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def head01(): Bool =
        LazyListOld.head(LazyListOld.empty(): LazyListOld.LazyList[Unit]) == None

    @test
    def head02(): Bool =
        LazyListOld.head(LazyCons(1, lazy Empty)) == Some(1)

    @test
    def head03(): Bool =
        LazyListOld.head(LazyCons(42, lazy Empty)) == Some(42)

    @test
    def head04(): Bool =
        LazyListOld.head(LazyCons(42, lazy LazyCons(43, lazy Empty))) == Some(42)

    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def last01(): Bool =
        LazyListOld.last(LazyListOld.empty(): LazyListOld.LazyList[Unit]) == None

    @test
    def last02(): Bool =
        LazyListOld.last(LazyCons(1, lazy LazyListOld.empty())) == Some(1)

    @test
    def last03(): Bool =
        LazyListOld.last(LazyCons(42, lazy LazyCons(43, lazy Empty))) == Some(43)

    @test
    def last04(): Bool =
        LazyListOld.last(LazyCons('a', lazy LazyCons('b', lazy Empty))) == Some('b')

    /////////////////////////////////////////////////////////////////////////////
    // nth                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def nth01(): Bool = LazyListOld.nth(0, LazyListOld.empty(): LazyListOld.LazyList[Unit]) == None

    @test
    def nth02(): Bool = LazyListOld.nth(1, LazyListOld.empty(): LazyListOld.LazyList[Unit]) == None

    @test
    def nth03(): Bool = {
        let xs = LazyListOld.take(10, LazyListOld.from(1));
        LazyListOld.nth(0, xs) == Some(1)
    }

    @test
    def nth04(): Bool = {
        let xs = LazyListOld.take(10, LazyListOld.from(1));
        LazyListOld.nth(5, xs) == Some(6)
    }

    @test
    def nth05(): Bool = {
        let xs = LazyListOld.take(10, LazyListOld.from(1));
        LazyListOld.nth(9, xs) == Some(10)
    }

    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def append01(): Bool =
        LazyListOld.isEmpty(LazyListOld.append(LazyListOld.empty(), LazyListOld.empty()))

    @test
    def append02(): Bool = {
        let xs = LazyListOld.take(4, LazyListOld.from(1));
        let ys = LazyListOld.append(xs, LazyListOld.empty());
        LazyListOld.last(xs) == LazyListOld.last(ys)
    }

    @test
    def append03(): Bool = {
        let xs = LazyListOld.take(4, LazyListOld.from(1));
        let ys = LazyListOld.append(LazyListOld.empty(), xs);
        LazyListOld.last(xs) == LazyListOld.last(ys)
    }

    @test
    def append04(): Bool = {
        let xs = LazyListOld.take(4, LazyListOld.from(1));
        let ys = LazyListOld.take(9, LazyListOld.from(5));
        let zs = LazyListOld.append(xs, ys);
        LazyListOld.head(zs) == Some(1) and LazyListOld.last(zs) == Some(13)
    }

    @test
    def append05(): Bool = {
        let xs = LazyListOld.append(LazyCons(42, lazy Empty), LazyCons(107, lazy Empty));
        LazyListOld.foldLeft((acc, x) -> acc + x, 0, xs) == 149
    }

    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reverse01(): Bool =
        LazyListOld.isEmpty(LazyListOld.reverse(LazyListOld.empty()))

    @test
    def reverse02(): Bool = {
        let xs = LazyListOld.reverse(LazyCons(42, lazy Empty));
        LazyListOld.head(xs) == Some(42)
    }

    @test
    def reverse03(): Bool = {
        let xs = LazyListOld.reverse(LazyCons(1, lazy LazyCons(2, lazy Empty)));
        LazyListOld.head(xs) == Some(2) and LazyListOld.last(xs) == Some(1)
    }

    @test
    def reverse04(): Bool = {
        let xs = LazyListOld.reverse(LazyListOld.take(7, LazyListOld.from(0)));
        LazyListOld.head(xs) == Some(6) and LazyListOld.last(xs) == Some(0)
    }

    @test
    def reverse05(): Bool = {
        let xs = LazyListOld.reverse(LazyListOld.unfold(x -> if (x == 42) None else Some((x - 1, x - 1)), 55));
        LazyListOld.head(xs) == Some(42) and LazyListOld.last(xs) == Some(54)
    }

    /////////////////////////////////////////////////////////////////////////////
    // take                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def take01(): Bool =
        LazyListOld.isEmpty(LazyListOld.take(0, LazyListOld.take(5, LazyListOld.from(10))))

    @test
    def take02(): Bool =
        LazyListOld.isEmpty(LazyListOld.take(0, LazyListOld.repeat(4)))

    @test
    def take03(): Bool = {
        let xs = LazyListOld.take(1, LazyListOld.repeat(7));
        LazyListOld.length(xs) == 1 and LazyListOld.head(xs) == Some(7)
    }

    @test
    def take04(): Bool = {
        let xs = LazyListOld.take(2, LazyListOld.from(42));
        LazyListOld.length(xs) == 2 and LazyListOld.last(xs) == Some(43)
    }

    /////////////////////////////////////////////////////////////////////////////
    // drop                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def drop01(): Bool =
        LazyListOld.isEmpty(LazyListOld.drop(0, LazyListOld.empty()))

    @test
    def drop02(): Bool =
        LazyListOld.isEmpty(LazyListOld.drop(42, LazyListOld.empty()))

    @test
    def drop03(): Bool =
        LazyListOld.length(LazyListOld.drop(7, LazyListOld.take(10, LazyListOld.repeat(3)))) == 3

    @test
    def drop04(): Bool = {
        let xs = LazyListOld.drop(9, LazyListOld.take(10, LazyListOld.repeat(3)));
        LazyListOld.length(xs) == 1 and LazyListOld.head(xs) == Some(3)
    }

    @test
    def drop05(): Bool = {
        let xs = LazyListOld.drop(1, LazyListOld.take(5, LazyListOld.repeat(3)));
        LazyListOld.length(xs) == 4 and LazyListOld.head(xs) == Some(3)
    }

    /////////////////////////////////////////////////////////////////////////////
    // tail                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tail01(): Bool =
        LazyListOld.isEmpty(LazyListOld.tail(LazyListOld.empty()))

    @test
    def tail02(): Bool =
        LazyListOld.isEmpty(LazyListOld.tail(LazyCons(42, lazy Empty)))

    @test
    def tail03(): Bool = {
        let xs = LazyListOld.tail(LazyListOld.take(4, LazyListOld.from(10)));
        LazyListOld.length(xs) == 3 and LazyListOld.head(xs) == Some(11)
    }

    @test
    def tail04(): Bool = {
        let xs = LazyListOld.tail(LazyListOld.take(10, LazyListOld.from(0)));
        LazyListOld.length(xs) == 9 and LazyListOld.head(xs) == Some(1)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toList01(): Bool =
        LazyListOld.toList(LazyListOld.empty(): LazyListOld.LazyList[Unit]) == Nil

    @test
    def toList02(): Bool =
        LazyListOld.toList(LazyCons(42, lazy Empty)) == 42 :: Nil

    @test
    def toList03(): Bool = {
        let xs = LazyListOld.unfold(x -> if (x == 50) None else Some((x - 1, x - 1)), 55);
        LazyListOld.toList(xs) == 54 :: 53 :: 52 :: 51 :: 50 :: Nil
    }

    @test
    def toList04(): Bool = {
        let xs = LazyListOld.unfold(s -> if (s >= 10) None else Some(Char.fromInt32(s + 48), s + 1), 0);
        LazyListOld.toList(xs) == '0' :: '1' :: '2' :: '3' :: '4' :: '5' :: '6' :: '7' :: '8' :: '9' :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool =
        LazyListOld.foldLeft((acc, x) -> acc + x, 0, LazyListOld.take(4, LazyListOld.repeat(1))) == 4

    @test
    def foldLeft02(): Bool =
        LazyListOld.foldLeft((acc, x) -> acc + x, "", LazyListOld.take(3, LazyListOld.repeat("z"))) == "zzz"

    @test
    def foldLeft03(): Bool =
        LazyListOld.foldLeft((acc, x) -> acc - x, 0, LazyListOld.take(3, LazyListOld.from(10))) == -33

    @test
    def foldLeft04(): Bool =
        LazyListOld.foldLeft((acc, x) -> acc * x, 1, LazyListOld.take(5, LazyListOld.from(1))) == 120

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool =
        LazyListOld.foldRight((x, acc) -> x + (force acc), 0, LazyListOld.take(4, LazyListOld.repeat(1))) == 4

    @test
    def foldRight02(): Bool =
        not LazyListOld.foldRight((x, acc) -> x and (force acc), true, LazyListOld.repeat(false))

    @test
    def foldRight03(): Bool =
        LazyListOld.foldRight((x, acc) -> x or (force acc), false, LazyListOld.repeat(true))

    @test
    def foldRight04(): Bool =
        LazyListOld.foldRight((x, acc) -> x - (force acc), 0, LazyListOld.take(10, LazyListOld.from(0))) == -5

    @test
    def foldRight05(): Bool =
        LazyListOld.foldRight((x, acc) -> (force acc) - x, 0, LazyListOld.take(10, LazyListOld.from(0))) == -45

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def exists01(): Bool =
        not LazyListOld.exists(x -> x == 0, LazyListOld.empty())

    @test
    def exists02(): Bool =
        LazyListOld.exists(x -> x == 0, LazyCons(0, lazy Empty))

    @test
    def exists03(): Bool =
        LazyListOld.exists(x -> x == 42, LazyListOld.from(42))

    @test
    def exists04(): Bool =
        LazyListOld.exists(x -> x == 10, LazyListOld.repeat(10))

    /////////////////////////////////////////////////////////////////////////////
    // forall                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forall01(): Bool =
        not LazyListOld.forall(x -> x > 2, LazyListOld.repeat(1))

    @test
    def forall02(): Bool =
        not LazyListOld.forall(x -> x == 9, LazyListOld.from(10))

    @test
    def forall03(): Bool =
        LazyListOld.forall(x -> x >= 7, LazyListOld.take(10, LazyListOld.from(7)))

    @test
    def forall04(): Bool =
        LazyListOld.forall(x -> x == 42, LazyListOld.take(5, LazyListOld.repeat(42)))

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map01(): Bool = {
        let xs = LazyListOld.map(x -> x * 2, LazyListOld.take(10, LazyListOld.from(0)));
        LazyListOld.length(xs) == 10 and LazyListOld.nth(9, xs) == Some(18)
    }

    @test
    def map02(): Bool = {
        let xs = LazyListOld.map(x -> x + 1, LazyCons(1, lazy Empty));
        LazyListOld.head(xs) == Some(2)
    }

    @test
    def map03(): Bool = {
        let xs = LazyListOld.take(2, LazyListOld.map(x -> x + "z", LazyListOld.repeat("x")));
        LazyListOld.head(xs) == Some("xz")
    }

    @test
    def map04(): Bool = {
        let xs = LazyListOld.map(x -> x + 3, LazyListOld.take(3, LazyListOld.from(1)));
        LazyListOld.last(xs) == Some(6)
    }

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filter01(): Bool = {
        let xs = LazyListOld.filter(x -> x >= 2, LazyListOld.take(10, LazyListOld.from(0)));
        LazyListOld.head(xs) == Some(2) and LazyListOld.last(xs) == Some(9)
    }

    @test
    def filter02(): Bool = {
        let xs = LazyListOld.take(3, LazyListOld.filter(x -> x % 2 == 0, LazyListOld.from(0)));
        LazyListOld.last(xs) == Some(4)
    }

    @test
    def filter03(): Bool = {
        let xs = LazyListOld.filter(x -> x == '.', LazyListOld.take(5, LazyListOld.repeat(':')));
        LazyListOld.isEmpty(xs)
    }

    @test
    def filter04(): Bool = {
        let xs = LazyListOld.filter(x -> x > 14, LazyListOld.take(5, LazyListOld.from(10)));
        LazyListOld.isEmpty(xs)
    }

    /////////////////////////////////////////////////////////////////////////////
    // unfold                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unfold01(): Bool = {
        let xs = LazyListOld.unfold(x -> if (x == 5) None else Some((x + 1, x + 1)), 0);
        LazyListOld.last(xs) == Some(5)
    }

    @test
    def unfold02(): Bool = {
        let xs = LazyListOld.unfold(x -> if (x == 100) None else Some((Char.fromInt32(x), x + 1)), 97);
        LazyListOld.head(xs) == Some('a') and LazyListOld.last(xs) == Some('c')
    }

    @test
    def unfold03(): Bool = {
        let xs = LazyListOld.unfold(x -> if (x == 0) None else Some((x - 1, x - 1)), 10);
        LazyListOld.head(xs) == Some(9)
    }

    /////////////////////////////////////////////////////////////////////////////
    // zipWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWith01(): Bool = {
        let xs = LazyListOld.take(3, LazyListOld.repeat(1));
        let ys = LazyListOld.zipWith((x, y) -> (x + 1, y + 1), xs, xs);
        LazyListOld.length(ys) == 3 and LazyListOld.head(ys) == Some((2, 2))
    }

    @test
    def zipWith02(): Bool = {
        let xs = LazyListOld.take(3, LazyListOld.repeat(1));
        let ys = LazyListOld.zipWith((x, y) -> (x + 1, y + 1), xs, LazyListOld.empty());
        LazyListOld.isEmpty(ys)
    }

    @test
    def zipWith03(): Bool = {
        let xs = LazyListOld.take(3, LazyListOld.repeat(1));
        let ys = LazyListOld.zipWith((x, y) -> (x + 1, y + 1), LazyListOld.empty(), xs);
        LazyListOld.isEmpty(ys)
    }

    @test
    def zipWith04(): Bool = {
        let xs = LazyListOld.take(4, LazyListOld.from(5));
        let ys = LazyListOld.take(4, LazyListOld.from(11));
        let zs = LazyListOld.zipWith((x, y) -> (x + 1, y - 1), xs, ys);
        LazyListOld.head(zs) == Some((6, 10)) and LazyListOld.last(zs) == Some((9, 13))
    }

    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zip01(): Bool = {
        let xs = LazyListOld.take(2, LazyListOld.zip(LazyListOld.repeat(4), LazyListOld.repeat(7)));
        LazyListOld.head(xs) == Some((4, 7))
    }

    @test
    def zip02(): Bool = {
        let xs = LazyListOld.take(1, LazyListOld.zip(LazyListOld.repeat('a'), LazyListOld.repeat('b')));
        LazyListOld.head(xs) == Some(('a', 'b'))
    }

    @test
    def zip03(): Bool = {
        let xs = LazyListOld.take(3, LazyListOld.repeat(1));
        let ys = LazyListOld.zip(xs, LazyListOld.empty());
        LazyListOld.isEmpty(ys)
    }

    @test
    def zip04(): Bool = {
        let xs = LazyListOld.take(3, LazyListOld.repeat(1));
        let ys = LazyListOld.zip(LazyListOld.empty(), xs);
        LazyListOld.isEmpty(ys)
    }

}

*/
