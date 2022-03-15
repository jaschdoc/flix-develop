/*
 * Copyright 2020 Magnus Madsen, Stephen Tetley
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

namespace TestValidation {

    use Hash.hash;
    use Nec.{cons, singleton};

    def necOf2(x: a, y: a): Nec[a] = cons(x, singleton(y))

    def necOf3(w: a, x: a, y: a): Nec[a] = cons(w, cons(x, singleton(y)))

    def necOf4(v: a, w: a, x: a, y: a): Nec[a] = cons(v, cons(w, cons(x, singleton(y))))

    def necOf5(u: a, v: a, w: a, x: a, y: a): Nec[a] = cons(u, cons(v, cons(w, cons(x, singleton(y)))))

    def necOf6(t: a, u: a, v: a, w: a, x: a, y: a): Nec[a] = cons(t, cons(u, cons(v, cons(w, cons(x, singleton(y))))))

    def necOf7(s: a, t: a, u: a, v: a, w: a, x: a, y: a): Nec[a] = cons(s, cons(t, cons(u, cons(v, cons(w, cons(x, singleton(y)))))))

    def necOf8(r: a, s: a, t: a, u: a, v: a, w: a, x: a, y: a): Nec[a] = cons(r, cons(s, cons(t, cons(u, cons(v, cons(w, cons(x, singleton(y))))))))

    def necOf9(q: a, r: a, s: a, t: a, u: a, v: a, w: a, x: a, y: a): Nec[a] = cons(q, cons(r, cons(s, cons(t, cons(u, cons(v, cons(w, cons(x, singleton(y)))))))))

    def necOf10(p: a, q: a, r: a, s: a, t: a, u: a, v: a, w: a, x: a, y: a): Nec[a] = cons(p, cons(q, cons(r, cons(s, cons(t, cons(u, cons(v, cons(w, cons(x, singleton(y))))))))))

    /////////////////////////////////////////////////////////////////////////////
    // ap                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def ap01(): Bool = VValidation.ap(Success(x -> x + 1), Success(123)): Validation[_, Unit] == Success(124)

    @test
    def ap02(): Bool = VValidation.ap(Success(x -> x + 1), Failure(singleton(42))) == Failure(singleton(42))

    @test
    def ap03(): Bool = VValidation.ap(Failure(singleton(42)), Success(123)): Validation[Unit, _] == Failure(singleton(42))

    @test
    def ap04(): Bool = VValidation.ap(Failure(singleton(123)), (Failure(singleton(456)))): Validation[Unit, _] == Failure(necOf2(123, 456))

    @test
    def ap05(): Bool = VValidation.ap(Failure(necOf2(1, 2)), (Failure(singleton(3)))): Validation[Unit, _] == Failure(necOf3(1, 2, 3))

    @test
    def ap06(): Bool = VValidation.ap(Failure(singleton(1)), (Failure(necOf2(2, 3)))): Validation[Unit, _] == Failure(necOf3(1, 2, 3))

    @test
    def ap07(): Bool = VValidation.ap(Failure(necOf2(1, 2)), (Failure(necOf2(3, 4)))): Validation[Unit, _] == Failure(necOf4(1, 2, 3, 4))

    /////////////////////////////////////////////////////////////////////////////
    // getWithDefault                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def getWithDefault01(): Bool = VValidation.getWithDefault(456, Success(123)) == 123

    @test
    def getWithDefault02(): Bool = VValidation.getWithDefault(456, Failure(singleton(42))) == 456

    /////////////////////////////////////////////////////////////////////////////
    // withDefault                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def withDefault01(): Bool = VValidation.withDefault(default = Success(2), Success(1)): Validation[_, Unit] == Success(1)

    @test
    def withDefault02(): Bool = VValidation.withDefault(default = Failure(singleton(1)), Success(1)) == Success(1)

    @test
    def withDefault03(): Bool = VValidation.withDefault(default = Success(2), Failure(singleton(1))) == Success(2)

    @test
    def withDefault04(): Bool = VValidation.withDefault(default = Failure(singleton(2)), Failure(singleton(1))): Validation[Unit, _] == Failure(singleton(2))

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def exists01(): Bool = VValidation.exists(x -> x == 123, (Success(123))) == true

    @test
    def exists02(): Bool = VValidation.exists(x -> x != 123, (Success(123))) == false

    /////////////////////////////////////////////////////////////////////////////
    // forall                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forall01(): Bool = VValidation.exists(x -> x == 123, (Success(123))) == true

    @test
    def forall02(): Bool = VValidation.exists(x -> x != 123, (Success(123))) == false

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map01(): Bool = VValidation.map(x -> x, Success(123)): Validation[_, Unit] == Success(123)

    @test
    def map02(): Bool = VValidation.map(x -> x + 1, Success(123)): Validation[_, Unit] == Success(124)

    @test
    def map03(): Bool = VValidation.map(x -> x, Failure(singleton(123))): Validation[Unit, _] == Failure(singleton(123))

    @test
    def map04(): Bool = (Success(123): Validation[_, Unit]) |> VValidation.map(x -> x + 1) |> VValidation.map(x -> x * 2)  == Success(248)

    /////////////////////////////////////////////////////////////////////////////
    // sequence                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sequence01(): Bool = VValidation.sequence(Success(1) :: Nil): Validation[_, Unit] == Success(1 :: Nil)

    @test
    def sequence02(): Bool = VValidation.sequence(Success(1) :: Success(2) :: Nil): Validation[_, Unit] == Success(1 :: 2 :: Nil)

    @test
    def sequence03(): Bool = VValidation.sequence(Success(1) :: Success(2) :: Success(3) :: Nil): Validation[_, Unit] == Success(1 :: 2 :: 3 :: Nil)

    @test
    def sequence04(): Bool = VValidation.sequence(Failure(singleton(1)) :: Nil): Validation[List[Unit], _] == Failure(singleton(1))

    @test
    def sequence05(): Bool = VValidation.sequence(Failure(singleton(1)) :: Failure(singleton(2)) :: Nil): Validation[List[Unit], _] == Failure(necOf2(1, 2))

    @test
    def sequence06(): Bool = VValidation.sequence(Success(1) :: Failure(singleton(42)) :: Nil) == Failure(singleton(42))

    @test
    def sequence07(): Bool = VValidation.sequence(Failure(singleton(42)) :: Success(1) :: Nil) == Failure(singleton(42))

    /////////////////////////////////////////////////////////////////////////////
    // traverse                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def traverse01(): Bool = VValidation.traverse(x -> Success(x + 1), 1 :: Nil): Validation[_, Unit] == Success(2 :: Nil)

    @test
    def traverse02(): Bool = VValidation.traverse(x -> Success(x + 1), 1 :: 2 :: Nil): Validation[_, Unit] == Success(2 :: 3 :: Nil)

    @test
    def traverse03(): Bool = VValidation.traverse(x -> Success(x + 1), 1 :: 2 :: 3 :: Nil): Validation[_, Unit] == Success(2 :: 3 :: 4 :: Nil)

    @test
    def traverse04(): Bool = VValidation.traverse(x -> Failure(singleton(x)), 1 :: 2 :: 3 :: Nil): Validation[List[Unit], _] == Failure(necOf3(1, 2, 3))

    @test
    def traverse05(): Bool = VValidation.traverse(x -> Failure(necOf2(x, x)), 1 :: 2 :: 3 :: Nil): Validation[List[Unit], _] == Failure(necOf6(1, 1, 2, 2, 3, 3))

    @test
    def traverse06(): Bool = VValidation.traverse(x -> if (x != 2) Success(x + 1) else Failure(singleton(42)), 1 :: 2 :: 3 :: Nil) == Failure(singleton(42))

    @test
    def traverse07(): Bool = VValidation.traverse(x -> if (x == 2) Success(x + 1) else Failure(singleton(x)), 1 :: 2 :: 3 :: Nil) == Failure(necOf2(1, 3))

    /////////////////////////////////////////////////////////////////////////////
    // traverseX                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def traverseX01(): Bool = VValidation.traverseX(x -> Success(x + 1), 1 :: Nil): Validation[_, Unit] == Success()

    @test
    def traverseX02(): Bool = VValidation.traverseX(x -> Success(x + 1), 1 :: 2 :: Nil): Validation[_, Unit] == Success()

    @test
    def traverseX03(): Bool = VValidation.traverseX(x -> Success(x + 1), 1 :: 2 :: 3 :: Nil): Validation[_, Unit] == Success()

    @test
    def traverseX04(): Bool = VValidation.traverseX(x -> Failure(singleton(x)), 1 :: 2 :: 3 :: Nil) == Failure(necOf3(1, 2, 3))

    @test
    def traverseX05(): Bool = VValidation.traverseX(x -> Failure(necOf2(x, x)), 1 :: 2 :: 3 :: Nil) == Failure(necOf6(1, 1, 2, 2, 3, 3))

    @test
    def traverseX06(): Bool = VValidation.traverseX(x -> if (x != 2) Success(x + 1) else Failure(singleton(42)), 1 :: 2 :: 3 :: Nil) == Failure(singleton(42))

    @test
    def traverseX07(): Bool = VValidation.traverseX(x -> if (x == 2) Success(x + 1) else Failure(singleton(x)), 1 :: 2 :: 3 :: Nil) == Failure(necOf2(1, 3))

    /////////////////////////////////////////////////////////////////////////////
    // toOption                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toOption01(): Bool = (Success(123) |> VValidation.toOption) == Some(123)

    @test
    def toOption02(): Bool = (Failure(singleton(42)) |> VValidation.toOption): Option[Int32] == None

    /////////////////////////////////////////////////////////////////////////////
    // toResult                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toResult01(): Bool = (Success(123) |> VValidation.toResult): Result[_, Nec[Unit]] == Ok(123)

    @test
    def toResult02(): Bool = (Failure(singleton(42)) |> VValidation.toResult): Result[Unit, _] == Err(singleton(42))

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toList01(): Bool = (Success(123) |> VValidation.toList) == 123 :: Nil

    @test
    def toList02(): Bool = ((Failure(singleton(42)): Validation[Unit, _]) |> VValidation.toList) == Nil

    /////////////////////////////////////////////////////////////////////////////
    // lift2                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def lift201(): Bool = VValidation.lift2((x1,x2) -> x1 + x2, Success(123), Success(1)): Validation[_, Unit] == Success(124)

    @test
    def lift202(): Bool = VValidation.lift2((x1,x2) -> x1 + x2, Success(123), Failure(singleton("e1"))) == Failure(singleton("e1"))

    @test
    def lift203(): Bool = VValidation.lift2((x1,x2) -> x1 + x2, Failure(singleton("e1")), Success(1)) == Failure(singleton("e1"))

    @test
    def lift204(): Bool = VValidation.lift2((x1,x2) -> x1 + x2, Failure(singleton("e1")), Failure(singleton("e2")): Validation[Int32, String]) == Failure(necOf2("e1", "e2"))

    /////////////////////////////////////////////////////////////////////////////
    // lift3                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def lift301(): Bool = VValidation.lift3((x1,x2,x3) -> x1 + x2 + x3, Success(123), Success(1), Success(2)): Validation[_, Unit] == Success(126)

    @test
    def lift302(): Bool = VValidation.lift3((x1,x2,x3) -> x1 + x2 + x3, Success(123), Success(1), Failure(singleton("e1"))) == Failure(singleton("e1"))

    @test
    def lift303(): Bool = VValidation.lift3((x1,x2,x3) -> x1 + x2 + x3, Success(123), Failure(singleton("e1")), Success(2)) == Failure(singleton("e1"))

    @test
    def lift304(): Bool = VValidation.lift3((x1,x2,x3) -> x1 + x2 + x3, Failure(singleton("e1")), Success(1), Success(2)) == Failure(singleton("e1"))

    @test
    def lift305(): Bool = VValidation.lift3((x1,x2,x3) -> x1 + x2 + x3, Success(123), Failure(singleton("e1")), Failure(singleton("e2"))) == Failure(necOf2("e1", "e2"))

    @test
    def lift306(): Bool = VValidation.lift3((x1,x2,x3) -> x1 + x2 + x3, Failure(singleton("e1")), Success(1), Failure(singleton("e2"))) == Failure(necOf2("e1", "e2"))

    @test
    def lift307(): Bool = VValidation.lift3((x1,x2,x3) -> x1 + x2 + x3, Failure(singleton("e1")), Failure(singleton("e2")), Failure(singleton("e3")): Validation[Int32, String]) == Failure(necOf3("e1", "e2", "e3"))

    /////////////////////////////////////////////////////////////////////////////
    // lift4                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def lift401(): Bool = VValidation.lift4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Success(123), Success(1), Success(2), Success(3)): Validation[_, Unit] == Success(129)

    @test
    def lift402(): Bool = VValidation.lift4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Success(123), Success(1), Success(2), Failure(singleton("e1"))) == Failure(singleton("e1"))

    @test
    def lift403(): Bool = VValidation.lift4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Success(123), Success(1), Failure(singleton("e1")), Success(3)) == Failure(singleton("e1"))

    @test
    def lift404(): Bool = VValidation.lift4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Success(123), Failure(singleton("e1")), Success(2), Success(3)) == Failure(singleton("e1"))

    @test
    def lift405(): Bool = VValidation.lift4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Failure(singleton("e1")), Success(1), Success(2), Success(3)) == Failure(singleton("e1"))

    @test
    def lift406(): Bool = VValidation.lift4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Failure(singleton("e1")), Failure(singleton("e2")), Failure(singleton("e3")), Failure(singleton("e4")): Validation[Int32, String]) == Failure(necOf4("e1", "e2", "e3", "e4"))

    /////////////////////////////////////////////////////////////////////////////
    // lift5                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def lift501(): Bool = VValidation.lift5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Success(123), Success(1), Success(2), Success(3), Success(4)): Validation[_, Unit] == Success(133)

    @test
    def lift502(): Bool = VValidation.lift5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Success(123), Success(1), Success(2), Success(3), Failure(singleton("e1"))) == Failure(singleton("e1"))

    @test
    def lift503(): Bool = VValidation.lift5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Success(123), Success(1), Success(2), Failure(singleton("e1")), Success(4)) == Failure(singleton("e1"))

    @test
    def lift504(): Bool = VValidation.lift5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Success(123), Success(1), Failure(singleton("e1")), Success(3), Success(4)) == Failure(singleton("e1"))

    @test
    def lift505(): Bool = VValidation.lift5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Success(123), Failure(singleton("e1")), Success(2), Success(3), Success(4)) == Failure(singleton("e1"))

    @test
    def lift506(): Bool = VValidation.lift5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Failure(singleton("e1")), Success(1), Success(2), Success(3), Success(4)) == Failure(singleton("e1"))

    @test
    def lift507(): Bool = VValidation.lift5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Failure(singleton("e1")), Failure(singleton("e2")), Failure(singleton("e3")), Failure(singleton("e4")), Failure(singleton("e5")): Validation[Int32, String]) == Failure(necOf5("e1", "e2", "e3", "e4", "e5"))

    /////////////////////////////////////////////////////////////////////////////
    // lift6                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def lift601(): Bool = VValidation.lift6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5)): Validation[_, Unit] == Success(138)

    @test
    def lift602(): Bool = VValidation.lift6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Success(123), Success(1), Success(2), Success(3), Success(4), Failure(singleton("e1"))) == Failure(singleton("e1"))

    @test
    def lift603(): Bool = VValidation.lift6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Success(123), Success(1), Success(2), Success(3), Failure(singleton("e1")), Success(5)) == Failure(singleton("e1"))

    @test
    def lift604(): Bool = VValidation.lift6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Success(123), Success(1), Success(2), Failure(singleton("e1")), Success(4), Success(5)) == Failure(singleton("e1"))

    @test
    def lift605(): Bool = VValidation.lift6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Success(123), Success(1), Failure(singleton("e1")), Success(3), Success(4), Success(5)) == Failure(singleton("e1"))

    @test
    def lift606(): Bool = VValidation.lift6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Success(123), Failure(singleton("e1")), Success(2), Success(3), Success(4), Success(5)) == Failure(singleton("e1"))

    @test
    def lift607(): Bool = VValidation.lift6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Failure(singleton("e1")), Success(1), Success(2), Success(3), Success(4), Success(5)) == Failure(singleton("e1"))

    @test
    def lift608(): Bool = VValidation.lift6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Failure(singleton("e1")), Failure(singleton("e2")), Failure(singleton("e3")), Failure(singleton("e4")), Failure(singleton("e5")), Failure(singleton("e6")): Validation[Int32, String]) == Failure(necOf6("e1", "e2", "e3", "e4", "e5", "e6"))

    /////////////////////////////////////////////////////////////////////////////
    // lift7                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def lift701(): Bool = VValidation.lift7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6)): Validation[_, Unit] == Success(144)

    @test
    def lift702(): Bool = VValidation.lift7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Failure(singleton("e1"))) == Failure(singleton("e1"))

    @test
    def lift703(): Bool = VValidation.lift7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Success(123), Success(1), Success(2), Success(3), Success(4), Failure(singleton("e1")), Success(6)) == Failure(singleton("e1"))

    @test
    def lift704(): Bool = VValidation.lift7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Success(123), Success(1), Success(2), Success(3), Failure(singleton("e1")), Success(5), Success(6)) == Failure(singleton("e1"))

    @test
    def lift705(): Bool = VValidation.lift7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Success(123), Success(1), Success(2), Failure(singleton("e1")), Success(4), Success(5), Success(6)) == Failure(singleton("e1"))

    @test
    def lift706(): Bool = VValidation.lift7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Success(123), Success(1), Failure(singleton("e1")), Success(3), Success(4), Success(5), Success(6)) == Failure(singleton("e1"))

    @test
    def lift707(): Bool = VValidation.lift7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Success(123), Failure(singleton("e1")), Success(2), Success(3), Success(4), Success(5), Success(6)) == Failure(singleton("e1"))

    @test
    def lift708(): Bool = VValidation.lift7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Failure(singleton("e1")), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6)) == Failure(singleton("e1"))

    @test
    def lift709(): Bool = VValidation.lift7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Failure(singleton("e1")), Failure(singleton("e2")), Failure(singleton("e3")), Failure(singleton("e4")), Failure(singleton("e5")), Failure(singleton("e6")), Failure(singleton("e7")): Validation[Int32, String]) == Failure(necOf7("e1", "e2", "e3", "e4", "e5", "e6", "e7"))

    /////////////////////////////////////////////////////////////////////////////
    // lift8                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def lift801(): Bool = VValidation.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7)): Validation[_, Unit] == Success(151)

    @test
    def lift802(): Bool = VValidation.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Failure(singleton("e1"))) == Failure(singleton("e1"))

    @test
    def lift803(): Bool = VValidation.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Failure(singleton("e1")), Success(7)) == Failure(singleton("e1"))

    @test
    def lift804(): Bool = VValidation.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Success(123), Success(1), Success(2), Success(3), Success(4), Failure(singleton("e1")), Success(6), Success(7)) == Failure(singleton("e1"))

    @test
    def lift805(): Bool = VValidation.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Success(123), Success(1), Success(2), Success(3), Failure(singleton("e1")), Success(5), Success(6), Success(7)) == Failure(singleton("e1"))

    @test
    def lift806(): Bool = VValidation.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Success(123), Success(1), Success(2), Failure(singleton("e1")), Success(4), Success(5), Success(6), Success(7)) == Failure(singleton("e1"))

    @test
    def lift807(): Bool = VValidation.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Success(123), Success(1), Failure(singleton("e1")), Success(3), Success(4), Success(5), Success(6), Success(7)) == Failure(singleton("e1"))

    @test
    def lift808(): Bool = VValidation.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Success(123), Failure(singleton("e1")), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7)) == Failure(singleton("e1"))

    @test
    def lift809(): Bool = VValidation.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Failure(singleton("e1")), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7)) == Failure(singleton("e1"))

    @test
    def lift810(): Bool = VValidation.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Failure(singleton("e1")), Failure(singleton("e2")), Failure(singleton("e3")), Failure(singleton("e4")), Failure(singleton("e5")), Failure(singleton("e6")), Failure(singleton("e7")), Failure(singleton("e8")): Validation[Int32, String]) == Failure(necOf8("e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8"))

    /////////////////////////////////////////////////////////////////////////////
    // lift9                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def lift901(): Bool = VValidation.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7), Success(8)): Validation[_, Unit] == Success(159)

    @test
    def lift902(): Bool = VValidation.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7), Failure(singleton("e1"))) == Failure(singleton("e1"))

    @test
    def lift903(): Bool = VValidation.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Failure(singleton("e1")), Success(8)) == Failure(singleton("e1"))

    @test
    def lift904(): Bool = VValidation.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Failure(singleton("e1")), Success(7), Success(8)) == Failure(singleton("e1"))

    @test
    def lift905(): Bool = VValidation.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Success(123), Success(1), Success(2), Success(3), Success(4), Failure(singleton("e1")), Success(6), Success(7), Success(8)) == Failure(singleton("e1"))

    @test
    def lift906(): Bool = VValidation.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Success(123), Success(1), Success(2), Success(3), Failure(singleton("e1")), Success(5), Success(6), Success(7), Success(8)) == Failure(singleton("e1"))

    @test
    def lift907(): Bool = VValidation.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Success(123), Success(1), Success(2), Failure(singleton("e1")), Success(4), Success(5), Success(6), Success(7), Success(8)) == Failure(singleton("e1"))

    @test
    def lift908(): Bool = VValidation.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Success(123), Success(1), Failure(singleton("e1")), Success(3), Success(4), Success(5), Success(6), Success(7), Success(8)) == Failure(singleton("e1"))

    @test
    def lift909(): Bool = VValidation.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Success(123), Failure(singleton("e1")), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7), Success(8)) == Failure(singleton("e1"))

    @test
    def lift910(): Bool = VValidation.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Failure(singleton("e1")), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7), Success(8)) == Failure(singleton("e1"))

    @test
    def lift911(): Bool = VValidation.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Failure(singleton("e1")), Failure(singleton("e2")), Failure(singleton("e3")), Failure(singleton("e4")), Failure(singleton("e5")), Failure(singleton("e6")), Failure(singleton("e7")), Failure(singleton("e8")), Failure(singleton("e9")): Validation[Int32, String]) == Failure(necOf9("e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9"))

    /////////////////////////////////////////////////////////////////////////////
    // lift10                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def lift1001(): Bool = VValidation.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7), Success(8), Success(9)): Validation[_, Unit] == Success(168)

    @test
    def lift1002(): Bool = VValidation.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7), Success(8), Failure(singleton("e1"))) == Failure(singleton("e1"))

    @test
    def lift1003(): Bool = VValidation.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7), Failure(singleton("e1")), Success(9)) == Failure(singleton("e1"))

    @test
    def lift1004(): Bool = VValidation.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Failure(singleton("e1")), Success(8), Success(9)) == Failure(singleton("e1"))

    @test
    def lift1005(): Bool = VValidation.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Failure(singleton("e1")), Success(7), Success(8), Success(9)) == Failure(singleton("e1"))

    @test
    def lift1006(): Bool = VValidation.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Success(1), Success(2), Success(3), Success(4), Failure(singleton("e1")), Success(6), Success(7), Success(8), Success(9)) == Failure(singleton("e1"))

    @test
    def lift1007(): Bool = VValidation.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Success(1), Success(2), Success(3), Failure(singleton("e1")), Success(5), Success(6), Success(7), Success(8), Success(9)) == Failure(singleton("e1"))

    @test
    def lift1008(): Bool = VValidation.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Success(1), Success(2), Failure(singleton("e1")), Success(4), Success(5), Success(6), Success(7), Success(8), Success(9)) == Failure(singleton("e1"))

    @test
    def lift1009(): Bool = VValidation.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Success(1), Failure(singleton("e1")), Success(3), Success(4), Success(5), Success(6), Success(7), Success(8), Success(9)) == Failure(singleton("e1"))

    @test
    def lift1010(): Bool = VValidation.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Failure(singleton("e1")), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7), Success(8), Success(9)) == Failure(singleton("e1"))

    @test
    def lift1011(): Bool = VValidation.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Failure(singleton("e1")), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7), Success(8), Success(9)) == Failure(singleton("e1"))

    @test
    def lift1012(): Bool = VValidation.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Failure(singleton("e1")), Failure(singleton("e2")), Failure(singleton("e3")), Failure(singleton("e4")), Failure(singleton("e5")), Failure(singleton("e6")), Failure(singleton("e7")), Failure(singleton("e8")), Failure(singleton("e9")), Failure(singleton("e10")): Validation[Int32, String]) == Failure(necOf10("e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "e10"))

    /////////////////////////////////////////////////////////////////////////////
    // hash                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def hash01(): Bool = hash(Success(()): Validation[_, Unit]) == hash(Success(()): Validation[_, Unit])

    @test
    def hash02(): Bool = hash(Success(()): Validation[_, Unit]) != hash(Failure(necOf2((), ())): Validation[Unit, _])

    @test
    def hash03(): Bool = hash(Success(1, 2): Validation[_, Unit]) == hash(Success(1, 2): Validation[_, Unit])

    @test
    def hash04(): Bool = hash(Success(1, 2, 3): Validation[_, Unit]) != hash(Success(1, 3, 2): Validation[_, Unit])

    @test
    def hash05(): Bool = hash(Success(singleton(1)): Validation[_, Int32]) != hash(Failure(singleton(1)): Validation[Nec[Int32], _])

}
