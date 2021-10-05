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

namespace List1 {

    ///
    /// Converts the list `l` to a `Stream`.
    ///
    pub def toStream(l: List[a]): Stream[a, List[a]] =
        let next = s -> match s {
            case Nil     => Done
            case x :: xs => Yield(x, xs)
        };
        Stream(next, l)

    ///
    /// Mocks `List.filter` until merged into nightly compiler.
    ///
    pub def filter(f: a -> Bool & ef, xs: List[a]): List[a] & ef = filterAcc(f, xs, Nil) |> List.reverse

    pub def filterAcc(f: a -> Bool & ef, l: List[a], rs: List[a]): List[a] & ef = match l {
        case Nil => rs
        case x :: xs =>
            if (f(x)) filterAcc(f, xs, x :: rs) else filterAcc(f, xs, rs)
    }


    pub def toStreamListed(l: List[a]): StreamListed[a, s] =
        Listed(l)

}