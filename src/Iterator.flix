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

/*
///
/// The type of the done function.
///
type alias Done = Unit ~> Bool

///
/// The type of the next function.
///
type alias Next[a] = Unit ~> a

///
/// An iterator is a stream of elements that are produced on demand.
///
pub enum Iterator[a] {
    case Iterator(Done, Next[a])
}
*/

namespace Iterator {

    pub def range(b: Int32, e: Int32): Iterator[Int32] & Impure = ???

    pub def repeat(x: a, n: Int32): Iterator[a] & Impure = ???

}