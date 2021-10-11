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


namespace MutDeque {

    pub def new(): MutDeque[a] & Impure = ???

    pub def size(d: MutDeque[a]): Int32 & Impure = ???

    pub def isEmpty(d: MutDeque[a]): Bool & Impure = ???

    pub def popFront(d: MutDeque[a]): Unit & Impure = ???
    
    pub def popBack(d: MutDeque[a]): Unit & Impure = ???

    pub def pushFront(d: MutDeque[a]): Unit & Impure = ???

    pub def pushBack(d: MutDeque[a]): Unit & Impure = ???
    
}