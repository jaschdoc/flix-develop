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

namespace AllGeneratedTests2 {
	/*

@test
def intersperse01(): Bool = 
	LazyList1.intersperse(-11, ENil) == ENil

@test
def intersperse02(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ENil)) == ENil


@test
def intersperse03(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ENil)) == ECons(1, ENil)

@test
def intersperse04(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ENil)))) == ECons(1, ENil)

@test
def intersperse05(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ENil))) == ECons(1, ENil)

@test
def intersperse06(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ENil))) == ECons(1, ENil)

@test
def intersperse07(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ENil)) == ECons(1, ENil)

@test
def intersperse08(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ENil))) == ECons(1, ENil)

@test
def intersperse09(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ENil)))) == ECons(1, ENil)

@test
def intersperse10(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ENil))) == ECons(1, ENil)


@test
def intersperse11(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ENil)))

@test
def intersperse12(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, LList(lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ENil)))

@test
def intersperse13(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy ENil)))) == ECons(1, ECons(-11, ECons(2, ENil)))

@test
def intersperse14(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy ENil))) == ECons(1, ECons(-11, ECons(2, ENil)))

@test
def intersperse15(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, LList(lazy ENil)))) == ECons(1, ECons(-11, ECons(2, ENil)))

@test
def intersperse16(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy ENil)))) == ECons(1, ECons(-11, ECons(2, ENil)))

@test
def intersperse17(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, LList(lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ENil)))

@test
def intersperse18(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy LList(lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ENil)))

@test
def intersperse19(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, ENil))))) == ECons(1, ECons(-11, ECons(2, ENil)))

@test
def intersperse20(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, ENil))) == ECons(1, ECons(-11, ECons(2, ENil)))

@test
def intersperse21(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ENil)))

@test
def intersperse22(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, ENil))))) == ECons(1, ECons(-11, ECons(2, ENil)))

@test
def intersperse23(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, LList(lazy ENil)))) == ECons(1, ECons(-11, ECons(2, ENil)))

@test
def intersperse24(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ENil)))

@test
def intersperse25(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ENil)))

@test
def intersperse26(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ENil)))

@test
def intersperse27(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy LList(lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ENil)))

@test
def intersperse28(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy ENil)))) == ECons(1, ECons(-11, ECons(2, ENil)))

@test
def intersperse29(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, ENil)))) == ECons(1, ECons(-11, ECons(2, ENil)))

@test
def intersperse30(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, LList(lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ENil)))

@test
def intersperse31(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ENil)))

@test
def intersperse32(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, ENil))) == ECons(1, ECons(-11, ECons(2, ENil)))

@test
def intersperse33(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy ENil))) == ECons(1, ECons(-11, ECons(2, ENil)))

@test
def intersperse34(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy LList(lazy ENil)))) == ECons(1, ECons(-11, ECons(2, ENil)))

@test
def intersperse35(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy LList(lazy ENil)))) == ECons(1, ECons(-11, ECons(2, ENil)))

@test
def intersperse36(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy ENil)))) == ECons(1, ECons(-11, ECons(2, ENil)))

@test
def intersperse37(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, LList(lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ENil)))

@test
def intersperse38(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, ENil)))) == ECons(1, ECons(-11, ECons(2, ENil)))

@test
def intersperse39(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ENil)))

@test
def intersperse40(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, ENil)))) == ECons(1, ECons(-11, ECons(2, ENil)))

@test
def intersperse41(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ENil)))

@test
def intersperse42(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, ENil)))) == ECons(1, ECons(-11, ECons(2, ENil)))



@test
def intersperse43(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse44(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse45(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse46(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse47(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse48(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy ECons(3, ENil)))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse49(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse50(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse51(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, ECons(3, LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse52(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse53(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, ECons(3, LList(lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse54(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, LList(lazy LCons(3, lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse55(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy LCons(3, lazy LList(lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse56(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse57(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse58(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy ECons(3, LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse59(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse60(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse61(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, ECons(3, ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse62(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse63(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse64(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse65(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse66(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, LCons(3, lazy ENil)))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse67(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy ECons(3, ENil)))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse68(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, LCons(3, lazy ENil)))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse69(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse70(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy ECons(3, ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse71(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy ECons(3, LList(lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse72(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, LCons(3, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse73(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse74(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse75(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse76(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse77(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse78(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse79(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse80(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse81(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse82(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, ECons(3, ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse83(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, ECons(3, LList(lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse84(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse85(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse86(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse87(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, LCons(3, lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse88(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, ECons(3, ENil)))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse89(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse90(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, LCons(3, lazy LList(lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse91(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse92(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse93(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse94(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy LCons(3, lazy LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse95(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse96(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse97(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, LList(lazy ECons(3, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse98(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse99(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, LCons(3, lazy LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse100(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse101(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, ECons(3, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse102(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy ECons(3, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse103(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse104(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse105(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse106(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil)))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse107(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse108(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse109(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse110(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse111(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse112(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse113(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse114(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse115(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, ECons(3, ENil)))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse116(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy LCons(3, lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse117(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse118(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse119(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy ECons(3, LList(lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse120(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, ECons(3, LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse121(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse122(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse123(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse124(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy LCons(3, lazy LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse125(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse126(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse127(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse128(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, ECons(3, ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse129(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse148(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, LList(lazy ECons(3, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse149(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy LList(lazy ECons(3, ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse150(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy LCons(3, lazy ENil)))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))


@test
def intercalate03(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), ECons(LCons(1, lazy ENil), ENil)) == ECons(1, ENil)

@test
def intercalate04(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), ECons(LList(lazy ECons(1, LList(lazy ENil))), ENil)) == ECons(1, ENil)

@test
def intercalate05(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), ECons(ECons(1, LList(lazy ENil)), ENil)) == ECons(1, ENil)

@test
def intercalate06(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), ECons(LCons(1, lazy LList(lazy ENil)), ENil)) == ECons(1, ENil)

@test
def intercalate07(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), ECons(ECons(1, ENil), ENil)) == ECons(1, ENil)

@test
def intercalate08(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), ECons(LList(lazy LCons(1, lazy ENil)), ENil)) == ECons(1, ENil)

@test
def intercalate09(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), ECons(LList(lazy LCons(1, lazy LList(lazy ENil))), ENil)) == ECons(1, ENil)

@test
def intercalate10(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), ECons(LList(lazy ECons(1, ENil)), ENil)) == ECons(1, ENil)


@test
def intercalate11(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil)))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate12(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil)))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate13(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate14(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil)))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate15(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, ENil)))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate16(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(ECons(1, LCons(2, lazy ECons(3, ENil))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate17(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy LList(lazy ENil)))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate18(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate19(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(ECons(1, LList(lazy ECons(2, ECons(3, LList(lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate20(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy ENil))))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate21(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LCons(1, lazy ECons(2, ECons(3, LList(lazy ENil)))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate22(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(ECons(1, ECons(2, LList(lazy LCons(3, lazy ENil)))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate23(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(ECons(1, LCons(2, lazy LCons(3, lazy LList(lazy ENil)))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate24(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ENil)))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate25(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate26(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(ECons(1, LList(lazy LCons(2, lazy ECons(3, LList(lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate27(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, ENil)))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate28(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy ENil)))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate29(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(ECons(1, LList(lazy ECons(2, ECons(3, ENil)))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate30(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate31(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil)))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate32(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy ENil)))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate33(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil))))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate34(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(ECons(1, ECons(2, LCons(3, lazy ENil))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate35(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LCons(1, lazy LCons(2, lazy ECons(3, ENil))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate36(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LCons(1, lazy ECons(2, LCons(3, lazy ENil))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate37(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy ECons(1, ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil)))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate38(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy ECons(1, LCons(2, lazy ECons(3, ENil)))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate39(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LCons(1, lazy LCons(2, lazy ECons(3, LList(lazy ENil)))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate40(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy ECons(1, LList(lazy ECons(2, LCons(3, lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate41(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(ECons(1, ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate42(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate43(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil))))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate44(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate45(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil)))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate46(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy ENil)))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate47(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate48(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy ENil)))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate49(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil))))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate50(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy LCons(1, lazy ECons(2, ECons(3, ENil)))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate51(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(ECons(1, ECons(2, ECons(3, LList(lazy ENil)))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate52(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate53(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy ENil))))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate54(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate55(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy ECons(1, ECons(2, LCons(3, lazy ENil)))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate56(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(ECons(1, ECons(2, ECons(3, ENil))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate57(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil))))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate58(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(ECons(1, ECons(2, LCons(3, lazy LList(lazy ENil)))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate59(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate60(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy ENil)))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate61(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate62(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy LCons(1, lazy LCons(2, lazy LCons(3, lazy LList(lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate63(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ENil)))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate64(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy ENil)))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate65(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy ECons(1, ECons(2, LList(lazy ECons(3, ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate66(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate67(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy ECons(1, ECons(2, LCons(3, lazy LList(lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate68(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, LList(lazy ENil)))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate69(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy ECons(1, LList(lazy ECons(2, ECons(3, ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate70(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy ECons(1, LList(lazy LCons(2, lazy ECons(3, ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate71(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate72(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil)))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate73(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy ENil)))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate74(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate75(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate76(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ENil)))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate77(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy ECons(1, LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil)))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate78(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate79(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ENil))))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate80(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy ECons(1, LList(lazy ECons(2, ECons(3, LList(lazy ENil)))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate81(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate82(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, ENil)))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate83(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LCons(1, lazy ECons(2, ECons(3, ENil))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate84(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy ECons(1, LCons(2, lazy LCons(3, lazy ENil)))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate85(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate86(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(ECons(1, LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate87(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(ECons(1, LCons(2, lazy ECons(3, LList(lazy ENil)))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate88(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy LCons(1, lazy ECons(2, ECons(3, LList(lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate89(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil)))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate90(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate91(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, ENil)))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate92(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy ECons(1, LCons(2, lazy LCons(3, lazy LList(lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate93(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy ENil)))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate94(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate95(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil)))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate96(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy ECons(1, ECons(2, ECons(3, ENil)))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate97(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil)))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate98(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy ECons(1, LList(lazy LCons(2, lazy ECons(3, LList(lazy ENil)))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate99(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ENil)))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate100(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(ECons(1, LList(lazy LCons(2, lazy ECons(3, ENil)))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate101(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy LCons(1, lazy LCons(2, lazy ECons(3, ENil)))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate102(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LCons(1, lazy LCons(2, lazy LCons(3, lazy LList(lazy ENil)))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate103(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LCons(1, lazy LList(lazy ECons(2, ECons(3, ENil)))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate104(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil)))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate105(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate106(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate107(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(ECons(1, LList(lazy ECons(2, LCons(3, lazy ENil)))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate108(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy ENil)))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate109(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate110(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate111(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy ECons(1, ECons(2, ECons(3, LList(lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate112(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy ENil)))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate113(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy ECons(1, LCons(2, lazy LList(lazy ECons(3, ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate114(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(ECons(1, LList(lazy ECons(2, LCons(3, lazy LList(lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate115(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LCons(1, lazy ECons(2, LList(lazy ECons(3, LList(lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate116(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy LCons(1, lazy ECons(2, LList(lazy ECons(3, LList(lazy ENil)))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate117(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(ECons(1, LCons(2, lazy LList(lazy ECons(3, ENil)))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate118(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(ECons(1, LCons(2, lazy LCons(3, lazy ENil))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate119(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy ECons(1, LCons(2, lazy ECons(3, LList(lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate120(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy LCons(1, lazy ECons(2, LList(lazy ECons(3, ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate121(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy ENil)))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate122(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(ECons(1, ECons(2, LList(lazy ECons(3, LList(lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate123(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy LCons(1, lazy ECons(2, LCons(3, lazy ENil)))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate124(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ENil))))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate125(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, ENil)))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate126(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, LList(lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate127(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy ECons(1, LList(lazy ECons(2, LCons(3, lazy LList(lazy ENil)))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate128(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil)))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate129(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy LList(lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate130(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate131(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(ECons(1, ECons(2, LList(lazy ECons(3, ENil)))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate132(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LCons(1, lazy ECons(2, LList(lazy ECons(3, ENil)))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate133(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy LCons(1, lazy LCons(2, lazy ECons(3, LList(lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate134(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate135(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil)))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate136(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy ECons(1, ECons(2, LList(lazy LCons(3, lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate137(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy ECons(1, ECons(2, LList(lazy ECons(3, LList(lazy ENil)))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

@test
def intercalate138(): Bool = 
	LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, ENil)))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))


@test
def flatMap01(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ENil: LazyList[Int32]) == ENil

@test
def flatMap02(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ENil): LazyList[Int32]) == ENil

@test
def flatMap03(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy ENil)) == ECons(1, ENil)

@test
def flatMap04(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, LList(lazy ENil)))) == ECons(1, ENil)

@test
def flatMap05(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, LList(lazy ENil))) == ECons(1, ENil)

@test
def flatMap06(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy LList(lazy ENil))) == ECons(1, ENil)

@test
def flatMap07(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, ENil)) == ECons(1, ENil)

@test
def flatMap08(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy ENil))) == ECons(1, ENil)

@test
def flatMap09(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy LList(lazy ENil)))) == ECons(1, ENil)

@test
def flatMap10(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, ENil))) == ECons(1, ENil)


@test
def flatMap11(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(2, LList(lazy ENil)))) == ECons(2, ECons(2, ENil))

@test
def flatMap12(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(2, lazy ENil)) == ECons(2, ECons(2, ENil))

@test
def flatMap13(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(2, LList(lazy ENil))) == ECons(2, ECons(2, ENil))

@test
def flatMap14(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(2, ENil))) == ECons(2, ECons(2, ENil))

@test
def flatMap15(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(2, lazy LList(lazy ENil)))) == ECons(2, ECons(2, ENil))

@test
def flatMap16(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(2, lazy LList(lazy ENil))) == ECons(2, ECons(2, ENil))

@test
def flatMap17(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(2, ENil)) == ECons(2, ECons(2, ENil))

@test
def flatMap18(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(2, lazy ENil))) == ECons(2, ECons(2, ENil))

@test
def flatMap19(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil))))) == ECons(1, ECons(2, ECons(2, ENil)))

@test
def flatMap20(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy ECons(2, LList(lazy ENil))))) == ECons(1, ECons(2, ECons(2, ENil)))

@test
def flatMap21(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy LCons(2, lazy ENil)))) == ECons(1, ECons(2, ECons(2, ENil)))

@test
def flatMap22(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, LCons(2, lazy ENil))) == ECons(1, ECons(2, ECons(2, ENil)))

@test
def flatMap23(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy ECons(2, LList(lazy ENil)))) == ECons(1, ECons(2, ECons(2, ENil)))

@test
def flatMap24(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, LList(lazy LCons(2, lazy ENil)))) == ECons(1, ECons(2, ECons(2, ENil)))

@test
def flatMap25(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, ECons(2, LList(lazy ENil))))) == ECons(1, ECons(2, ECons(2, ENil)))

@test
def flatMap26(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, LList(lazy LCons(2, lazy LList(lazy ENil))))) == ECons(1, ECons(2, ECons(2, ENil)))

@test
def flatMap27(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, LList(lazy ECons(2, ENil))))) == ECons(1, ECons(2, ECons(2, ENil)))

@test
def flatMap28(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy ECons(2, ENil))) == ECons(1, ECons(2, ECons(2, ENil)))

@test
def flatMap29(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ENil))))) == ECons(1, ECons(2, ECons(2, ENil)))

@test
def flatMap30(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy LList(lazy ECons(2, ENil))))) == ECons(1, ECons(2, ECons(2, ENil)))

@test
def flatMap31(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, ECons(2, LList(lazy ENil)))) == ECons(1, ECons(2, ECons(2, ENil)))

@test
def flatMap32(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ENil)))

@test
def flatMap33(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ENil)))

@test
def flatMap34(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, LList(lazy LCons(2, lazy ENil))))) == ECons(1, ECons(2, ECons(2, ENil)))

@test
def flatMap35(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, LCons(2, lazy LList(lazy ENil))))) == ECons(1, ECons(2, ECons(2, ENil)))

@test
def flatMap36(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy LList(lazy LCons(2, lazy ENil)))) == ECons(1, ECons(2, ECons(2, ENil)))

@test
def flatMap37(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, LList(lazy ECons(2, ENil)))) == ECons(1, ECons(2, ECons(2, ENil)))

@test
def flatMap38(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, LList(lazy ECons(2, LList(lazy ENil))))) == ECons(1, ECons(2, ECons(2, ENil)))

@test
def flatMap39(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ENil)))

@test
def flatMap40(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, ECons(2, ENil))) == ECons(1, ECons(2, ECons(2, ENil)))

@test
def flatMap41(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy LCons(2, lazy ENil))) == ECons(1, ECons(2, ECons(2, ENil)))

@test
def flatMap42(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy LCons(2, lazy LList(lazy ENil)))) == ECons(1, ECons(2, ECons(2, ENil)))

@test
def flatMap43(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, LCons(2, lazy LList(lazy ENil)))) == ECons(1, ECons(2, ECons(2, ENil)))

@test
def flatMap44(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, LCons(2, lazy ENil)))) == ECons(1, ECons(2, ECons(2, ENil)))

@test
def flatMap45(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy LList(lazy ECons(2, LList(lazy ENil))))) == ECons(1, ECons(2, ECons(2, ENil)))

@test
def flatMap46(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, ECons(2, ENil)))) == ECons(1, ECons(2, ECons(2, ENil)))

@test
def flatMap47(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy ENil))))) == ECons(1, ECons(2, ECons(2, ENil)))

@test
def flatMap48(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy LList(lazy ECons(2, ENil)))) == ECons(1, ECons(2, ECons(2, ENil)))

@test
def flatMap49(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ENil)))

@test
def flatMap50(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy ECons(2, ENil)))) == ECons(1, ECons(2, ECons(2, ENil)))



@test
def flatMap51(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap52(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap53(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap54(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap55(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap56(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, LCons(2, lazy ECons(3, ENil)))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap57(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy LList(lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap58(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap59(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, LList(lazy ECons(2, ECons(3, LList(lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap60(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy ENil)))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap61(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy ECons(2, ECons(3, LList(lazy ENil))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap62(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, ECons(2, LList(lazy LCons(3, lazy ENil))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap63(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, LCons(2, lazy LCons(3, lazy LList(lazy ENil))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap64(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap65(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap66(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, LList(lazy LCons(2, lazy ECons(3, LList(lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap67(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap68(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy ENil))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap69(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, LList(lazy ECons(2, ECons(3, ENil))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap70(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap71(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap72(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap73(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil)))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap74(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, ECons(2, LCons(3, lazy ENil)))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap75(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy LCons(2, lazy ECons(3, ENil)))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap76(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy ECons(2, LCons(3, lazy ENil)))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap77(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap78(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, LCons(2, lazy ECons(3, ENil))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap79(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy LCons(2, lazy ECons(3, LList(lazy ENil))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap80(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, LList(lazy ECons(2, LCons(3, lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap81(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap82(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap83(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil)))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap84(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap85(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap86(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy ENil))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap87(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap88(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap89(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil)))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap90(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy ECons(2, ECons(3, ENil))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap91(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, ECons(2, ECons(3, LList(lazy ENil))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap92(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap93(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy ENil)))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap94(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap95(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, ECons(2, LCons(3, lazy ENil))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap96(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, ECons(2, ECons(3, ENil)))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap97(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil)))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap98(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, ECons(2, LCons(3, lazy LList(lazy ENil))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap99(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap100(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap101(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap102(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy LCons(2, lazy LCons(3, lazy LList(lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap103(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap104(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap105(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, ECons(2, LList(lazy ECons(3, ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap106(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap107(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, ECons(2, LCons(3, lazy LList(lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap108(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, LList(lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap109(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, LList(lazy ECons(2, ECons(3, ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap110(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, LList(lazy LCons(2, lazy ECons(3, ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap111(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap112(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap113(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy ENil))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap114(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil)))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap115(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap116(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap117(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap118(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap119(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ENil)))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap120(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap121(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap122(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, ENil))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap123(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy ECons(2, ECons(3, ENil)))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap124(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, LCons(2, lazy LCons(3, lazy ENil))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap125(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap126(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap127(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, LCons(2, lazy ECons(3, LList(lazy ENil))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap128(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy ECons(2, ECons(3, LList(lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap129(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap130(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap131(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap132(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, LCons(2, lazy LCons(3, lazy LList(lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap133(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy ENil))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap134(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap135(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap136(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, ECons(2, ECons(3, ENil))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap137(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap138(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, LList(lazy LCons(2, lazy ECons(3, LList(lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap139(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap140(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, LList(lazy LCons(2, lazy ECons(3, ENil))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap141(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy LCons(2, lazy ECons(3, ENil))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap142(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy LCons(2, lazy LCons(3, lazy LList(lazy ENil))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap143(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy LList(lazy ECons(2, ECons(3, ENil))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap144(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap145(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap146(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap147(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, LList(lazy ECons(2, LCons(3, lazy ENil))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap148(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy ENil))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap149(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap150(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap151(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, ECons(2, ECons(3, LList(lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap152(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap153(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, LCons(2, lazy LList(lazy ECons(3, ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap154(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, LList(lazy ECons(2, LCons(3, lazy LList(lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap155(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy ECons(2, LList(lazy ECons(3, LList(lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap156(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy ECons(2, LList(lazy ECons(3, LList(lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap157(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, LCons(2, lazy LList(lazy ECons(3, ENil))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap158(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, LCons(2, lazy LCons(3, lazy ENil)))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap159(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, LCons(2, lazy ECons(3, LList(lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap160(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy ECons(2, LList(lazy ECons(3, ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap161(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap162(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, ECons(2, LList(lazy ECons(3, LList(lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap163(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy ECons(2, LCons(3, lazy ENil))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap164(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ENil)))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap165(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, ENil))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap166(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, LList(lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap167(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, LList(lazy ECons(2, LCons(3, lazy LList(lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap168(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap169(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy LList(lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap170(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap171(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, ECons(2, LList(lazy ECons(3, ENil))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap172(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy ECons(2, LList(lazy ECons(3, ENil))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap173(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy LCons(1, lazy LCons(2, lazy ECons(3, LList(lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap174(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap175(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap176(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, ECons(2, LList(lazy LCons(3, lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap177(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, ECons(2, LList(lazy ECons(3, LList(lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

@test
def flatMap178(): Bool = 
	LazyList1.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))


@test
def memberOf03(): Bool = 
	not (LazyList1.memberOf(0, LCons(1, lazy ENil)))

@test
def memberOf04(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ENil)))))

@test
def memberOf05(): Bool = 
	not (LazyList1.memberOf(0, ECons(1, LList(lazy ENil))))

@test
def memberOf06(): Bool = 
	not (LazyList1.memberOf(0, LCons(1, lazy LList(lazy ENil))))

@test
def memberOf07(): Bool = 
	not (LazyList1.memberOf(0, ECons(1, ENil)))

@test
def memberOf08(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy LCons(1, lazy ENil))))

@test
def memberOf09(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ENil)))))

@test
def memberOf10(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy ECons(1, ENil))))


@test
def memberOf11(): Bool = 
	not (LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))))

@test
def memberOf12(): Bool = 
	not (LazyList1.memberOf(0, LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))))

@test
def memberOf13(): Bool = 
	not (LazyList1.memberOf(0, LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil)))))))

@test
def memberOf14(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))))

@test
def memberOf15(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, ENil))))))))

@test
def memberOf16(): Bool = 
	not (LazyList1.memberOf(0, ECons(1, LCons(2, lazy ECons(3, ENil)))))

@test
def memberOf17(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy LList(lazy ENil))))))))

@test
def memberOf18(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy ENil)))))))

@test
def memberOf19(): Bool = 
	not (LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, ECons(3, LList(lazy ENil)))))))

@test
def memberOf20(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy ENil)))))))))

@test
def memberOf21(): Bool = 
	not (LazyList1.memberOf(0, LCons(1, lazy ECons(2, ECons(3, LList(lazy ENil))))))

@test
def memberOf22(): Bool = 
	not (LazyList1.memberOf(0, ECons(1, ECons(2, LList(lazy LCons(3, lazy ENil))))))

@test
def memberOf23(): Bool = 
	not (LazyList1.memberOf(0, ECons(1, LCons(2, lazy LCons(3, lazy LList(lazy ENil))))))

@test
def memberOf24(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ENil))))))))

@test
def memberOf25(): Bool = 
	not (LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, ENil)))))))

@test
def memberOf26(): Bool = 
	not (LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy ECons(3, LList(lazy ENil)))))))

@test
def memberOf27(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, ENil))))))))

@test
def memberOf28(): Bool = 
	not (LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy ENil))))))

@test
def memberOf29(): Bool = 
	not (LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, ECons(3, ENil))))))

@test
def memberOf30(): Bool = 
	not (LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ENil)))))))

@test
def memberOf31(): Bool = 
	not (LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil))))))))

@test
def memberOf32(): Bool = 
	not (LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy ENil))))))))

@test
def memberOf33(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil)))))))))

@test
def memberOf34(): Bool = 
	not (LazyList1.memberOf(0, ECons(1, ECons(2, LCons(3, lazy ENil)))))

@test
def memberOf35(): Bool = 
	not (LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy ECons(3, ENil)))))

@test
def memberOf36(): Bool = 
	not (LazyList1.memberOf(0, LCons(1, lazy ECons(2, LCons(3, lazy ENil)))))

@test
def memberOf37(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil))))))))

@test
def memberOf38(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy ECons(3, ENil))))))

@test
def memberOf39(): Bool = 
	not (LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy ECons(3, LList(lazy ENil))))))

@test
def memberOf40(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, LCons(3, lazy ENil)))))))

@test
def memberOf41(): Bool = 
	not (LazyList1.memberOf(0, ECons(1, ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil)))))))

@test
def memberOf42(): Bool = 
	not (LazyList1.memberOf(0, ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ENil)))))))

@test
def memberOf43(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil)))))))))

@test
def memberOf44(): Bool = 
	not (LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy ENil)))))))

@test
def memberOf45(): Bool = 
	not (LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil))))))))

@test
def memberOf46(): Bool = 
	not (LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy ENil))))))

@test
def memberOf47(): Bool = 
	not (LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil)))))))

@test
def memberOf48(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy ENil))))))))

@test
def memberOf49(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil)))))))))

@test
def memberOf50(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, ECons(3, ENil))))))

@test
def memberOf51(): Bool = 
	not (LazyList1.memberOf(0, ECons(1, ECons(2, ECons(3, LList(lazy ENil))))))

@test
def memberOf52(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil)))))))

@test
def memberOf53(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy ENil)))))))))

@test
def memberOf54(): Bool = 
	not (LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, ENil)))))))

@test
def memberOf55(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, LCons(3, lazy ENil))))))

@test
def memberOf56(): Bool = 
	not (LazyList1.memberOf(0, ECons(1, ECons(2, ECons(3, ENil)))))

@test
def memberOf57(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil)))))))))

@test
def memberOf58(): Bool = 
	not (LazyList1.memberOf(0, ECons(1, ECons(2, LCons(3, lazy LList(lazy ENil))))))

@test
def memberOf59(): Bool = 
	not (LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil)))))))

@test
def memberOf60(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy ENil))))))))

@test
def memberOf61(): Bool = 
	not (LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, ENil)))))))

@test
def memberOf62(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy LCons(3, lazy LList(lazy ENil)))))))

@test
def memberOf63(): Bool = 
	not (LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ENil))))))))

@test
def memberOf64(): Bool = 
	not (LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy ENil))))))))

@test
def memberOf65(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, LList(lazy ECons(3, ENil)))))))

@test
def memberOf66(): Bool = 
	not (LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy ENil)))))))

@test
def memberOf67(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, LCons(3, lazy LList(lazy ENil)))))))

@test
def memberOf68(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, LList(lazy ENil))))))))

@test
def memberOf69(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, ECons(3, ENil)))))))

@test
def memberOf70(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy ECons(3, ENil)))))))

@test
def memberOf71(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil)))))))

@test
def memberOf72(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))))))

@test
def memberOf73(): Bool = 
	not (LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy ENil))))))

@test
def memberOf74(): Bool = 
	not (LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil)))))

@test
def memberOf75(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy ENil)))))))

@test
def memberOf76(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ENil))))))))

@test
def memberOf77(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil))))))))

@test
def memberOf78(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy ENil)))))))

@test
def memberOf79(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ENil)))))))))

@test
def memberOf80(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))))

@test
def memberOf81(): Bool = 
	not (LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy ENil)))))))

@test
def memberOf82(): Bool = 
	not (LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, ENil))))))

@test
def memberOf83(): Bool = 
	not (LazyList1.memberOf(0, LCons(1, lazy ECons(2, ECons(3, ENil)))))

@test
def memberOf84(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy LCons(3, lazy ENil))))))

@test
def memberOf85(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy ENil)))))))

@test
def memberOf86(): Bool = 
	not (LazyList1.memberOf(0, ECons(1, LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil)))))))

@test
def memberOf87(): Bool = 
	not (LazyList1.memberOf(0, ECons(1, LCons(2, lazy ECons(3, LList(lazy ENil))))))

@test
def memberOf88(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, ECons(3, LList(lazy ENil)))))))

@test
def memberOf89(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil))))))

@test
def memberOf90(): Bool = 
	not (LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil)))))))

@test
def memberOf91(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, ENil))))))))

@test
def memberOf92(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy LCons(3, lazy LList(lazy ENil)))))))

@test
def memberOf93(): Bool = 
	not (LazyList1.memberOf(0, LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy ENil))))))

@test
def memberOf94(): Bool = 
	not (LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, ENil)))))))

@test
def memberOf95(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil))))))))

@test
def memberOf96(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, ECons(3, ENil))))))

@test
def memberOf97(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))))))

@test
def memberOf98(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy ECons(3, LList(lazy ENil))))))))

@test
def memberOf99(): Bool = 
	not (LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ENil))))))))

@test
def memberOf100(): Bool = 
	not (LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy ECons(3, ENil))))))

@test
def memberOf101(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy ECons(3, ENil))))))

@test
def memberOf102(): Bool = 
	not (LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy LCons(3, lazy LList(lazy ENil))))))

@test
def memberOf103(): Bool = 
	not (LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, ECons(3, ENil))))))

@test
def memberOf104(): Bool = 
	not (LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil))))))))

@test
def memberOf105(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, ENil)))))))

@test
def memberOf106(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, ENil)))))))

@test
def memberOf107(): Bool = 
	not (LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, LCons(3, lazy ENil))))))

@test
def memberOf108(): Bool = 
	not (LazyList1.memberOf(0, ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy ENil))))))

@test
def memberOf109(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, ENil)))))))

@test
def memberOf110(): Bool = 
	not (LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy ENil)))))))

@test
def memberOf111(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, ECons(3, LList(lazy ENil)))))))

@test
def memberOf112(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy ENil))))))))

@test
def memberOf113(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy LList(lazy ECons(3, ENil)))))))

@test
def memberOf114(): Bool = 
	not (LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, LCons(3, lazy LList(lazy ENil)))))))

@test
def memberOf115(): Bool = 
	not (LazyList1.memberOf(0, LCons(1, lazy ECons(2, LList(lazy ECons(3, LList(lazy ENil)))))))

@test
def memberOf116(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, LList(lazy ECons(3, LList(lazy ENil))))))))

@test
def memberOf117(): Bool = 
	not (LazyList1.memberOf(0, ECons(1, LCons(2, lazy LList(lazy ECons(3, ENil))))))

@test
def memberOf118(): Bool = 
	not (LazyList1.memberOf(0, ECons(1, LCons(2, lazy LCons(3, lazy ENil)))))

@test
def memberOf119(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy ECons(3, LList(lazy ENil)))))))

@test
def memberOf120(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, LList(lazy ECons(3, ENil)))))))

@test
def memberOf121(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy ENil))))))))

@test
def memberOf122(): Bool = 
	not (LazyList1.memberOf(0, ECons(1, ECons(2, LList(lazy ECons(3, LList(lazy ENil)))))))

@test
def memberOf123(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, LCons(3, lazy ENil))))))

@test
def memberOf124(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ENil)))))))))

@test
def memberOf125(): Bool = 
	not (LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, ENil))))))

@test
def memberOf126(): Bool = 
	not (LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, LList(lazy ENil)))))))

@test
def memberOf127(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, LCons(3, lazy LList(lazy ENil))))))))

@test
def memberOf128(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil))))))))

@test
def memberOf129(): Bool = 
	not (LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy LList(lazy ENil)))))))

@test
def memberOf130(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy ENil)))))))

@test
def memberOf131(): Bool = 
	not (LazyList1.memberOf(0, ECons(1, ECons(2, LList(lazy ECons(3, ENil))))))

@test
def memberOf132(): Bool = 
	not (LazyList1.memberOf(0, LCons(1, lazy ECons(2, LList(lazy ECons(3, ENil))))))

@test
def memberOf133(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy ECons(3, LList(lazy ENil)))))))

@test
def memberOf134(): Bool = 
	not (LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil)))))))

@test
def memberOf135(): Bool = 
	not (LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil))))))))

@test
def memberOf136(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, LList(lazy LCons(3, lazy ENil)))))))

@test
def memberOf137(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, LList(lazy ECons(3, LList(lazy ENil))))))))

@test
def memberOf138(): Bool = 
	not (LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, ENil))))))))
@test
def memberOf139(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy ECons(2, LList(lazy ECons(3, ECons(0, ENil))))))

@test
def memberOf140(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy ECons(3, LCons(0, lazy ENil)))))

@test
def memberOf141(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, LList(lazy ECons(3, ECons(0, LList(lazy ENil))))))))

@test
def memberOf142(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy LCons(0, lazy ENil)))))))

@test
def memberOf143(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, ECons(0, ENil)))))))

@test
def memberOf144(): Bool = 
	LazyList1.memberOf(0, ECons(1, ECons(2, LList(lazy LCons(3, lazy LList(lazy ECons(0, ENil)))))))

@test
def memberOf145(): Bool = 
	LazyList1.memberOf(0, ECons(1, LCons(2, lazy LCons(3, lazy LList(lazy ECons(0, ENil))))))

@test
def memberOf146(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy ECons(0, ENil))))))

@test
def memberOf147(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, ECons(3, LList(lazy LCons(0, lazy ENil)))))))

@test
def memberOf148(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, ECons(0, LList(lazy ENil)))))))

@test
def memberOf149(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, ECons(0, LList(lazy ENil))))))))

@test
def memberOf150(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy LCons(0, lazy ENil))))))))

@test
def memberOf151(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy LCons(0, lazy ENil)))))))

@test
def memberOf152(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, ECons(0, LList(lazy ENil)))))))))

@test
def memberOf153(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy LList(lazy LCons(0, lazy ENil)))))))

@test
def memberOf154(): Bool = 
	LazyList1.memberOf(0, ECons(1, LCons(2, lazy LList(lazy ECons(3, ECons(0, ENil))))))

@test
def memberOf155(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, LList(lazy ECons(3, ECons(0, LList(lazy ENil))))))))

@test
def memberOf156(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, ECons(3, ECons(0, ENil)))))))

@test
def memberOf157(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy LCons(0, lazy ENil))))))

@test
def memberOf158(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, ECons(3, LList(lazy ECons(0, LList(lazy ENil))))))))

@test
def memberOf159(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy ECons(0, ENil)))))))

@test
def memberOf160(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, ECons(3, LList(lazy ECons(0, ENil)))))))

@test
def memberOf161(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf162(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy LCons(0, lazy ENil)))))))

@test
def memberOf163(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy ECons(0, LList(lazy ENil))))))))

@test
def memberOf164(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf165(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy LCons(0, lazy ENil))))))))

@test
def memberOf166(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, LCons(0, lazy ENil))))))

@test
def memberOf167(): Bool = 
	LazyList1.memberOf(0, ECons(1, ECons(2, ECons(3, LList(lazy LCons(0, lazy ENil))))))

@test
def memberOf168(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy ECons(0, LList(lazy ENil))))))))

@test
def memberOf169(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ECons(0, LList(lazy ENil))))))))))

@test
def memberOf170(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy ECons(3, ECons(0, ENil))))))

@test
def memberOf171(): Bool = 
	LazyList1.memberOf(0, ECons(1, ECons(2, ECons(3, LCons(0, lazy LList(lazy ENil))))))

@test
def memberOf172(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ECons(0, LList(lazy ENil)))))))))

@test
def memberOf173(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, LList(lazy ECons(3, LList(lazy ECons(0, ENil))))))))

@test
def memberOf174(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, LCons(3, lazy LCons(0, lazy LList(lazy ENil)))))))

@test
def memberOf175(): Bool = 
	LazyList1.memberOf(0, ECons(1, ECons(2, LCons(3, lazy ECons(0, LList(lazy ENil))))))

@test
def memberOf176(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf177(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy ECons(0, ENil))))))

@test
def memberOf178(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, ECons(3, LList(lazy LCons(0, lazy ENil))))))))

@test
def memberOf179(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, LCons(0, lazy LList(lazy ENil)))))))

@test
def memberOf180(): Bool = 
	LazyList1.memberOf(0, ECons(1, ECons(2, LCons(3, lazy LCons(0, lazy ENil)))))

@test
def memberOf181(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, LList(lazy LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf182(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy LCons(0, lazy ENil)))))))))

@test
def memberOf183(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, LCons(3, lazy LList(lazy ECons(0, LList(lazy ENil)))))))))

@test
def memberOf184(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, ECons(3, LList(lazy LCons(0, lazy LList(lazy ENil)))))))))

@test
def memberOf185(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, LCons(0, lazy LList(lazy ENil)))))))))

@test
def memberOf186(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ECons(0, LList(lazy ENil))))))))

@test
def memberOf187(): Bool = 
	LazyList1.memberOf(0, ECons(1, LCons(2, lazy LList(lazy ECons(3, LList(lazy ECons(0, LList(lazy ENil))))))))

@test
def memberOf188(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, LCons(3, lazy ECons(0, LList(lazy ENil)))))))

@test
def memberOf189(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, LCons(3, lazy LCons(0, lazy ENil))))))

@test
def memberOf190(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy ECons(3, LList(lazy ECons(0, LList(lazy ENil))))))))

@test
def memberOf191(): Bool = 
	LazyList1.memberOf(0, ECons(1, LCons(2, lazy LList(lazy ECons(3, LCons(0, lazy ENil))))))

@test
def memberOf192(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, ECons(3, ECons(0, LList(lazy ENil)))))))

@test
def memberOf193(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy LCons(0, lazy LList(lazy ENil)))))))))

@test
def memberOf194(): Bool = 
	LazyList1.memberOf(0, ECons(1, LCons(2, lazy LCons(3, lazy ECons(0, ENil)))))

@test
def memberOf195(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, ECons(3, LList(lazy ECons(0, LList(lazy ENil))))))))

@test
def memberOf196(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy ECons(3, ECons(0, ENil)))))))

@test
def memberOf197(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy LList(lazy ECons(3, LCons(0, lazy ENil)))))))

@test
def memberOf198(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, LCons(3, lazy ECons(0, LList(lazy ENil))))))))

@test
def memberOf199(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy ECons(3, LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf200(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, LList(lazy LCons(3, lazy ECons(0, LList(lazy ENil))))))))

@test
def memberOf201(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf202(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ECons(0, LList(lazy ENil)))))))))

@test
def memberOf203(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy ECons(0, LList(lazy ENil)))))))))
*/
}