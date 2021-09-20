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

namespace AllGeneratedTests {/*

@test
def memberOf204(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy ECons(0, LList(lazy ENil))))))))

@test
def memberOf205(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ECons(0, ENil)))))))

@test
def memberOf206(): Bool = 
	LazyList1.memberOf(0, ECons(1, LCons(2, lazy ECons(3, ECons(0, ENil)))))

@test
def memberOf207(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, LList(lazy ECons(3, LList(lazy ECons(0, ENil))))))))

@test
def memberOf208(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy LCons(0, lazy LList(lazy ENil)))))))))

@test
def memberOf209(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ECons(0, LList(lazy ENil)))))))))

@test
def memberOf210(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy LList(lazy ECons(3, LList(lazy ECons(0, LList(lazy ENil)))))))))

@test
def memberOf211(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LCons(0, lazy LList(lazy ENil)))))))))

@test
def memberOf212(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy LCons(0, lazy ENil)))))))))

@test
def memberOf213(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, LList(lazy ECons(3, LCons(0, lazy ENil)))))))

@test
def memberOf214(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, ECons(3, LList(lazy ECons(0, ENil))))))))

@test
def memberOf215(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy LCons(3, lazy LList(lazy LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf216(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy ECons(0, ENil))))))))

@test
def memberOf217(): Bool = 
	LazyList1.memberOf(0, ECons(1, ECons(2, LList(lazy LCons(3, lazy ECons(0, LList(lazy ENil)))))))

@test
def memberOf218(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy LList(lazy ECons(3, ECons(0, ENil)))))))

@test
def memberOf219(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, ECons(3, LList(lazy LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf220(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ECons(0, ENil))))))))

@test
def memberOf221(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy LCons(3, lazy LList(lazy LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf222(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ECons(0, LList(lazy ENil)))))))))

@test
def memberOf223(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LCons(0, lazy LList(lazy ENil)))))))))

@test
def memberOf224(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy LList(lazy ECons(0, ENil))))))))

@test
def memberOf225(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf226(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy ECons(2, ECons(3, LList(lazy ECons(0, ENil))))))

@test
def memberOf227(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, LCons(3, lazy LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf228(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy LCons(3, lazy ECons(0, LList(lazy ENil)))))))

@test
def memberOf229(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy ECons(2, LCons(3, lazy LCons(0, lazy LList(lazy ENil))))))

@test
def memberOf230(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy ECons(0, LList(lazy ENil)))))))))

@test
def memberOf231(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, ECons(3, ECons(0, ENil))))))

@test
def memberOf232(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, ECons(3, ECons(0, LList(lazy ENil)))))))

@test
def memberOf233(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy ECons(3, LList(lazy LCons(0, lazy ENil))))))

@test
def memberOf234(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(0, lazy LList(lazy ENil)))))))))

@test
def memberOf235(): Bool = 
	LazyList1.memberOf(0, ECons(1, LCons(2, lazy LCons(3, lazy LCons(0, lazy LList(lazy ENil))))))

@test
def memberOf236(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy ECons(3, LList(lazy ECons(0, ENil))))))

@test
def memberOf237(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, LList(lazy LCons(3, lazy LList(lazy LCons(0, lazy ENil))))))))

@test
def memberOf238(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy LCons(0, lazy LList(lazy ENil))))))))))

@test
def memberOf239(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LCons(0, lazy ENil))))))))

@test
def memberOf240(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LCons(0, lazy ENil)))))))

@test
def memberOf241(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy ECons(2, LList(lazy ECons(3, LList(lazy ECons(0, ENil)))))))

@test
def memberOf242(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy LCons(3, lazy ECons(0, ENil)))))

@test
def memberOf243(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, ECons(3, LCons(0, lazy ENil))))))

@test
def memberOf244(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf245(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, ECons(3, ECons(0, ENil))))))

@test
def memberOf246(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf247(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, LList(lazy LCons(0, lazy ENil))))))))

@test
def memberOf248(): Bool = 
	LazyList1.memberOf(0, ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy ECons(0, ENil))))))

@test
def memberOf249(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy ECons(3, ECons(0, ENil)))))

@test
def memberOf250(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ECons(0, LList(lazy ENil))))))))))

@test
def memberOf251(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ECons(0, ENil)))))))))

@test
def memberOf252(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf253(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf254(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, ECons(3, LCons(0, lazy ENil))))))

@test
def memberOf255(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, ECons(3, LList(lazy ECons(0, ENil)))))))

@test
def memberOf256(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, LList(lazy ECons(3, LList(lazy LCons(0, lazy ENil))))))))

@test
def memberOf257(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ECons(0, ENil))))))

@test
def memberOf258(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf259(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, ECons(3, ECons(0, LList(lazy ENil)))))))

@test
def memberOf260(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy LCons(0, lazy ENil))))))

@test
def memberOf261(): Bool = 
	LazyList1.memberOf(0, ECons(1, ECons(2, LList(lazy LCons(3, lazy LCons(0, lazy LList(lazy ENil)))))))

@test
def memberOf262(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, ECons(3, LList(lazy ECons(0, LList(lazy ENil))))))))

@test
def memberOf263(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy LCons(3, lazy LCons(0, lazy ENil))))))

@test
def memberOf264(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy ECons(3, LCons(0, lazy ENil))))))

@test
def memberOf265(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy ECons(2, LList(lazy ECons(3, LCons(0, lazy ENil))))))

@test
def memberOf266(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ECons(0, LList(lazy ENil))))))))))

@test
def memberOf267(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy ECons(3, LList(lazy LCons(0, lazy ENil))))))))

@test
def memberOf268(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, LList(lazy ECons(3, ECons(0, ENil)))))))

@test
def memberOf269(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(0, lazy LList(lazy ENil))))))))))

@test
def memberOf270(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy ECons(2, LCons(3, lazy LCons(0, lazy ENil)))))

@test
def memberOf271(): Bool = 
	LazyList1.memberOf(0, ECons(1, LCons(2, lazy ECons(3, LCons(0, lazy LList(lazy ENil))))))

@test
def memberOf272(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy ECons(0, ENil))))))))

@test
def memberOf273(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy ECons(2, ECons(3, ECons(0, LList(lazy ENil))))))

@test
def memberOf274(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy ECons(0, LList(lazy ENil))))))))

@test
def memberOf275(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy ECons(0, ENil)))))))

@test
def memberOf276(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy ECons(3, LCons(0, lazy ENil))))))

@test
def memberOf277(): Bool = 
	LazyList1.memberOf(0, ECons(1, ECons(2, LCons(3, lazy LList(lazy LCons(0, lazy ENil))))))

@test
def memberOf278(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, LCons(3, lazy LList(lazy LCons(0, lazy ENil)))))))

@test
def memberOf279(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy LCons(3, lazy LList(lazy ECons(0, ENil))))))

@test
def memberOf280(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy LCons(0, lazy ENil)))))))))

@test
def memberOf281(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy LCons(0, lazy ENil)))))))

@test
def memberOf282(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, LList(lazy ECons(3, LList(lazy ECons(0, LList(lazy ENil)))))))))

@test
def memberOf283(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy ECons(3, LCons(0, lazy LList(lazy ENil)))))))

@test
def memberOf284(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LCons(0, lazy ENil))))))))

@test
def memberOf285(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, LList(lazy ECons(0, LList(lazy ENil))))))))

@test
def memberOf286(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, ECons(0, ENil))))))))

@test
def memberOf287(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy LCons(3, lazy ECons(0, LList(lazy ENil))))))

@test
def memberOf288(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, LCons(0, lazy ENil)))))))

@test
def memberOf289(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy ECons(3, ECons(0, ENil))))))

@test
def memberOf290(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, ECons(0, LList(lazy ENil)))))))))

@test
def memberOf291(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, LCons(3, lazy LList(lazy ECons(0, LList(lazy ENil))))))))

@test
def memberOf292(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ECons(0, ENil))))))))

@test
def memberOf293(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy LCons(0, lazy ENil)))))))

@test
def memberOf294(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy LCons(0, lazy ENil)))))))

@test
def memberOf295(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, ECons(3, ECons(0, LList(lazy ENil)))))))

@test
def memberOf296(): Bool = 
	LazyList1.memberOf(0, ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ECons(0, ENil)))))))

@test
def memberOf297(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, ECons(0, ENil))))))))

@test
def memberOf298(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, ECons(0, LList(lazy ENil))))))))

@test
def memberOf299(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, LCons(3, lazy LCons(0, lazy ENil))))))

@test
def memberOf300(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy LCons(0, lazy ENil))))))))

@test
def memberOf301(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy ECons(3, LList(lazy LCons(0, lazy LList(lazy ENil)))))))

@test
def memberOf302(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, LList(lazy ECons(3, LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf303(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy LCons(0, lazy LList(lazy ENil))))))))))

@test
def memberOf304(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, ECons(0, ENil))))))))

@test
def memberOf305(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy ECons(3, LList(lazy ECons(0, ENil)))))))

@test
def memberOf306(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, LList(lazy ECons(3, LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf307(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, ECons(0, ENil)))))))

@test
def memberOf308(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ECons(0, ENil)))))))

@test
def memberOf309(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, LCons(3, lazy LList(lazy LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf310(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy LCons(0, lazy ENil))))))))

@test
def memberOf311(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ECons(0, ENil))))))))

@test
def memberOf312(): Bool = 
	LazyList1.memberOf(0, ECons(1, ECons(2, LCons(3, lazy LList(lazy ECons(0, ENil))))))

@test
def memberOf313(): Bool = 
	LazyList1.memberOf(0, ECons(1, LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf314(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ECons(0, ENil)))))))

@test
def memberOf315(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ECons(0, LList(lazy ENil)))))))))

@test
def memberOf316(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy LCons(0, lazy ENil)))))))

@test
def memberOf317(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, LList(lazy ECons(0, LList(lazy ENil)))))))))

@test
def memberOf318(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ECons(0, LList(lazy ENil))))))))))

@test
def memberOf319(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, ECons(3, LCons(0, lazy ENil))))))

@test
def memberOf320(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy ECons(0, ENil)))))))

@test
def memberOf321(): Bool = 
	LazyList1.memberOf(0, ECons(1, LCons(2, lazy ECons(3, LList(lazy LCons(0, lazy LList(lazy ENil)))))))

@test
def memberOf322(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy LList(lazy LCons(0, lazy ENil))))))))

@test
def memberOf323(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy LCons(0, lazy ENil))))))))

@test
def memberOf324(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, LCons(3, lazy ECons(0, ENil))))))

@test
def memberOf325(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy LCons(3, lazy LList(lazy ECons(0, LList(lazy ENil)))))))

@test
def memberOf326(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ECons(0, ENil))))))))

@test
def memberOf327(): Bool = 
	LazyList1.memberOf(0, ECons(1, LCons(2, lazy LList(lazy ECons(3, LList(lazy ECons(0, ENil)))))))

@test
def memberOf328(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy LCons(3, lazy ECons(0, LList(lazy ENil)))))))

@test
def memberOf329(): Bool = 
	LazyList1.memberOf(0, ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf330(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy LCons(0, lazy LList(lazy ENil)))))))))

@test
def memberOf331(): Bool = 
	LazyList1.memberOf(0, ECons(1, ECons(2, LList(lazy LCons(3, lazy LCons(0, lazy ENil))))))

@test
def memberOf332(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ECons(0, LList(lazy ENil))))))))))

@test
def memberOf333(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ECons(0, LList(lazy ENil))))))))

@test
def memberOf334(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, LList(lazy ECons(3, LList(lazy LCons(0, lazy LList(lazy ENil)))))))))

@test
def memberOf335(): Bool = 
	LazyList1.memberOf(0, ECons(1, ECons(2, LList(lazy LCons(3, lazy LList(lazy ECons(0, LList(lazy ENil))))))))

@test
def memberOf336(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy ECons(0, ENil))))))

@test
def memberOf337(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy LCons(3, lazy LCons(0, lazy ENil)))))

@test
def memberOf338(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy LCons(0, lazy LList(lazy ENil)))))))))

@test
def memberOf339(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy LList(lazy ECons(0, LList(lazy ENil)))))))))

@test
def memberOf340(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy LCons(0, lazy LList(lazy ENil)))))))))

@test
def memberOf341(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy LCons(0, lazy LList(lazy ENil)))))))))

@test
def memberOf342(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy LCons(0, lazy LList(lazy ENil)))))))))

@test
def memberOf343(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy ECons(2, LCons(3, lazy ECons(0, ENil)))))

@test
def memberOf344(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy LCons(0, lazy ENil))))))))

@test
def memberOf345(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy ECons(3, LList(lazy ECons(0, LList(lazy ENil)))))))))

@test
def memberOf346(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy ECons(2, ECons(3, ECons(0, ENil)))))

@test
def memberOf347(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy LCons(0, lazy LList(lazy ENil)))))))))

@test
def memberOf348(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy ECons(3, LList(lazy LCons(0, lazy LList(lazy ENil)))))))))

@test
def memberOf349(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy ECons(2, ECons(3, LList(lazy LCons(0, lazy ENil))))))

@test
def memberOf350(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ECons(0, ENil)))))))))

@test
def memberOf351(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, ECons(3, ECons(0, ENil))))))

@test
def memberOf352(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LCons(0, lazy ENil))))))))

@test
def memberOf353(): Bool = 
	LazyList1.memberOf(0, ECons(1, ECons(2, ECons(3, ECons(0, ENil)))))

@test
def memberOf354(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy ECons(3, LList(lazy LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf355(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy LCons(0, lazy LList(lazy ENil)))))))

@test
def memberOf356(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, LList(lazy LCons(3, lazy LList(lazy ECons(0, ENil))))))))

@test
def memberOf357(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LCons(0, lazy ENil)))))))

@test
def memberOf358(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LCons(0, lazy ENil))))))

@test
def memberOf359(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ECons(0, ENil)))))))

@test
def memberOf360(): Bool = 
	LazyList1.memberOf(0, ECons(1, ECons(2, LList(lazy ECons(3, ECons(0, LList(lazy ENil)))))))

@test
def memberOf361(): Bool = 
	LazyList1.memberOf(0, ECons(1, ECons(2, LList(lazy ECons(3, LList(lazy ECons(0, LList(lazy ENil))))))))

@test
def memberOf362(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, LCons(0, lazy ENil))))))

@test
def memberOf363(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy LList(lazy ECons(3, LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf364(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, LList(lazy ECons(3, LList(lazy LCons(0, lazy ENil))))))))

@test
def memberOf365(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ECons(0, LList(lazy ENil)))))))))

@test
def memberOf366(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LCons(0, lazy LList(lazy ENil)))))))

@test
def memberOf367(): Bool = 
	LazyList1.memberOf(0, ECons(1, ECons(2, ECons(3, LList(lazy ECons(0, ENil))))))

@test
def memberOf368(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LCons(0, lazy ENil))))))))

@test
def memberOf369(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, LCons(0, lazy ENil)))))))

@test
def memberOf370(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy LCons(0, lazy LList(lazy ENil)))))))

@test
def memberOf371(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy LCons(3, lazy LList(lazy LCons(0, lazy ENil)))))))

@test
def memberOf372(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ECons(0, ENil)))))))))

@test
def memberOf373(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy LCons(0, lazy LList(lazy ENil)))))))

@test
def memberOf374(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy ECons(2, LList(lazy ECons(3, LCons(0, lazy LList(lazy ENil)))))))

@test
def memberOf375(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf376(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, ECons(0, ENil))))))))

@test
def memberOf377(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf378(): Bool = 
	LazyList1.memberOf(0, ECons(1, ECons(2, LList(lazy LCons(3, lazy ECons(0, ENil))))))

@test
def memberOf379(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf380(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LCons(0, lazy LList(lazy ENil)))))))

@test
def memberOf381(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy LCons(0, lazy ENil))))))))

@test
def memberOf382(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, LCons(3, lazy LList(lazy LCons(0, lazy LList(lazy ENil)))))))))

@test
def memberOf383(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ECons(0, ENil))))))))

@test
def memberOf384(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy ECons(2, LCons(3, lazy ECons(0, LList(lazy ENil))))))

@test
def memberOf385(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(0, lazy ENil))))))))

@test
def memberOf386(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ECons(0, LList(lazy ENil)))))))))

@test
def memberOf387(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy LCons(3, lazy LCons(0, lazy LList(lazy ENil)))))))

@test
def memberOf388(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LCons(0, lazy ENil)))))))

@test
def memberOf389(): Bool = 
	LazyList1.memberOf(0, ECons(1, LCons(2, lazy ECons(3, LList(lazy LCons(0, lazy ENil))))))

@test
def memberOf390(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, LCons(3, lazy ECons(0, ENil)))))))

@test
def memberOf391(): Bool = 
	LazyList1.memberOf(0, ECons(1, ECons(2, LList(lazy ECons(3, LList(lazy ECons(0, ENil)))))))

@test
def memberOf392(): Bool = 
	LazyList1.memberOf(0, ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy LCons(0, lazy ENil)))))))

@test
def memberOf393(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf394(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ECons(0, ENil)))))))

@test
def memberOf395(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy LCons(0, lazy LList(lazy ENil)))))))

@test
def memberOf396(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy LCons(0, lazy ENil)))))))))

@test
def memberOf397(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, LCons(3, lazy LCons(0, lazy LList(lazy ENil)))))))

@test
def memberOf398(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy LCons(0, lazy LList(lazy ENil)))))))))

@test
def memberOf399(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy LCons(0, lazy ENil)))))))

@test
def memberOf400(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(0, lazy ENil))))))))

@test
def memberOf401(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, LCons(3, lazy LList(lazy LCons(0, lazy ENil)))))))

@test
def memberOf402(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy ECons(2, ECons(3, LList(lazy LCons(0, lazy LList(lazy ENil)))))))

@test
def memberOf403(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, LCons(0, lazy ENil)))))))

@test
def memberOf404(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, ECons(0, ENil))))))

@test
def memberOf405(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf406(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy LList(lazy LCons(0, lazy LList(lazy ENil)))))))))

@test
def memberOf407(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(0, lazy ENil))))))))

@test
def memberOf408(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy LCons(0, lazy ENil)))))))))

@test
def memberOf409(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy ECons(0, ENil))))))))

@test
def memberOf410(): Bool = 
	LazyList1.memberOf(0, ECons(1, ECons(2, LCons(3, lazy ECons(0, ENil)))))

@test
def memberOf411(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf412(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy ECons(0, ENil))))))))

@test
def memberOf413(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy ECons(0, LList(lazy ENil)))))))))

@test
def memberOf414(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy ECons(0, ENil))))))))

@test
def memberOf415(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ECons(0, LList(lazy ENil)))))))

@test
def memberOf416(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy LCons(0, lazy ENil))))))))

@test
def memberOf417(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, LList(lazy ECons(3, LCons(0, lazy ENil)))))))

@test
def memberOf418(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy LCons(0, lazy ENil))))))

@test
def memberOf419(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy ECons(3, LList(lazy LCons(0, lazy ENil)))))))

@test
def memberOf420(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, ECons(0, LList(lazy ENil)))))))

@test
def memberOf421(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy ECons(0, LList(lazy ENil)))))))))

@test
def memberOf422(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy ECons(2, LList(lazy ECons(3, LList(lazy LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf423(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ECons(0, ENil))))))))

@test
def memberOf424(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, ECons(3, LList(lazy LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf425(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy LCons(0, lazy ENil)))))))))

@test
def memberOf426(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, LList(lazy LCons(3, lazy LCons(0, lazy ENil)))))))

@test
def memberOf427(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy LCons(0, lazy LList(lazy ENil))))))))))

@test
def memberOf428(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy LCons(3, lazy LList(lazy ECons(0, LList(lazy ENil))))))))

@test
def memberOf429(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy ECons(0, ENil)))))))

@test
def memberOf430(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LCons(0, lazy ENil))))))

@test
def memberOf431(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy ECons(0, LList(lazy ENil)))))))))

@test
def memberOf432(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy LCons(3, lazy LList(lazy LCons(0, lazy ENil)))))))

@test
def memberOf433(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ECons(0, LList(lazy ENil)))))))))

@test
def memberOf434(): Bool = 
	LazyList1.memberOf(0, ECons(1, LCons(2, lazy ECons(3, LCons(0, lazy ENil)))))

@test
def memberOf435(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy LCons(3, lazy LList(lazy LCons(0, lazy LList(lazy ENil)))))))

@test
def memberOf436(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy ECons(2, ECons(3, LCons(0, lazy ENil)))))

@test
def memberOf437(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf438(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ECons(0, ENil))))))))

@test
def memberOf439(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy LCons(0, lazy LList(lazy ENil))))))))))

@test
def memberOf440(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, ECons(3, LList(lazy LCons(0, lazy ENil)))))))

@test
def memberOf441(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy LCons(3, lazy LList(lazy ECons(0, LList(lazy ENil))))))))

@test
def memberOf442(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ECons(0, LList(lazy ENil))))))))

@test
def memberOf443(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy LCons(0, lazy LList(lazy ENil))))))))))

@test
def memberOf444(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf445(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy LCons(3, lazy LList(lazy ECons(0, ENil)))))))

@test
def memberOf446(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy LCons(3, lazy LCons(0, lazy LList(lazy ENil)))))))

@test
def memberOf447(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy LCons(0, lazy ENil))))))))

@test
def memberOf448(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy ECons(3, LCons(0, lazy LList(lazy ENil))))))

@test
def memberOf449(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, ECons(3, LCons(0, lazy LList(lazy ENil)))))))

@test
def memberOf450(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy ECons(0, ENil)))))))))

@test
def memberOf451(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, ECons(0, LList(lazy ENil))))))))

@test
def memberOf452(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf453(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy ECons(0, LList(lazy ENil)))))))))

@test
def memberOf454(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy ECons(3, LList(lazy ECons(0, LList(lazy ENil)))))))

@test
def memberOf455(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ECons(0, ENil))))))))

@test
def memberOf456(): Bool = 
	LazyList1.memberOf(0, ECons(1, ECons(2, LList(lazy LCons(3, lazy LList(lazy LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf457(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, ECons(0, ENil)))))))

@test
def memberOf458(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf459(): Bool = 
	LazyList1.memberOf(0, ECons(1, LCons(2, lazy LList(lazy ECons(3, ECons(0, LList(lazy ENil)))))))

@test
def memberOf460(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(0, lazy LList(lazy ENil))))))))))

@test
def memberOf461(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, ECons(3, LCons(0, lazy ENil)))))))

@test
def memberOf462(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, ECons(3, ECons(0, LList(lazy ENil))))))))

@test
def memberOf463(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ECons(0, LList(lazy ENil))))))))

@test
def memberOf464(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy ECons(2, ECons(3, LList(lazy ECons(0, LList(lazy ENil)))))))

@test
def memberOf465(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, LCons(3, lazy LList(lazy LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf466(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy ECons(3, LList(lazy LCons(0, lazy ENil)))))))

@test
def memberOf467(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ECons(0, ENil)))))))))

@test
def memberOf468(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, LCons(3, lazy ECons(0, LList(lazy ENil)))))))

@test
def memberOf469(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, ECons(3, LList(lazy LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf470(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(0, lazy ENil)))))))))

@test
def memberOf471(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, LList(lazy ECons(0, ENil))))))))

@test
def memberOf472(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ECons(0, ENil)))))))))

@test
def memberOf473(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, LList(lazy LCons(3, lazy ECons(0, ENil)))))))

@test
def memberOf474(): Bool = 
	LazyList1.memberOf(0, ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy ECons(0, LList(lazy ENil)))))))

@test
def memberOf475(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy ECons(2, ECons(3, LCons(0, lazy LList(lazy ENil))))))

@test
def memberOf476(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, ECons(0, LList(lazy ENil)))))))))

@test
def memberOf477(): Bool = 
	LazyList1.memberOf(0, ECons(1, ECons(2, ECons(3, ECons(0, LList(lazy ENil))))))

@test
def memberOf478(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy ECons(3, ECons(0, LList(lazy ENil)))))))

@test
def memberOf479(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy ECons(3, ECons(0, ENil))))))

@test
def memberOf480(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, LCons(3, lazy LCons(0, lazy ENil))))))

@test
def memberOf481(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, ECons(3, LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf482(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, ECons(0, LList(lazy ENil))))))))

@test
def memberOf483(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy ECons(0, ENil))))))

@test
def memberOf484(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy ECons(0, ENil)))))))

@test
def memberOf485(): Bool = 
	LazyList1.memberOf(0, ECons(1, LCons(2, lazy LCons(3, lazy LList(lazy LCons(0, lazy ENil))))))

@test
def memberOf486(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, ECons(3, LCons(0, lazy LList(lazy ENil)))))))

@test
def memberOf487(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy LCons(0, lazy ENil)))))))

@test
def memberOf488(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy ECons(3, LList(lazy LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf489(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(0, lazy LList(lazy ENil)))))))))

@test
def memberOf490(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy ECons(2, LList(lazy ECons(3, ECons(0, LList(lazy ENil)))))))

@test
def memberOf491(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ECons(0, LList(lazy ENil)))))))

@test
def memberOf492(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy ECons(3, LList(lazy ECons(0, ENil))))))))

@test
def memberOf493(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy ECons(0, LList(lazy ENil))))))))

@test
def memberOf494(): Bool = 
	LazyList1.memberOf(0, ECons(1, LCons(2, lazy LCons(3, lazy ECons(0, LList(lazy ENil))))))

@test
def memberOf495(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy LCons(3, lazy LList(lazy LCons(0, lazy ENil))))))

@test
def memberOf496(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, LCons(3, lazy LCons(0, lazy ENil)))))))

@test
def memberOf497(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, LList(lazy ECons(3, ECons(0, ENil)))))))

@test
def memberOf498(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, LList(lazy LCons(0, lazy LList(lazy ENil)))))))))

@test
def memberOf499(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, LList(lazy LCons(0, lazy ENil)))))))

@test
def memberOf500(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LCons(0, lazy ENil)))))))

@test
def memberOf501(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf502(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy LCons(3, lazy LList(lazy ECons(0, ENil)))))))

@test
def memberOf503(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy ECons(3, LCons(0, lazy LList(lazy ENil)))))))

@test
def memberOf504(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, LList(lazy ECons(3, LList(lazy LCons(0, lazy LList(lazy ENil)))))))))

@test
def memberOf505(): Bool = 
	LazyList1.memberOf(0, ECons(1, ECons(2, LList(lazy ECons(3, LList(lazy LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf506(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy LCons(0, lazy LList(lazy ENil)))))))))

@test
def memberOf507(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LCons(0, lazy ENil)))))))

@test
def memberOf508(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, LCons(3, lazy LList(lazy ECons(0, ENil))))))))

@test
def memberOf509(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, ECons(0, ENil)))))))

@test
def memberOf510(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy ECons(0, LList(lazy ENil)))))))))

@test
def memberOf511(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, ECons(3, LCons(0, lazy LList(lazy ENil)))))))

@test
def memberOf512(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy ECons(0, ENil)))))))

@test
def memberOf513(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, ECons(3, LCons(0, lazy ENil))))))

@test
def memberOf514(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, LCons(3, lazy LList(lazy ECons(0, ENil)))))))

@test
def memberOf515(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ECons(0, LList(lazy ENil))))))))))

@test
def memberOf516(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy ECons(0, ENil)))))))

@test
def memberOf517(): Bool = 
	LazyList1.memberOf(0, ECons(1, LCons(2, lazy LCons(3, lazy LList(lazy ECons(0, LList(lazy ENil)))))))

@test
def memberOf518(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy LCons(0, lazy ENil))))))

@test
def memberOf519(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, ECons(0, ENil)))))))

@test
def memberOf520(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy ECons(0, LList(lazy ENil)))))))

@test
def memberOf521(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, ECons(0, LList(lazy ENil))))))))

@test
def memberOf522(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf523(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LCons(0, lazy LList(lazy ENil)))))))))

@test
def memberOf524(): Bool = 
	LazyList1.memberOf(0, ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy LCons(0, lazy ENil))))))

@test
def memberOf525(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy ECons(0, LList(lazy ENil))))))))

@test
def memberOf526(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy LCons(3, lazy ECons(0, ENil))))))

@test
def memberOf527(): Bool = 
	LazyList1.memberOf(0, ECons(1, LCons(2, lazy LList(lazy ECons(3, LCons(0, lazy LList(lazy ENil)))))))

@test
def memberOf528(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy ECons(0, LList(lazy ENil))))))))

@test
def memberOf529(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy LList(lazy ECons(0, ENil)))))))

@test
def memberOf530(): Bool = 
	LazyList1.memberOf(0, ECons(1, ECons(2, LList(lazy ECons(3, LCons(0, lazy LList(lazy ENil)))))))

@test
def memberOf531(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LCons(0, lazy ENil)))))))

@test
def memberOf532(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy LList(lazy ECons(3, LList(lazy ECons(0, ENil))))))))

@test
def memberOf533(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy LCons(0, lazy LList(lazy ENil)))))))))

@test
def memberOf534(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, ECons(3, LList(lazy ECons(0, ENil)))))))

@test
def memberOf535(): Bool = 
	LazyList1.memberOf(0, ECons(1, LCons(2, lazy ECons(3, LList(lazy ECons(0, LList(lazy ENil)))))))

@test
def memberOf536(): Bool = 
	LazyList1.memberOf(0, ECons(1, ECons(2, LCons(3, lazy LCons(0, lazy LList(lazy ENil))))))

@test
def memberOf537(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy LCons(3, lazy LCons(0, lazy ENil))))))

@test
def memberOf538(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy LList(lazy LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf539(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, LCons(0, lazy LList(lazy ENil)))))))

@test
def memberOf540(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(0, lazy LList(lazy ENil)))))))))

@test
def memberOf541(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, LCons(3, lazy LList(lazy ECons(0, LList(lazy ENil))))))))

@test
def memberOf542(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ECons(0, LList(lazy ENil)))))))))

@test
def memberOf543(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy ECons(0, ENil))))))))

@test
def memberOf544(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ECons(0, ENil))))))

@test
def memberOf545(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, LCons(0, lazy ENil))))))))

@test
def memberOf546(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, ECons(0, LList(lazy ENil)))))))))

@test
def memberOf547(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf548(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, LCons(3, lazy LCons(0, lazy LList(lazy ENil)))))))

@test
def memberOf549(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ECons(0, ENil)))))))))

@test
def memberOf550(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, ECons(3, LCons(0, lazy LList(lazy ENil)))))))

@test
def memberOf551(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf552(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy ECons(0, ENil))))))))

@test
def memberOf553(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy ECons(0, LList(lazy ENil))))))))

@test
def memberOf554(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy LCons(0, lazy ENil)))))))

@test
def memberOf555(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy LList(lazy ECons(3, ECons(0, LList(lazy ENil))))))))

@test
def memberOf556(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy ECons(0, LList(lazy ENil)))))))

@test
def memberOf557(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, ECons(0, ENil))))))

@test
def memberOf558(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy ECons(3, LCons(0, lazy ENil))))))

@test
def memberOf559(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy LCons(0, lazy ENil))))))))

@test
def memberOf560(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy LCons(0, lazy ENil))))))))

@test
def memberOf561(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy ECons(3, LCons(0, lazy LList(lazy ENil)))))))

@test
def memberOf562(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LCons(0, lazy LList(lazy ENil)))))))))

@test
def memberOf563(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy LCons(0, lazy ENil)))))))

@test
def memberOf564(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, LCons(3, lazy LList(lazy LCons(0, lazy ENil))))))))

@test
def memberOf565(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy ECons(3, LList(lazy ECons(0, LList(lazy ENil))))))))

@test
def memberOf566(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy LCons(0, lazy LList(lazy ENil)))))))))

@test
def memberOf567(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(0, lazy ENil)))))))

@test
def memberOf568(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy ECons(0, LList(lazy ENil))))))))))

@test
def memberOf569(): Bool = 
	LazyList1.memberOf(0, ECons(1, ECons(2, LCons(3, lazy LList(lazy LCons(0, lazy LList(lazy ENil)))))))

@test
def memberOf570(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, LCons(0, lazy ENil))))))))

@test
def memberOf571(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy ECons(3, LList(lazy ECons(0, ENil)))))))

@test
def memberOf572(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy ECons(3, ECons(0, LList(lazy ENil))))))

@test
def memberOf573(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, LCons(0, lazy LList(lazy ENil)))))))))

@test
def memberOf574(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, LCons(0, lazy ENil)))))))

@test
def memberOf575(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ECons(0, LList(lazy ENil)))))))))

@test
def memberOf576(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy ECons(0, ENil)))))))

@test
def memberOf577(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, LList(lazy ECons(3, LList(lazy ECons(0, LList(lazy ENil)))))))))

@test
def memberOf578(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ECons(0, LList(lazy ENil))))))))

@test
def memberOf579(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, LList(lazy LCons(3, lazy LList(lazy ECons(0, LList(lazy ENil)))))))))

@test
def memberOf580(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy ECons(3, LList(lazy ECons(0, LList(lazy ENil))))))))

@test
def memberOf581(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy LCons(0, lazy ENil))))))))

@test
def memberOf582(): Bool = 
	LazyList1.memberOf(0, ECons(1, LCons(2, lazy LCons(3, lazy LList(lazy LCons(0, lazy LList(lazy ENil)))))))

@test
def memberOf583(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy ECons(0, ENil))))))))

@test
def memberOf584(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy ECons(0, ENil)))))))))

@test
def memberOf585(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy ECons(3, ECons(0, LList(lazy ENil)))))))

@test
def memberOf586(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy ECons(0, LList(lazy ENil)))))))))

@test
def memberOf587(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy LCons(0, lazy ENil)))))))

@test
def memberOf588(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, ECons(0, ENil)))))))

@test
def memberOf589(): Bool = 
	LazyList1.memberOf(0, ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy LCons(0, lazy LList(lazy ENil)))))))

@test
def memberOf590(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy LCons(3, lazy ECons(0, ENil))))))

@test
def memberOf591(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, LList(lazy ECons(0, ENil)))))))

@test
def memberOf592(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ECons(0, ENil)))))))

@test
def memberOf593(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, LCons(3, lazy ECons(0, LList(lazy ENil)))))))

@test
def memberOf594(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy LCons(0, lazy ENil))))))))

@test
def memberOf595(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, LList(lazy LCons(3, lazy LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf596(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy ECons(3, LList(lazy LCons(0, lazy ENil)))))))

@test
def memberOf597(): Bool = 
	LazyList1.memberOf(0, ECons(1, ECons(2, LList(lazy ECons(3, LList(lazy LCons(0, lazy ENil)))))))

@test
def memberOf598(): Bool = 
	LazyList1.memberOf(0, ECons(1, ECons(2, LList(lazy LCons(3, lazy LList(lazy LCons(0, lazy ENil)))))))

@test
def memberOf599(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, LCons(3, lazy LList(lazy ECons(0, ENil)))))))

@test
def memberOf600(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ECons(0, LList(lazy ENil))))))))

@test
def memberOf601(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ECons(0, ENil))))))))

@test
def memberOf602(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, ECons(0, LList(lazy ENil))))))))

@test
def memberOf603(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy ECons(0, LList(lazy ENil)))))))

@test
def memberOf604(): Bool = 
	LazyList1.memberOf(0, ECons(1, LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(0, lazy ENil)))))))

@test
def memberOf605(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy LCons(0, lazy LList(lazy ENil)))))))))

@test
def memberOf606(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy ECons(0, LList(lazy ENil)))))))

@test
def memberOf607(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, ECons(3, LList(lazy LCons(0, lazy ENil)))))))

@test
def memberOf608(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, ECons(0, LList(lazy ENil))))))))

@test
def memberOf609(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy ECons(0, LList(lazy ENil))))))))

@test
def memberOf610(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy ECons(2, ECons(3, ECons(0, ENil))))))

@test
def memberOf611(): Bool = 
	LazyList1.memberOf(0, ECons(1, LCons(2, lazy ECons(3, LList(lazy ECons(0, ENil))))))

@test
def memberOf612(): Bool = 
	LazyList1.memberOf(0, ECons(1, ECons(2, LList(lazy ECons(3, ECons(0, ENil))))))

@test
def memberOf613(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy ECons(0, ENil)))))))

@test
def memberOf614(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy ECons(3, ECons(0, LList(lazy ENil)))))))

@test
def memberOf615(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, LCons(3, lazy ECons(0, ENil))))))

@test
def memberOf616(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy ECons(3, LList(lazy LCons(0, lazy LList(lazy ENil))))))))

@test
def memberOf617(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy ECons(0, ENil)))))))

@test
def memberOf618(): Bool = 
	LazyList1.memberOf(0, ECons(1, LCons(2, lazy ECons(3, ECons(0, LList(lazy ENil))))))

@test
def memberOf619(): Bool = 
	LazyList1.memberOf(0, ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ECons(0, LList(lazy ENil))))))))

@test
def memberOf620(): Bool = 
	LazyList1.memberOf(0, ECons(1, ECons(2, ECons(3, LList(lazy LCons(0, lazy LList(lazy ENil)))))))

@test
def memberOf621(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy ECons(0, ENil)))))))

@test
def memberOf622(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ECons(0, ENil))))))))

@test
def memberOf623(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy LCons(0, lazy ENil))))))))

@test
def memberOf624(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy ECons(0, LList(lazy ENil))))))))))

@test
def memberOf625(): Bool = 
	LazyList1.memberOf(0, ECons(1, LCons(2, lazy LCons(3, lazy LCons(0, lazy ENil)))))

@test
def memberOf626(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LCons(2, lazy LCons(3, lazy LCons(0, lazy LList(lazy ENil))))))

@test
def memberOf627(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ECons(0, LList(lazy ENil)))))))))

@test
def memberOf628(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, ECons(3, LList(lazy ECons(0, LList(lazy ENil)))))))))

@test
def memberOf629(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy ECons(3, LCons(0, lazy ENil)))))))

@test
def memberOf630(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy ECons(0, LList(lazy ENil))))))))

@test
def memberOf631(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LCons(0, lazy ENil)))))))

@test
def memberOf632(): Bool = 
	LazyList1.memberOf(0, ECons(1, ECons(2, ECons(3, LCons(0, lazy ENil)))))

@test
def memberOf633(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy ECons(2, LList(lazy ECons(3, LList(lazy LCons(0, lazy ENil)))))))

@test
def memberOf634(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(0, lazy ENil))))))))

@test
def memberOf635(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy ECons(3, LList(lazy ECons(0, ENil)))))))

@test
def memberOf636(): Bool = 
	LazyList1.memberOf(0, ECons(1, ECons(2, ECons(3, LList(lazy ECons(0, LList(lazy ENil)))))))

@test
def memberOf637(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, ECons(0, ENil)))))))

@test
def memberOf638(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ECons(0, ENil))))))))

@test
def memberOf639(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy LCons(0, lazy LList(lazy ENil))))))))))

@test
def memberOf640(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy LCons(0, lazy LList(lazy ENil)))))))))

@test
def memberOf641(): Bool = 
	LazyList1.memberOf(0, ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy ECons(0, LList(lazy ENil))))))))

@test
def memberOf642(): Bool = 
	LazyList1.memberOf(0, ECons(1, ECons(2, LCons(3, lazy LList(lazy ECons(0, LList(lazy ENil)))))))

@test
def memberOf643(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(0, lazy ENil)))))))))

@test
def memberOf644(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, LList(lazy LCons(3, lazy LList(lazy LCons(0, lazy LList(lazy ENil)))))))))

@test
def memberOf645(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, LList(lazy LCons(2, lazy ECons(3, ECons(0, LList(lazy ENil))))))))

@test
def memberOf646(): Bool = 
	LazyList1.memberOf(0, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(0, lazy LList(lazy ENil)))))))))

@test
def memberOf647(): Bool = 
	LazyList1.memberOf(0, LList(lazy ECons(1, ECons(2, LCons(3, lazy ECons(0, ENil))))))

@test
def memberOf648(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy ECons(2, LList(lazy ECons(3, LList(lazy ECons(0, LList(lazy ENil))))))))

@test
def memberOf649(): Bool = 
	LazyList1.memberOf(0, ECons(1, ECons(2, LList(lazy ECons(3, LCons(0, lazy ENil))))))

@test
def memberOf650(): Bool = 
	LazyList1.memberOf(0, LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy LList(lazy ECons(0, LList(lazy ENil))))))))

*/
}
