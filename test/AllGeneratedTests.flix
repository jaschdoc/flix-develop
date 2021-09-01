namespace AllGeneratedTests {

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

}
