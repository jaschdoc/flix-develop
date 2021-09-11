namespace AllGeneratedTests2 {
    
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
def intersperse130(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy ECons(3, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse131(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse132(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy ECons(3, ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse133(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy ECons(3, ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse134(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy LCons(3, lazy LList(lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse135(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, ECons(3, ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse136(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse137(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse138(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse139(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, LCons(3, lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse140(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse141(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse142(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse143(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, ECons(3, LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse144(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse145(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy LList(lazy ECons(3, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse146(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, LCons(3, lazy LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse147(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, LList(lazy ECons(3, LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

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
def intersperse151(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy ECons(3, LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse152(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, LList(lazy ECons(3, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse153(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse154(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, LList(lazy ECons(3, LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse155(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, LCons(3, lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse156(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse157(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse158(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse159(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, LCons(3, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse160(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse161(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse162(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse163(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, LList(lazy ECons(3, ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse164(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, LList(lazy ECons(3, ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse165(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy ECons(3, LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse166(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse167(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse168(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, LList(lazy LCons(3, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse169(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, LList(lazy ECons(3, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

@test
def intersperse170(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))




@test
def intersperse171(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy LCons(3, lazy ECons(4, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse172(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy ECons(3, LList(lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse173(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil)))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse174(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy ECons(3, ECons(4, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse175(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy LCons(4, lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse176(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ECons(4, LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse177(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse178(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, ECons(3, ECons(4, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse179(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, ECons(3, ECons(4, LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse180(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, LList(lazy ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse181(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ECons(4, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse182(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, ECons(3, LList(lazy ECons(4, LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse183(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy ECons(3, LList(lazy ECons(4, LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse184(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse185(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse186(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse187(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy LCons(3, lazy LList(lazy ECons(4, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse188(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy ECons(3, LList(lazy LCons(4, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse189(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse190(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse191(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, ECons(3, LList(lazy ECons(4, ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse192(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse193(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, LList(lazy ECons(3, LList(lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse194(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, LList(lazy ECons(3, ECons(4, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse195(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy ECons(3, ECons(4, LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse196(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse197(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, LCons(3, lazy ECons(4, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse198(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, ECons(4, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse199(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, LList(lazy ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse200(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ECons(4, LList(lazy ENil)))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse201(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy ECons(4, ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse202(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LCons(4, lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse203(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy ECons(3, ECons(4, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse204(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy ECons(4, ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse205(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse206(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy ECons(4, ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse207(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, LCons(3, lazy ECons(4, ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse208(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse209(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, LCons(4, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse210(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, ECons(3, LCons(4, lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse211(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse212(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ECons(4, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse213(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy LList(lazy ECons(3, LList(lazy ECons(4, LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse214(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, LList(lazy ECons(3, LList(lazy ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse215(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, LList(lazy LCons(3, lazy LList(lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse216(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse217(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ECons(4, ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse218(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, ECons(3, LList(lazy LCons(4, lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse219(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse220(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy LList(lazy ECons(3, LCons(4, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse221(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ECons(4, ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse222(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, LList(lazy ECons(3, LList(lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse223(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy ECons(3, LList(lazy LCons(4, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse224(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, LCons(3, lazy LList(lazy LCons(4, lazy LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse225(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy LCons(3, lazy LList(lazy LCons(4, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse226(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse227(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, LCons(4, lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse228(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse229(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy LCons(4, lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse230(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse231(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse232(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy LList(lazy LCons(4, lazy LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse233(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy ECons(3, ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse234(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy ECons(4, ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse235(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, LCons(3, lazy LList(lazy ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse236(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy ECons(3, LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse237(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, ECons(3, ECons(4, LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse238(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy ECons(4, LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse239(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse240(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, ECons(4, ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse241(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, LCons(3, lazy ECons(4, ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse242(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy ECons(3, LList(lazy ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse243(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy ECons(4, LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse244(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ECons(4, LList(lazy ENil)))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse245(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy LCons(4, lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse246(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LCons(4, lazy LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse247(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse248(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy ECons(3, LList(lazy ECons(4, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse249(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse250(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, LCons(3, lazy ECons(4, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse251(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse252(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, LCons(3, lazy ECons(4, LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse253(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy LCons(3, lazy LCons(4, lazy LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse254(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy LList(lazy LCons(4, lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse255(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy ECons(3, ECons(4, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse256(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse257(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, LCons(3, lazy ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse258(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, ECons(4, LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse259(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy LList(lazy ECons(3, LList(lazy ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse260(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil)))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse261(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, LCons(3, lazy ECons(4, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse262(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy ECons(4, LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse263(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy LCons(3, lazy LList(lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse264(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LCons(4, lazy LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse265(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy LCons(4, lazy LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse266(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse267(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse268(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse269(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, LList(lazy ECons(3, ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse270(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, ECons(3, LCons(4, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse271(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy ECons(4, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse272(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, ECons(3, ECons(4, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse273(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse274(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, LList(lazy ECons(3, ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse275(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(4, lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse276(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse277(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy LCons(4, lazy LList(lazy ENil)))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse278(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse279(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse280(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse281(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, LList(lazy ECons(3, LList(lazy LCons(4, lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse282(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy LList(lazy ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse283(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(4, lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse284(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy LCons(4, lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse285(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy LCons(3, lazy LList(lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse286(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse287(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, ECons(3, LList(lazy ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse288(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, ECons(3, LList(lazy ECons(4, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse289(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ECons(4, ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse290(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ECons(4, ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse291(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse292(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse293(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, ECons(3, ECons(4, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse294(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse295(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy LCons(3, lazy LCons(4, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse296(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, ECons(4, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse297(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse298(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy ECons(3, LCons(4, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse299(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse300(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy LCons(3, lazy LCons(4, lazy LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse301(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy ECons(4, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse302(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse303(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ECons(4, ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse304(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse305(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy ECons(3, LCons(4, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse306(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy LList(lazy ECons(3, ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse307(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse308(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse309(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse310(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse311(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse312(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy LCons(4, lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse313(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, LCons(3, lazy LList(lazy LCons(4, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse314(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ECons(4, LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse315(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse316(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, LCons(3, lazy LCons(4, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse317(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse318(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LCons(4, lazy LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse319(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, LCons(3, lazy LList(lazy ECons(4, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse320(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy ECons(3, LCons(4, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse321(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, LList(lazy LCons(3, lazy LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse322(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, LCons(3, lazy LList(lazy LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse323(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy LCons(3, lazy LList(lazy LCons(4, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse324(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy LCons(4, lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse325(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy LList(lazy ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse326(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy ECons(4, LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse327(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse328(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse329(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse330(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, ECons(3, ECons(4, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse331(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, ECons(3, LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse332(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse333(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse334(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy LCons(3, lazy ECons(4, ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse335(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, LCons(3, lazy LCons(4, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse336(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy ECons(4, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse337(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, LCons(3, lazy ECons(4, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse338(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy LList(lazy ECons(3, LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse339(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy LCons(4, lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse340(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy LCons(4, lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse341(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy LCons(3, lazy LList(lazy ECons(4, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse342(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, LList(lazy ECons(3, ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse343(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy ECons(3, LList(lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse344(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, LList(lazy ECons(3, ECons(4, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse345(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy LCons(4, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse346(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ECons(4, ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse347(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, LList(lazy ECons(3, LList(lazy ECons(4, ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse348(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse349(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, LList(lazy ECons(3, LList(lazy ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse350(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy ECons(4, ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse351(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy LCons(4, lazy LList(lazy ENil)))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse352(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy LCons(4, lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse353(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, LList(lazy ECons(3, LList(lazy ECons(4, LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse354(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse355(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, LCons(3, lazy LList(lazy LCons(4, lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse356(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse357(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse358(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ECons(4, ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse359(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy ECons(3, LList(lazy ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse360(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy LCons(3, lazy ECons(4, ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse361(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy ECons(4, LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse362(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy LCons(4, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse363(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy LCons(3, lazy ECons(4, LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse364(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy LCons(4, lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse365(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, ECons(3, LCons(4, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse366(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy LList(lazy ECons(3, LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse367(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy ECons(3, LCons(4, lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse368(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse369(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, LCons(3, lazy LCons(4, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse370(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse371(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, LList(lazy ECons(4, LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse372(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse373(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, ECons(3, ECons(4, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse374(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse375(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse376(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy LCons(3, lazy LCons(4, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse377(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse378(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse379(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse380(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy ECons(4, ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse381(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy ECons(4, LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse382(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy ECons(3, LList(lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse383(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy LList(lazy ECons(4, ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse384(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ECons(4, ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse385(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse386(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LCons(4, lazy LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse387(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy LCons(4, lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse388(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse389(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse390(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ECons(4, ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse391(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, ECons(3, LCons(4, lazy LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse392(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, ECons(3, LList(lazy ECons(4, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse393(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse394(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, LList(lazy ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse395(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse396(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, ECons(3, LList(lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse397(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, ECons(3, ECons(4, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse398(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy ECons(4, ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse399(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, ECons(3, LCons(4, lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse400(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, LCons(3, lazy LList(lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse401(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, LCons(3, lazy LCons(4, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse402(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy LCons(4, lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse403(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse404(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, LList(lazy ECons(3, LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse405(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil)))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse406(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ECons(4, ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse407(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, LList(lazy LCons(3, lazy LList(lazy LCons(4, lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse408(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, LCons(3, lazy ECons(4, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse409(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy ECons(3, LList(lazy ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse410(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse411(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy ECons(3, LList(lazy ECons(4, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse412(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy LCons(3, lazy LCons(4, lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse413(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LCons(4, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse414(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse415(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, ECons(3, LList(lazy ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse416(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy LCons(4, lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse417(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, LCons(3, lazy LList(lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse418(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy LCons(3, lazy LList(lazy ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse419(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(4, lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse420(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, LList(lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse421(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy LCons(4, lazy LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse422(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy LCons(4, lazy LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse423(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse424(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse425(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ECons(4, LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse426(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ECons(4, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse427(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse428(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, LList(lazy LCons(3, lazy LList(lazy ECons(4, LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse429(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, LList(lazy ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse430(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, LList(lazy ECons(3, LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse431(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy LCons(4, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse432(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse433(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy ECons(3, LCons(4, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse434(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse435(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, ECons(3, LCons(4, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse436(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy ECons(3, LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse437(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, ECons(3, LCons(4, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse438(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, ECons(3, ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse439(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse440(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy ECons(3, ECons(4, ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse441(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(4, lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse442(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse443(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ECons(4, ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse444(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, LList(lazy ECons(3, LList(lazy LCons(4, lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse445(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ECons(4, LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse446(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy ECons(4, LList(lazy ENil)))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse447(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse448(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy ECons(3, LList(lazy ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse449(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy LCons(4, lazy LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse450(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, LList(lazy ECons(3, ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse451(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy LCons(3, lazy ECons(4, LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse452(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse453(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, LList(lazy LCons(3, lazy LCons(4, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse454(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, LCons(3, lazy LCons(4, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse455(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy LCons(4, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse456(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse457(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse458(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy ECons(3, ECons(4, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse459(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse460(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy ECons(3, LCons(4, lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse461(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, LList(lazy LCons(3, lazy LCons(4, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse462(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, ECons(4, LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse463(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, LCons(3, lazy LList(lazy ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse464(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ECons(4, LList(lazy ENil)))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse465(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse466(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, LList(lazy LCons(3, lazy ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse467(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, ECons(3, LList(lazy ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse468(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, ECons(3, ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse469(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy LCons(3, lazy LList(lazy ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse470(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, ECons(4, ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse471(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ECons(4, LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse472(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse473(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse474(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy ECons(3, LCons(4, lazy LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse475(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, LCons(3, lazy LList(lazy ECons(4, ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse476(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, LCons(3, lazy LCons(4, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse477(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LCons(4, lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse478(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LCons(4, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse479(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse480(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy LCons(3, lazy LList(lazy ECons(4, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse481(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy LCons(4, lazy LList(lazy ENil)))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse482(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ECons(4, ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse483(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy LCons(4, lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse484(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, ECons(3, LList(lazy LCons(4, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse485(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, LCons(3, lazy LCons(4, lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse486(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy LCons(3, lazy LList(lazy LCons(4, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse487(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy LList(lazy LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse488(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LCons(4, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse489(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse490(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, ECons(3, ECons(4, ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse491(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy LList(lazy ECons(3, LList(lazy ECons(4, ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse492(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, LList(lazy ECons(3, LCons(4, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse493(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy LCons(3, lazy ECons(4, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse494(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse495(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse496(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy ECons(3, LCons(4, lazy LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse497(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, LList(lazy ECons(3, LList(lazy ECons(4, LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse498(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse499(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy ECons(4, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse500(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy LCons(4, lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse501(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse502(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ECons(4, LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse503(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, LList(lazy ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse504(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy LCons(3, lazy ECons(4, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse505(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil)))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse506(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, LCons(4, lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse507(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy LCons(3, lazy LList(lazy ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse508(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, ECons(3, LList(lazy LCons(4, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse509(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, ECons(4, ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse510(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy LList(lazy ECons(3, ECons(4, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse511(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, ECons(3, LList(lazy ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse512(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, ECons(3, LList(lazy ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse513(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse514(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, ECons(4, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse515(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse516(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy ECons(3, LList(lazy ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse517(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, LCons(4, lazy LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse518(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LCons(4, lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse519(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ECons(4, LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse520(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ECons(4, LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse521(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, ECons(3, ECons(4, ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse522(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, LList(lazy ECons(3, LCons(4, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse523(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ECons(4, LList(lazy ENil)))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse524(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy LCons(3, lazy LList(lazy ECons(4, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse525(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy ECons(4, LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse526(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, LCons(3, lazy LList(lazy LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse527(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy ECons(4, ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse528(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, LCons(3, lazy LList(lazy ECons(4, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse529(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy LCons(4, lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse530(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse531(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, LList(lazy ECons(4, ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse532(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse533(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ECons(4, ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse534(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse535(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy LCons(3, lazy LCons(4, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse536(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, LList(lazy LCons(3, lazy ECons(4, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse537(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse538(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy LList(lazy ECons(4, LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse539(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, ECons(4, ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse540(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse541(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, LList(lazy LCons(3, lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse542(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, LCons(3, lazy LList(lazy ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse543(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, LCons(3, lazy LCons(4, lazy LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse544(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, LList(lazy LCons(3, lazy LList(lazy ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse545(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, LList(lazy LCons(3, lazy ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse546(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy LCons(4, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse547(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy LCons(4, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse548(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ECons(4, ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse549(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy LCons(3, lazy LList(lazy LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse550(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ECons(4, LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse551(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy ECons(3, LList(lazy ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse552(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy LCons(3, lazy LList(lazy LCons(4, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse553(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, LList(lazy LCons(3, lazy LList(lazy ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse554(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse555(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, LList(lazy ECons(3, ECons(4, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse556(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy LCons(4, lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse557(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy LCons(4, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse558(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, LCons(3, lazy ECons(4, LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse559(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy ECons(3, ECons(4, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse560(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy LCons(4, lazy LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse561(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy LCons(3, lazy ECons(4, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse562(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy ECons(3, ECons(4, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse563(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, LList(lazy LCons(4, lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse564(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse565(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse566(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse567(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse568(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ECons(4, ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse569(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, LList(lazy LCons(3, lazy LList(lazy ECons(4, ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse570(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(4, lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse571(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, LList(lazy ECons(3, LCons(4, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse572(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, LCons(4, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse573(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy ECons(3, ECons(4, LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse574(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy LList(lazy ECons(3, LCons(4, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse575(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse576(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy LCons(4, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse577(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, LList(lazy ECons(3, LList(lazy ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse578(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse579(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse580(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy ECons(4, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse581(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ECons(4, LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse582(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy ECons(3, LList(lazy LCons(4, lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse583(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ECons(4, LList(lazy ENil)))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse584(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy ECons(4, ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse585(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LCons(4, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse586(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy LCons(4, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse587(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy ECons(4, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse588(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, LList(lazy ECons(3, LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse589(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy ECons(3, ECons(4, ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse590(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy LList(lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse591(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, ECons(3, LCons(4, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse592(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy ECons(3, LCons(4, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse593(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy ECons(3, LList(lazy ECons(4, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse594(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse595(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy LCons(3, lazy LList(lazy ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse596(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, LCons(3, lazy LList(lazy LCons(4, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse597(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy LCons(3, lazy LList(lazy LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse598(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy LCons(4, lazy LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse599(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse600(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, LList(lazy LCons(3, lazy ECons(4, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse601(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, ECons(4, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse602(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy LCons(4, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse603(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy LCons(4, lazy LList(lazy ENil)))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse604(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ECons(4, LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse605(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, ECons(3, LCons(4, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse606(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, LList(lazy LCons(3, lazy LList(lazy LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse607(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, LCons(3, lazy ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse608(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, LCons(3, lazy LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse609(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, ECons(3, LList(lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse610(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, ECons(4, LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse611(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy LCons(4, lazy LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse612(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse613(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse614(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy LList(lazy ECons(3, LList(lazy ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse615(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ECons(4, ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse616(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, LCons(3, lazy ECons(4, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse617(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, LCons(4, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse618(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy ECons(4, LList(lazy ENil)))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse619(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy ECons(4, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse620(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse621(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, LCons(3, lazy LCons(4, lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse622(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy ECons(4, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse623(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse624(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, ECons(3, LList(lazy ECons(4, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse625(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, LCons(3, lazy LList(lazy ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse626(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LCons(2, lazy LList(lazy ECons(3, ECons(4, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse627(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, ECons(3, LCons(4, lazy LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse628(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, LList(lazy ECons(3, LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse629(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, ECons(3, ECons(4, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse630(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy LCons(4, lazy LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse631(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse632(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, ECons(3, LList(lazy ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse633(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse634(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy LCons(4, lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse635(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse636(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, LCons(3, lazy LCons(4, lazy LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse637(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, LList(lazy ECons(3, LCons(4, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse638(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy ECons(3, LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse639(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy ECons(3, LList(lazy ECons(4, ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse640(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(4, lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse641(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LCons(4, lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse642(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy LCons(4, lazy LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse643(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse644(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy LCons(4, lazy LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse645(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy ECons(3, LCons(4, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse646(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, LCons(3, lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse647(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy ECons(4, ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse648(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, ECons(3, LCons(4, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse649(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, ECons(4, LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse650(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, ECons(3, LList(lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse651(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse652(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LCons(2, lazy LCons(3, lazy LCons(4, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse653(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, LList(lazy ECons(3, LList(lazy ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse654(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, LCons(3, lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse655(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, ECons(3, ECons(4, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse656(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy ECons(4, LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse657(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy ECons(3, LList(lazy ECons(4, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse658(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy LCons(4, lazy LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse659(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy ECons(4, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse660(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, ECons(3, LList(lazy ECons(4, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse661(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse662(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ECons(4, ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse663(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ECons(4, LList(lazy ENil)))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse664(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, LList(lazy ECons(3, LList(lazy ECons(4, ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse665(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ECons(4, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse666(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse667(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, LList(lazy ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse668(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy ECons(3, ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse669(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse670(): Bool = 
	LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, LCons(4, lazy LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse671(): Bool = 
	LazyList1.intersperse(-11, ECons(1, ECons(2, LList(lazy ECons(3, ECons(4, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse672(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy LList(lazy ECons(3, ECons(4, LList(lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse673(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, ECons(3, LCons(4, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse674(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, LList(lazy LCons(3, lazy LList(lazy LCons(4, lazy LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse675(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, LCons(3, lazy LList(lazy ECons(4, LList(lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse676(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy LCons(3, lazy LCons(4, lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse677(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy LCons(4, lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse678(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy ECons(2, LList(lazy LCons(3, lazy ECons(4, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse679(): Bool = 
	LazyList1.intersperse(-11, ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy ECons(4, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse680(): Bool = 
	LazyList1.intersperse(-11, LCons(1, lazy LList(lazy LCons(2, lazy ECons(3, LCons(4, lazy LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse681(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ECons(2, ECons(3, LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

@test
def intersperse682(): Bool = 
	LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy LCons(4, lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))
}