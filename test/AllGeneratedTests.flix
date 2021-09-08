namespace AllGeneratedTests {

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


}
