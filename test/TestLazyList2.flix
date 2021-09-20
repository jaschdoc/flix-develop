///
/// Additional Tests for LazyList.
///
namespace TestLazyList2 {
    
    /////////////////////////////////////////////////////////////////////////////
    // filterMap (pure)                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterMapPure01(): Bool = 
        LazyList1.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, ENil) == ENil

    @test
    def filterMapPure02(): Bool = 
        LazyList1.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, LList(lazy ENil)) == ENil

    @test
    def filterMapPure03(): Bool = 
        LazyList1.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, LCons(1, lazy ENil)) == ENil

    @test
    def filterMapPure04(): Bool = 
        LazyList1.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, ECons(1, LList(lazy ENil))) == ENil

    @test
    def filterMapPure05(): Bool = 
        LazyList1.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, LCons(1, lazy LList(lazy ENil))) == ENil

    @test
    def filterMapPure06(): Bool = 
        LazyList1.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, ECons(1, ENil)) == ENil

    @test
    def filterMapPure07(): Bool = 
        LazyList1.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, LList(lazy ECons(2, LList(lazy ENil)))) == ECons(1, ENil)

    @test
    def filterMapPure08(): Bool = 
        LazyList1.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, LCons(2, lazy ENil)) == ECons(1, ENil)

    @test
    def filterMapPure09(): Bool = 
        LazyList1.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, ECons(2, LList(lazy ENil))) == ECons(1, ENil)

    @test
    def filterMapPure10(): Bool = 
        LazyList1.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, LList(lazy ECons(2, ENil))) == ECons(1, ENil)

	@test
	def filterMapPure11(): Bool = 
		LazyList1.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, LList(lazy ECons(1, ECons(3, ENil)))) == ENil

	@test
	def filterMapPure12(): Bool = 
		LazyList1.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, LList(lazy LCons(1, lazy LCons(3, lazy LList(lazy ENil))))) == ENil

	@test
	def filterMapPure13(): Bool = 
		LazyList1.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, LList(lazy ECons(1, LCons(3, lazy LList(lazy ENil))))) == ENil

	@test
	def filterMapPure14(): Bool = 
		LazyList1.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, ECons(1, ECons(3, LList(lazy ENil)))) == ENil

	@test
	def filterMapPure15(): Bool = 
		LazyList1.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, ECons(1, ECons(4, ENil))) == ECons(2, ENil)

	@test
	def filterMapPure16(): Bool = 
		LazyList1.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, ECons(6, ECons(-1, ENil))) == ECons(3, ENil)

	@test
	def filterMapPure17(): Bool = 
		LazyList1.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, ECons(8, ECons(6, ENil))) == ECons(4, ECons(3, ENil))
    
    @test
	def filterMapPure18(): Bool = 
		LazyList1.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, ECons(0, ECons(1, ECons(2, ECons(3, ECons(4, ECons(5, ECons(10, ENil)))))))) == ECons(0, ECons(1, ECons(2, ECons(5, ENil))))

}