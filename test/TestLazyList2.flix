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

}