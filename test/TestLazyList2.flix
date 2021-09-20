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

}