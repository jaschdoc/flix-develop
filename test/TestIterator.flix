namespace TestRegionIterator {

    /////////////////////////////////////////////////////////////////////////////
    // next                                                                    //
    /////////////////////////////////////////////////////////////////////////////
/*
    @test
    def next01(): Bool = region r {
        RegionIterator.new(r) |> RegionIterator.next == None: Option[Int32]
    }
*/
    @test
    def next02(): Bool = region r {
        let iter = List.toRegionIterator(r, 1 :: Nil);
        RegionIterator.next(iter) == Some(1)
    }

    @test
    def next03(): Bool = region r {
        let iter = List.toRegionIterator(r, 1 :: Nil);
        RegionIterator.next(iter);
        RegionIterator.next(iter) == None
    }

    @test
    def next04(): Bool = region r {
        let iter = List.toRegionIterator(r, 1 :: 2 :: Nil);
        RegionIterator.next(iter);
        RegionIterator.next(iter) == Some(2)
    }


}
