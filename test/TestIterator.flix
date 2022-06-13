namespace TestRegionIterator {

    /////////////////////////////////////////////////////////////////////////////
    // next                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def next01(): Bool = region r {
        new RegionIterator(r) |> RegionIterator.regNext == None: Option[Int32]
    }

    @test
    def next02(): Bool = region r {
        let iter = List.toRegionIterator(r, 1 :: Nil);
        RegionIterator.regNext(iter) == Some(1)
    }

    @test
    def next03(): Bool = region r {
        let iter = List.toRegionIterator(r, 1 :: Nil);
        RegionIterator.regNext(iter);
        RegionIterator.regNext(iter) == None
    }

    @test
    def next04(): Bool = region r {
        let iter = List.toRegionIterator(r, 1 :: 2 :: Nil);
        RegionIterator.regNext(iter);
        RegionIterator.regNext(iter) == Some(2)
    }


    /////////////////////////////////////////////////////////////////////////////
    // isEmpty (idempotency)                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isEmpty01(): Bool = region r { // empty
        let iter = new RegionIterator(r);
        invokeIsEmptyRegion(100, iter);
        RegionIterator.regToList(iter) == Nil: List[Int32]
    }

    @test
    def isEmpty02(): Bool = region r { // singleton
        let iter = RegionIterator.regSingleton(r, 1);
        invokeIsEmptyRegion(100, iter);
        RegionIterator.regToList(iter) == 1 :: Nil
    }

    ///
    /// Helper function that invokes `RegionIterator.isEmpty` `n` times.
    ///
    def invokeIsEmptyRegion(n: Int32, iter: RegionIterator[a, r]): Unit \ Read(r) =
        def loop(m) =
            if (m <= 0)
                ()
            else {
                RegionIterator.regIsEmpty(iter);
                loop(m - 1)
            };
        loop(n)


    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def range01(): Bool = region r {
        RegionIterator.regRange(r, 0, 0) |> RegionIterator.regToList == Nil
    }

    @test
    def range02(): Bool = region r {
        RegionIterator.regRange(r, 10, 0) |> RegionIterator.regToList == Nil
    }

    @test
    def range03(): Bool = region r {
        RegionIterator.regRange(r, 1, 4) |> RegionIterator.regToList == 1 :: 2 :: 3 :: Nil
    }

    @test
    def range04(): Bool = region r {
        RegionIterator.regRange(r, -1, 2) |> RegionIterator.regToList == -1 :: 0 :: 1 :: Nil
    }

    @test
    def range05(): Bool = region r {
        RegionIterator.regRange(r, -100, 200) |> RegionIterator.regToList == List.range(-100, 200)
    }


    /////////////////////////////////////////////////////////////////////////////
    // repeat                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def repeat01(): Bool = region r {
        RegionIterator.regRepeat(r, 0, 10) |> RegionIterator.regToList == Nil
    }

    @test
    def repeat02(): Bool = region r {
        RegionIterator.regRepeat(r, -100, 2) |> RegionIterator.regToList == Nil
    }

    @test
    def repeat03(): Bool = region r {
        RegionIterator.regRepeat(r, 100, 2) |> RegionIterator.regToList == List.repeat(100, 2)
    }

    @test
    def repeat04(): Bool = region r {
        RegionIterator.regRepeat(r, 3, 1) |> RegionIterator.regToList == 1 :: 1 :: 1 :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool = region r {
        let iter = List.toRegionIterator(r, Nil);
        RegionIterator.regFoldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, iter) == 100
    }

    @test
    def foldLeft02(): Bool = region r {
        let iter = List.toRegionIterator(r, 1 :: Nil);
        RegionIterator.regFoldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, iter) == 198
    }

    @test
    def foldLeft03(): Bool = region r {
        let iter = List.toRegionIterator(r, 1 :: 2 :: Nil);
        RegionIterator.regFoldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, iter) == 196
    }

    @test
    def foldLeft04(): Bool = region r {
        let iter = List.toRegionIterator(r, 1 :: 2 :: 3 :: Nil);
        RegionIterator.regFoldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, iter) == 386
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool = region r {
        let iter = List.toRegionIterator(r, Nil);
        RegionIterator.regFoldRight((e, acc) -> (acc - e) * (e rem 2 + 1), 100, iter) == 100
    }

    @test
    def foldRight02(): Bool = region r {
        let iter = List.toRegionIterator(r, 1 :: Nil);
        RegionIterator.regFoldRight((e, acc) -> (acc - e) * (e rem 2 + 1), 100, iter) == 198
    }

    @test
    def foldRight03(): Bool = region r {
        let iter = List.toRegionIterator(r, 1 :: 2 :: Nil);
        RegionIterator.regFoldRight((e, acc) -> (acc - e) * (e rem 2 + 1), 100, iter) == 194
    }

    @test
    def foldRight04(): Bool = region r {
        let iter = List.toRegionIterator(r, 1 :: 2 :: 3 :: Nil);
        RegionIterator.regFoldRight((e, acc) -> (acc - e) * (e rem 2 + 1), 100, iter) == 382
    }

}
