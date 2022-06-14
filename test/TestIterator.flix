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
    // product                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def product01(): Bool = region r {
        new RegionIterator(r) |> RegionIterator.product == 0
    }

    @test
    def product02(): Bool = region r {
        List.toRegionIterator(r, 1 :: Nil) |> RegionIterator.product == 1
    }

    @test
    def product03(): Bool = region r {
        List.toRegionIterator(r, 1 :: 2 :: 3 :: Nil) |> RegionIterator.product == 6
    }

    @test
    def product04(): Bool = region r {
        List.toRegionIterator(r, 1 :: 2 :: 3 :: -3 :: Nil) |> RegionIterator.product == -18
    }

    @test
    def product05(): Bool = region r {
        List.toRegionIterator(r, -1 :: -2 :: -3 :: -4 :: Nil) |> RegionIterator.product == 24
    }

    @test
    def product06(): Bool = region r {
        List.toRegionIterator(r, 10 :: -10 :: Nil) |> RegionIterator.product == -100
    }


    /////////////////////////////////////////////////////////////////////////////
    // productWith                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productWith01(): Bool = region r {
        new RegionIterator(r) |> RegionIterator.productWith(x -> x + 1) == 0
    }

    @test
    def productWith02(): Bool = region r {
        List.toRegionIterator(r, 1 :: Nil) |> RegionIterator.productWith(x -> x + 1) == 2
    }

    @test
    def productWith03(): Bool = region r {
        List.toRegionIterator(r, 1 :: 2 :: 3 :: Nil) |> RegionIterator.productWith(x -> x + 1) == 24
    }

    @test
    def productWith04(): Bool = region r {
        List.toRegionIterator(r, 1 :: 2 :: 3 :: -3 :: Nil) |> RegionIterator.productWith(x -> x + 1) == -48
    }

    @test
    def productWith05(): Bool = region r {
        List.toRegionIterator(r, -2 :: -3 :: -4 :: -5 :: Nil) |> RegionIterator.productWith(x -> x + 1) == 24
    }

    @test
    def productWith06(): Bool = region r {
        List.toRegionIterator(r, 10 :: -10 :: Nil) |> RegionIterator.productWith(x -> x + 1) == -99
    }


    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toArray01(): Bool = region r {
        Nil: List[Int32] |> List.toRegionIterator(r) |> RegionIterator.regToArray(r) `Array.sameElements` [] @ r
    }

    @test
    def toArray02(): Bool = region r {
        RegionIterator.regRange(r, -10, -5) |> RegionIterator.regToArray(r) `Array.sameElements` [-10, -9, -8, -7, -6] @ r
    }

    @test
    def toArray03(): Bool = region r {
        RegionIterator.regRepeat(r, 3, 1) |> RegionIterator.regToArray(r) `Array.sameElements` [1, 1, 1] @ r
    }

    @test
    def toArray04(): Bool = region r {
        RegionIterator.regRange(r, -100, 200) |> RegionIterator.regToArray(r) `Array.sameElements` Array.range(-100, 200, r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMap01(): Bool = region r {
        Map.empty() |> Map.toRegionIterator(r) |> RegionIterator.regToMap == Map.empty(): Map[Int32, Int32]
    }

    @test
    def toMap02(): Bool = region r {
        Map#{"A" => 1, "B" => 2, "C" => 3, "D" => 4} |>
            Map.toRegionIterator(r) |>
            RegionIterator.regToMap == Map#{"A" => 1, "B" => 2, "C" => 3, "D" => 4}
    }

    @test
    def toMap03(): Bool = region r {
        Map#{1 => 2, 2 => 3, 3 => 4} |> Map.toRegionIterator(r) |> RegionIterator.regToMap == Map#{1 => 2, 2 => 3, 3 => 4}
    }


    /////////////////////////////////////////////////////////////////////////////
    // toNel                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toNel01(): Bool = region r {
        Nil |> List.toRegionIterator(r) |> RegionIterator.regToNel == None: Option[Nel[Int32]]
    }

    @test
    def toNel02(): Bool = region r {
        (1 :: Nil) |> List.toRegionIterator(r) |> RegionIterator.regToNel == Some(Nel(1, Nil))
    }

    @test
    def toNel03(): Bool = region r {
        RegionIterator.regRange(r, 0, 1000) |> RegionIterator.regToNel == Some(Nel(0, List.range(1, 1000)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // toNec                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toNec01(): Bool = region r {
        let iter = List.toRegionIterator(r, Nil: List[Unit]);
        RegionIterator.regToNec(iter) == None
    }

    @test
    def toNec02(): Bool = region r {
        let iter = List.toRegionIterator(r, 1 :: Nil);
        RegionIterator.regToNec(iter) == Some(Nec.singleton(1))
    }

    @test
    def toNec03(): Bool = region r {
        let iter = List.toRegionIterator(r, 1 :: 2 :: Nil);
        RegionIterator.regToNec(iter) == Some(Nec.cons(1, Nec.singleton(2)))
    }

    @test
    def toNec04(): Bool = region r {
        let iter = List.toRegionIterator(r, 1 :: 1 :: Nil);
        RegionIterator.regToNec(iter) == Some(Nec.cons(1, Nec.singleton(1)))
    }

    @test
    def toNec05(): Bool = region r {
        let iter = List.toRegionIterator(r, 2 :: 1 :: Nil);
        RegionIterator.regToNec(iter) == Some(Nec.cons(2, Nec.singleton(1)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toSet01(): Bool = region r {
        Set.empty() |> Set.toRegionIterator(r) |> RegionIterator.regToSet == Set.empty(): Set[Int32]
    }

    @test
    def toSet02(): Bool = region r {
        Set#{"a", "b", "c", "d"} |> Set.toRegionIterator(r) |> RegionIterator.regToSet == Set#{"a", "b", "c", "d"}
    }

    @test
    def toSet03(): Bool = region r {
        Set.range(0, 10000) |> Set.toRegionIterator(r) |> RegionIterator.regToSet == Set.range(0, 10000)
    }


    /////////////////////////////////////////////////////////////////////////////
    // foreach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foreach01(): Bool = region r {
        let res = ref 21 @ r;
        Nil |> List.toRegionIterator(r) |>
            RegionIterator.regForeach(x -> res := x);
        21 == deref res
    }

    @test
    def foreach02(): Bool = region r {
        let res = ref 21 @ r;
        (1 :: 2 :: 3 :: Nil) |> List.toRegionIterator(r) |>
            RegionIterator.regForeach(x -> res := x);
        3 == deref res
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
