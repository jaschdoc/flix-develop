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
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filter01(): Bool = region r {
        Nil |> List.toRegionIterator(r) |> RegionIterator.regFilter(i -> i > 3) == Nil
    }

    @test
    def filter02(): Bool = region r {
        (2 :: Nil) |> List.toRegionIterator(r) |> RegionIterator.regFilter(i -> i > 3) == Nil
    }

    @test
    def filter03(): Bool = region r {
        (4 :: Nil) |> List.toRegionIterator(r) |> RegionIterator.regFilter(i -> i > 3) == 4 :: Nil
    }

    @test
    def filter04(): Bool = region r {
        (1 :: 3 :: Nil) |> List.toRegionIterator(r) |> RegionIterator.regFilter(i -> i > 3) == Nil
    }

    @test
    def filter05(): Bool = region r {
        (1 :: 8 :: Nil) |> List.toRegionIterator(r) |> RegionIterator.regFilter(i -> i > 3) == 8 :: Nil
    }

    @test
    def filter06(): Bool = region r {
        (8 :: 1 :: Nil) |> List.toRegionIterator(r) |> RegionIterator.regFilter(i -> i > 3) == 8 :: Nil
    }

    @test
    def filter07(): Bool = region r {
        (8 :: 9 :: Nil) |> List.toRegionIterator(r) |> RegionIterator.regFilter(i -> i > 3) == 8 :: 9 :: Nil
    }

    @test
    def filter08(): Bool = region r {
        (1 :: 4 :: 11 :: 2 :: -22 :: 17 :: Nil) |> List.toRegionIterator(r) |> RegionIterator.regFilter(i -> i > 3) == 4 :: 11 :: 17 :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map01(): Bool = region r {
        Nil |> List.toRegionIterator(r) |>
            RegionIterator.regMap(i -> i > 2) == Nil
    }

    @test
    def map02(): Bool = region r {
        (1 :: Nil) |> List.toRegionIterator(r) |>
            RegionIterator.regMap(i -> i > 2) == false :: Nil
    }

    @test
    def map03(): Bool = region r {
        (3 :: Nil) |> List.toRegionIterator(r) |>
            RegionIterator.regMap(i -> i > 2) == true :: Nil
    }

    @test
    def map04(): Bool = region r {
        (1 :: 2 :: Nil) |> List.toRegionIterator(r) |>
            RegionIterator.regMap(i -> i > 2) == false :: false :: Nil
    }

    @test
    def map05(): Bool = region r {
        (1 :: 8 :: Nil) |> List.toRegionIterator(r) |>
            RegionIterator.regMap(i -> i > 2) == false :: true :: Nil
    }

    @test
    def map06(): Bool = region r {
        (8 :: 1 :: Nil) |> List.toRegionIterator(r) |>
            RegionIterator.regMap(i -> i > 2) == true :: false :: Nil
    }

    @test
    def map07(): Bool = region r {
        (7 :: 8 :: Nil) |> List.toRegionIterator(r) |>
            RegionIterator.regMap(i -> i > 2) == true :: true :: Nil
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
    // lazyFilter                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def lazyFilter01(): Bool = region r {
        List.toRegionIterator(r, Nil) |> RegionIterator.regFilterL(i -> i > 3) |> RegionIterator.regToList == Nil
    }

    @test
    def lazyFilter02(): Bool = region r {
        List.toRegionIterator(r, 2 :: Nil) |> RegionIterator.regFilterL(i -> i > 3) |> RegionIterator.regToList == Nil
    }

    @test
    def lazyFilter03(): Bool = region r {
        List.toRegionIterator(r, 4 :: Nil) |> RegionIterator.regFilterL(i -> i > 3) |> RegionIterator.regToList == 4 :: Nil
    }

    @test
    def lazyFilter04(): Bool = region r {
        List.toRegionIterator(r, 1 :: 3 :: Nil) |> RegionIterator.regFilterL(i -> i > 3) |> RegionIterator.regToList == Nil
    }

    @test
    def lazyFilter05(): Bool = region r {
        List.toRegionIterator(r, 1 :: 8 :: Nil) |> RegionIterator.regFilterL(i -> i > 3) |> RegionIterator.regToList == 8 :: Nil
    }

    @test
    def lazyFilter06(): Bool = region r {
        List.toRegionIterator(r, 8 :: 1 :: Nil) |> RegionIterator.regFilterL(i -> i > 3) |> RegionIterator.regToList == 8 :: Nil
    }

    @test
    def lazyFilter07(): Bool = region r {
        List.toRegionIterator(r, 8 :: 9 :: Nil) |> RegionIterator.regFilterL(i -> i > 3) |> RegionIterator.regToList == 8 :: 9 :: Nil
    }

    @test
    def lazyFilter08(): Bool = region r {
        List.toRegionIterator(r, 1 :: 4 :: 11 :: 2 :: -22 :: 17 :: Nil) |> RegionIterator.regFilterL(i -> i > 3) |> RegionIterator.regToList == 4 :: 11 :: 17 :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // lazyMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def lazyMap01(): Bool = region r {
        List.toRegionIterator(r, Nil) |> RegionIterator.regMapL(i -> i > 2) |> RegionIterator.regToList == Nil
    }

    @test
    def lazyMap02(): Bool = region r {
        List.toRegionIterator(r, 1 :: Nil) |> RegionIterator.regMapL(i -> i > 2) |> RegionIterator.regToList == false :: Nil
    }

    @test
    def lazyMap03(): Bool = region r {
        List.toRegionIterator(r, 3 :: Nil) |> RegionIterator.regMapL(i -> i > 2) |> RegionIterator.regToList == true :: Nil
    }

    @test
    def lazyMap04(): Bool = region r {
        List.toRegionIterator(r, 1 :: 2 :: Nil) |> RegionIterator.regMapL(i -> i > 2) |> RegionIterator.regToList == false :: false :: Nil
    }

    @test
    def lazyMap05(): Bool = region r {
        List.toRegionIterator(r, 1 :: 8 :: Nil) |> RegionIterator.regMapL(i -> i > 2) |> RegionIterator.regToList == false :: true :: Nil
    }

    @test
    def lazyMap06(): Bool = region r {
        List.toRegionIterator(r, 8 :: 1 :: Nil) |> RegionIterator.regMapL(i -> i > 2) |> RegionIterator.regToList == true :: false :: Nil
    }

    @test
    def lazyMap07(): Bool = region r {
        List.toRegionIterator(r, 7 :: 8 :: Nil) |> RegionIterator.regMapL(i -> i > 2) |> RegionIterator.regToList == true :: true :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def replace01(): Bool = region r {
        List.toRegionIterator(r, Nil) |>
            RegionIterator.regReplace(from = 3, to = 4) |> RegionIterator.regToList == Nil
    }

    @test
    def replace02(): Bool = region r {
        List.toRegionIterator(r, 1 :: Nil) |>
            RegionIterator.regReplace(from = 3, to = 4) |> RegionIterator.regToList == 1 :: Nil
    }

    @test
    def replace03(): Bool = region r {
        List.toRegionIterator(r, 3 :: Nil) |>
            RegionIterator.regReplace(from = 3, to = 4) |> RegionIterator.regToList == 4 :: Nil
    }

    @test
    def replace04(): Bool = region r {
        List.toRegionIterator(r, 4 :: Nil) |>
            RegionIterator.regReplace(from = 3, to = 4) |> RegionIterator.regToList == 4 :: Nil
    }

    @test
    def replace05(): Bool = region r {
        List.toRegionIterator(r, 1 :: 2 :: Nil) |>
            RegionIterator.regReplace(from = 3, to = 4) |> RegionIterator.regToList == 1 :: 2 :: Nil
    }

    @test
    def replace06(): Bool = region r {
        List.toRegionIterator(r, 1 :: 3 :: Nil) |>
            RegionIterator.regReplace(from = 3, to = 4) |> RegionIterator.regToList == 1 :: 4 :: Nil
    }

    @test
    def replace07(): Bool = region r {
        List.toRegionIterator(r, 3 :: 4 :: Nil) |>
            RegionIterator.regReplace(from = 3, to = 4) |> RegionIterator.regToList == 4 :: 4 :: Nil
    }

    @test
    def replace08(): Bool = region r {
        List.toRegionIterator(r, 3 :: 3 :: Nil) |>
            RegionIterator.regReplace(from = 3, to = 4) |> RegionIterator.regToList == 4 :: 4 :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def append01(): Bool = region r {
        let iter1 = List.toRegionIterator(r, Nil: List[Unit]);
        let iter2 = List.toRegionIterator(r, Nil: List[Unit]);
        RegionIterator.regAppend(iter1, iter2) |> RegionIterator.regToList == Nil
    }

    @test
    def append02(): Bool = region r {
        let iter1 = List.toRegionIterator(r, Nil);
        let iter2 = List.toRegionIterator(r, 1 :: Nil);
        RegionIterator.regAppend(iter1, iter2) |> RegionIterator.regToList == 1 :: Nil
    }

    @test
    def append03(): Bool = region r {
        let iter1 = List.toRegionIterator(r, Nil);
        let iter2 = List.toRegionIterator(r, 1 :: 2 :: Nil);
        RegionIterator.regAppend(iter1, iter2) |> RegionIterator.regToList == 1 :: 2 :: Nil
    }

    @test
    def append04(): Bool = region r {
        let iter1 = List.toRegionIterator(r, 1 :: Nil);
        let iter2 = List.toRegionIterator(r, Nil);
        RegionIterator.regAppend(iter1, iter2) |> RegionIterator.regToList == 1 :: Nil
    }

    @test
    def append05(): Bool = region r {
        let iter1 = List.toRegionIterator(r, 1 :: 2 :: Nil);
        let iter2 = List.toRegionIterator(r, Nil);
        RegionIterator.regAppend(iter1, iter2) |> RegionIterator.regToList == 1 :: 2 :: Nil
    }

    @test
    def append06(): Bool = region r {
        let iter1 = List.toRegionIterator(r, 1 :: Nil);
        let iter2 = List.toRegionIterator(r, 2 :: Nil);
        RegionIterator.regAppend(iter1, iter2) |> RegionIterator.regToList == 1 :: 2 :: Nil
    }

    @test
    def append07(): Bool = region r {
        let iter1 = List.toRegionIterator(r, 1 :: 2 :: Nil);
        let iter2 = List.toRegionIterator(r, 3 :: Nil);
        RegionIterator.regAppend(iter1, iter2) |> RegionIterator.regToList == 1 :: 2 :: 3 :: Nil
    }

    @test
    def append08(): Bool = region r {
        let iter1 = List.toRegionIterator(r, 1 :: Nil);
        let iter2 = List.toRegionIterator(r, 2 :: 3 :: Nil);
        RegionIterator.regAppend(iter1, iter2) |> RegionIterator.regToList == 1 :: 2 :: 3 :: Nil
    }

    @test
    def append09(): Bool = region r1 {
        let iter1 = List.toRegionIterator(r1, 1 :: Nil);
        region r2 {
            let iter2 = List.toRegionIterator(r2, 2 :: 3 :: Nil);
            RegionIterator.regAppend(iter1, iter2) |> RegionIterator.regToList == 1 :: 2 :: 3 :: Nil
        }
    }


    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zip01(): Bool = region r {
        let iterA = Nil: List[Int32] |> List.toRegionIterator(r);
        let iterB = RegionIterator.regRepeat(r, 1000, "b");
        RegionIterator.regZip(iterA, iterB) |> RegionIterator.regToList == Nil
    }

    @test
    def zip02(): Bool = region r {
        let iterA = Nil: List[Int32] |> List.toRegionIterator(r);
        let iterB = RegionIterator.regRepeat(r, 1000, "b");
        RegionIterator.regZip(iterB, iterA) |> RegionIterator.regToList == Nil
    }

    @test
    def zip03(): Bool = region r {
        let iterA = RegionIterator.regRepeat(r, 500,  "a");
        let iterB = RegionIterator.regRepeat(r, 1000, "b");
        RegionIterator.regZip(iterA, iterB) |> RegionIterator.regToList == List.repeat(500, ("a", "b"))
    }

    @test
    def zip04(): Bool = region r {
        let iterA = RegionIterator.regRepeat(r, 1000, "a");
        let iterB = RegionIterator.regRepeat(r, 500,  "b");
        RegionIterator.regZip(iterA, iterB) |> RegionIterator.regToList == List.repeat(500, ("a", "b"))
    }

    @test
    def zip05(): Bool = region r {
        let iterA = RegionIterator.regRepeat(r, 1000, "a");
        let iterB = RegionIterator.regRepeat(r, 1000, "b");
        RegionIterator.regZip(iterA, iterB) |> RegionIterator.regToList == List.repeat(1000, ("a", "b"))
    }

    @test
    def zip06(): Bool = region r {
        let iterA = RegionIterator.regRange(r, 0, 500);
        let iterB = RegionIterator.regRepeat(r, 1000, "b");
        RegionIterator.regZip(iterA, iterB) |> RegionIterator.regToList == List.zip(List.range(0, 500), List.repeat(500, "b"))
    }

    @test
    def zip07(): Bool = region r1 {
        let iterA = RegionIterator.regRange(r1, 0, 500);
        region r2 {
            let iterB = RegionIterator.regRepeat(r2, 1000, "b");
            RegionIterator.regZip(iterA, iterB) |> RegionIterator.regToList == List.zip(List.range(0, 500), List.repeat(500, "b"))
        }
    }


    /////////////////////////////////////////////////////////////////////////////
    // lazyZipWith                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def lazyZipWith01(): Bool = region r {
        let iterA = List.toRegionIterator(r, Nil);
        let iterB = List.toRegionIterator(r, Nil);
        RegionIterator.regZipWithL((a, b) -> if (b) a + 1 else a, iterA, iterB) |> RegionIterator.regToList == Nil
    }

    @test
    def lazyZipWith02(): Bool = region r {
        let iterA = List.toRegionIterator(r, 1 :: Nil);
        let iterB = List.toRegionIterator(r, Nil);
        RegionIterator.regZipWithL((a, b) -> if (b) a + 1 else a, iterA, iterB) |> RegionIterator.regToList == Nil
    }

    @test
    def lazyZipWith03(): Bool = region r {
        let iterA = List.toRegionIterator(r, Nil);
        let iterB = List.toRegionIterator(r, true :: Nil);
        RegionIterator.regZipWithL((a, b) -> if (b) a + 1 else a, iterA, iterB) |> RegionIterator.regToList == Nil
    }

    @test
    def lazyZipWith04(): Bool = region r {
        let iterA = List.toRegionIterator(r, 1 :: Nil);
        let iterB = List.toRegionIterator(r, true :: Nil);
        RegionIterator.regZipWithL((a, b) -> if (b) a + 1 else a, iterA, iterB) |> RegionIterator.regToList == 2 :: Nil
    }

    @test
    def lazyZipWith05(): Bool = region r {
        let iterA = List.toRegionIterator(r, 1 :: Nil);
        let iterB = List.toRegionIterator(r, false :: Nil);
        RegionIterator.regZipWithL((a, b) -> if (b) a + 1 else a, iterA, iterB) |> RegionIterator.regToList == 1 :: Nil
    }

    @test
    def lazyZipWith06(): Bool = region r {
        let iterA = List.toRegionIterator(r, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        let iterB = List.toRegionIterator(r, false :: true :: true :: false :: false :: true :: true :: true :: Nil);
        RegionIterator.regZipWithL((a, b) -> if (b) a + 1 else a, iterA, iterB) |>
            RegionIterator.regToList == 1 :: 3 :: 4 :: 4 :: 5 :: 7 :: 8 :: 9 :: Nil
    }

    @test
    def lazyZipWith07(): Bool = region r1 {
        let iterA = List.toRegionIterator(r1, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        region r2 {
            let iterB = List.toRegionIterator(r2, false :: true :: true :: false :: false :: true :: true :: true :: Nil);
            RegionIterator.regZipWithL((a, b) -> if (b) a + 1 else a, iterA, iterB) |>
                RegionIterator.regToList == 1 :: 3 :: 4 :: 4 :: 5 :: 7 :: 8 :: 9 :: Nil
        }
    }


    /////////////////////////////////////////////////////////////////////////////
    // zipWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWith01(): Bool = region r {
        let iterA = List.toRegionIterator(r, Nil);
        let iterB = List.toRegionIterator(r, Nil);
        RegionIterator.regZipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) == Nil
    }

    @test
    def zipWith02(): Bool = region r {
        let iterA = List.toRegionIterator(r, 1 :: Nil);
        let iterB = List.toRegionIterator(r, Nil);
        RegionIterator.regZipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) == Nil
    }

    @test
    def zipWith03(): Bool = region r {
        let iterA = List.toRegionIterator(r, Nil);
        let iterB = List.toRegionIterator(r, true :: Nil);
        RegionIterator.regZipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) == Nil
    }

    @test
    def zipWith04(): Bool = region r {
        let iterA = List.toRegionIterator(r, 1 :: Nil);
        let iterB = List.toRegionIterator(r, true :: Nil);
        RegionIterator.regZipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) == 2 :: Nil
    }

    @test
    def zipWith05(): Bool = region r {
        let iterA = List.toRegionIterator(r, 1 :: Nil);
        let iterB = List.toRegionIterator(r, false :: Nil);
        RegionIterator.regZipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) == 1 :: Nil
    }

    @test
    def zipWith06(): Bool = region r {
        let iterA = List.toRegionIterator(r, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        let iterB = List.toRegionIterator(r, false :: true :: true :: false :: false :: true :: true :: true :: Nil);
        RegionIterator.regZipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) == 1 :: 3 :: 4 :: 4 :: 5 :: 7 :: 8 :: 9 :: Nil
    }

    @test
    def zipWith07(): Bool = region r1 {
        let iterA = List.toRegionIterator(r1, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        region r2 {
            let iterB = List.toRegionIterator(r2, false :: true :: true :: false :: false :: true :: true :: true :: Nil);
            RegionIterator.regZipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) == 1 :: 3 :: 4 :: 4 :: 5 :: 7 :: 8 :: 9 :: Nil
        }
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


    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRightWithCont01(): Bool = region r {
        let iter = List.toRegionIterator(r, Nil);
        RegionIterator.regFoldRightWithCont((e, k) -> (k() - e) * (e rem 2 + 1), 100, iter) == 100
    }

    @test
    def foldRightWithCont02(): Bool = region r {
        let iter = List.toRegionIterator(r, 1 :: Nil);
        RegionIterator.regFoldRightWithCont((e, k) -> (k() - e) * (e rem 2 + 1), 100, iter) == 198
    }

    @test
    def foldRightWithCont03(): Bool = region r {
        let iter = List.toRegionIterator(r, 1 :: 2 :: Nil);
        RegionIterator.regFoldRightWithCont((e, k) -> (k() - e) * (e rem 2 + 1), 100, iter) == 194
    }

    @test
    def foldRightWithCont04(): Bool = region r {
        let iter = List.toRegionIterator(r, 1 :: 2 :: 3 :: Nil);
        RegionIterator.regFoldRightWithCont((e, k) -> (k() - e) * (e rem 2 + 1), 100, iter) == 382
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldMap01(): Bool = region r {
        RegionIterator.regFoldMap(x -> 2 * x, List.toRegionIterator(r, Nil)) == 0
    }

    @test
    def foldMap02(): Bool = region r {
        RegionIterator.regFoldMap(x -> 2 * x, List.toRegionIterator(r, 1 :: 2 :: Nil)) == 6
    }

    @test
    def foldMap03(): Bool = region r {
        RegionIterator.regFoldMap(x -> if (x == "a") "b" else x, List.toRegionIterator(r, "a" :: Nil)) == "b"
    }

    @test
    def foldMap04(): Bool = region r {
        RegionIterator.regFoldMap(x -> if (x == "c") "b" else x, List.toRegionIterator(r, "a" :: "b" :: "c" :: Nil)) == "abb"
    }

    @test
    def foldMap05(): Bool = region r {
        RegionIterator.regFoldMap(Int32.toString, List.toRegionIterator(r, 1 :: 2 :: 3 :: Nil)) == "123"
    }


    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceLeft01(): Bool = region r {
        let iter = List.toRegionIterator(r, Nil: List[Int32]);
        RegionIterator.regReduceLeft((a, b) -> a - b, iter) == None
    }

    @test
    def reduceLeft02(): Bool = region r {
        let iter = List.toRegionIterator(r, 1 :: Nil);
        RegionIterator.regReduceLeft((a, b) -> a - b, iter) == Some(1)
    }

    @test
    def reduceLeft03(): Bool = region r {
        let iter = List.toRegionIterator(r, 1 :: 2 :: Nil);
        RegionIterator.regReduceLeft((a, b) -> a - b, iter) == Some(-1)
    }

    @test
    def reduceLeft04(): Bool = region r {
        let iter = List.toRegionIterator(r, 1 :: 2 :: 3 :: Nil);
        RegionIterator.regReduceLeft((a, b) -> a - b, iter) == Some(-4)
    }

    @test
    def reduceLeft05(): Bool = region r {
        let iter = List.toRegionIterator(r, 1 :: 2 :: 3 :: 4 :: Nil);
        RegionIterator.regReduceLeft((a, b) -> a - b, iter) == Some(-8)
    }


    /////////////////////////////////////////////////////////////////////////////
    // takeWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhile01(): Bool = region r {
        List.toRegionIterator(r, Nil: List[Int32]) |>
            RegionIterator.regTakeWhile(i -> i > 3) == Nil
    }

    @test
    def takeWhile02(): Bool = region r {
        List.toRegionIterator(r, 1 :: Nil) |>
            RegionIterator.regTakeWhile(i -> i > 3) == Nil
    }

    @test
    def takeWhile03(): Bool = region r {
        List.toRegionIterator(r, 4 :: Nil) |>
            RegionIterator.regTakeWhile(i -> i > 3) == 4 :: Nil
    }

    @test
    def takeWhile04(): Bool = region r {
        List.toRegionIterator(r, 1 :: 2 :: Nil) |>
            RegionIterator.regTakeWhile(i -> i > 3) == Nil
    }

    @test
    def takeWhile05(): Bool = region r {
        List.toRegionIterator(r, 1 :: 5 :: Nil) |>
            RegionIterator.regTakeWhile(i -> i > 3) == Nil
    }

    @test
    def takeWhile06(): Bool = region r {
        List.toRegionIterator(r, 5 :: 1 :: Nil) |>
            RegionIterator.regTakeWhile(i -> i > 3) == 5 :: Nil
    }

    @test
    def takeWhile07(): Bool = region r {
        List.toRegionIterator(r, 5 :: 8 :: Nil) |>
            RegionIterator.regTakeWhile(i -> i > 3) == 5 :: 8 :: Nil
    }

    @test
    def takeWhile08(): Bool = region r {
        List.toRegionIterator(r, 4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil) |>
            RegionIterator.regTakeWhile(i -> i > 3) == 4 :: 6 :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // dropWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhile01(): Bool = region r {
        List.toRegionIterator(r, Nil) |>
            RegionIterator.regDropWhile(i -> i > 3) == Nil
    }

    @test
    def dropWhile02(): Bool = region r {
        List.toRegionIterator(r, 1 :: Nil) |>
            RegionIterator.regDropWhile(i -> i > 3) == 1 :: Nil
    }

    @test
    def dropWhile03(): Bool = region r {
        List.toRegionIterator(r, 4 :: Nil) |>
            RegionIterator.regDropWhile(i -> i > 3) == Nil
    }

    @test
    def dropWhile04(): Bool = region r {
        List.toRegionIterator(r, 1 :: 2 :: Nil) |>
            RegionIterator.regDropWhile(i -> i > 3) == 1 :: 2 :: Nil
    }

    @test
    def dropWhile05(): Bool = region r {
        List.toRegionIterator(r, 1 :: 5 :: Nil) |>
            RegionIterator.regDropWhile(i -> i > 3) == 1 :: 5 :: Nil
    }

    @test
    def dropWhile06(): Bool = region r {
        List.toRegionIterator(r, 5 :: 1 :: Nil) |>
            RegionIterator.regDropWhile(i -> i > 3) == 1 :: Nil
    }

    @test
    def dropWhile07(): Bool = region r {
        List.toRegionIterator(r, 5 :: 8 :: Nil) |>
            RegionIterator.regDropWhile(i -> i > 3) == Nil
    }

    @test
    def dropWhile08(): Bool = region r {
        List.toRegionIterator(r, 4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil) |>
            RegionIterator.regDropWhile(i -> i > 3) == -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil
    }



}
