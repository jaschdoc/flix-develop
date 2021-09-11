namespace TestLazyList {

    /////////////////////////////////////////////////////////////////////////////
    // Eq                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def eq01(): Bool =
        ENil: LazyList[Unit] == ENil: LazyList[Unit]

    @test
    def eq02(): Bool =
        ECons(1, ENil) == ECons(1, ENil)

    @test
    def eq03(): Bool =
        LCons(1, lazy ENil) == LCons(1, lazy ENil)

    @test
    def eq04(): Bool =
        LList(lazy ECons(1, ENil)) == LList(lazy ECons(1, ENil))

    @test
    def eq05(): Bool =
        ECons(1, ENil) == LCons(1, lazy ENil)

    @test
    def eq06(): Bool =
        LCons(1, lazy ENil) == ECons(1, ENil)

    @test
    def eq07(): Bool =
        ECons(1, ENil) == LList(lazy ECons(1, ENil))

    @test
    def eq08(): Bool =
        LList(lazy ECons(1, ENil)) == ECons(1, ENil)

    @test
    def eq09(): Bool =
        ECons(1, ENil) == LList(lazy LCons(1, lazy ENil))

    @test
    def eq10(): Bool =
        LList(lazy LCons(1, lazy ENil)) == ECons(1, ENil)

    @test
    def eq11(): Bool =
        ECons(1, ECons(2, ECons(3, ENil))) == ECons(1, ECons(2, ECons(3, ENil)))

    @test
    def eq12(): Bool =
        ECons(1, ECons(2, ECons(3, ENil))) == LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil)))

    @test
    def eq13(): Bool =
        LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil))) == ECons(1, ECons(2, ECons(3, ENil)))

    @test
    def eq14(): Bool =
        ECons(1, LCons(2, lazy ECons(3, ENil))) == LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil)))

    @test
    def eq15(): Bool =
        LCons(1, lazy ECons(2, LCons(3, lazy ENil))) == ECons(1, ECons(2, ECons(3, ENil)))

    @test
    def eq16(): Bool =
        ECons(1, ECons(2, ECons(3, ENil))) == LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil)))

    @test
    def eq17(): Bool =
        LList(lazy ECons(1, ECons(2, ECons(3, ENil)))) == LList(lazy LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil))))

    @test
    def eq18(): Bool =
        LList(lazy ECons(1, ECons(2, ECons(3, ENil)))) == LList(lazy LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil))))

    @test
    def eq19(): Bool =
        ECons(0, ENil) != ECons(1, ENil)

    @test
    def eq20(): Bool =
        ECons(0, ENil) != LCons(1, lazy ENil)

    @test
    def eq21(): Bool =
        LCons(0, lazy ENil) != ECons(1, ENil)

    @test
    def eq22(): Bool =
        LCons(0, lazy ENil) != LCons(1, lazy ENil)

    @test
    def eq23(): Bool =
        LList(lazy ECons(0, ENil)) != LCons(1, lazy ENil)

    @test
    def eq24(): Bool =
        LCons(0, lazy ENil) != LList(lazy ECons(1, ENil))

    @test
    def eq25(): Bool =
        ECons(0, ENil) != ECons(0, ECons(0, ENil))

    @test
    def eq26(): Bool =
        ECons(0, ENil) != ECons(0, ECons(1, ENil))

    @test
    def eq27(): Bool =
        ECons(0, ENil) != ENil

    @test
    def eq28(): Bool =
        ENil != ECons(0, ENil)

    @test
    def eq29(): Bool =
        LCons(0, lazy ENil) != ENil

    @test
    def eq30(): Bool =
        ENil != LCons(0, lazy ENil)


    /////////////////////////////////////////////////////////////////////////////
    // empty                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def empty01(): Bool =
        (LazyList1.empty()): LazyList[Int32] |> LazyList1.toList == Nil


    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isEmpty01(): Bool =
        (LazyList1.empty()): LazyList[Int32] |> LazyList1.isEmpty

    @test
    def isEmpty02(): Bool =
        not (LazyList1.range(0, 1000) |> LazyList1.isEmpty)


    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def range01(): Bool =
        LazyList1.range(0, 0) |> LazyList1.isEmpty

    @test
    def range02(): Bool =
        LazyList1.range(100, 100) |> LazyList1.isEmpty

    @test
    def range03(): Bool =
        LazyList1.range(1, 0) |> LazyList1.isEmpty

    @test
    def range04(): Bool =
        not (LazyList1.range(0, 100) |> LazyList1.isEmpty)

    @test
    def range05(): Bool =
        LazyList1.range(0, 100) |> LazyList1.toList == List.range(0, 100)

    @test
    def range06(): Bool =
        not (LazyList1.range(-100, 100) |> LazyList1.isEmpty)

    @test
    def range07(): Bool =
        LazyList1.range(-100, 100) |> LazyList1.toList == List.range(-100, 100)


    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def head01(): Bool =
        (LazyList1.empty()): LazyList[Int32] |> LazyList1.head == None

    @test
    def head02(): Bool =
        LazyList1.range(0, 1000) |> LazyList1.head == Some(0)

    @test
    def head03(): Bool =
        let l = LazyList1.range(0, 1000);
        LazyList1.head(l) == Some(0)
            and LazyList1.head(l) == Some(0)


    /////////////////////////////////////////////////////////////////////////////
    // tail                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tail01(): Bool =
        LazyList1.tail(ENil: LazyList[Unit]) == ENil

    @test
    def tail02(): Bool =
        LazyList1.tail(LList(lazy ENil): LazyList[Unit]) == ENil

    @test
    def tail03(): Bool =
        LazyList1.range(0, 1) |> LazyList1.tail == ENil

    @test
    def tail04(): Bool =
    	LCons(1, lazy ENil) |> LazyList1.tail == ENil

    @test
    def tail05(): Bool =
    	LList(lazy ECons(1, LList(lazy ENil))) |> LazyList1.tail == ENil

    @test
    def tail06(): Bool =
    	ECons(1, LList(lazy ENil)) |> LazyList1.tail == ENil

    @test
    def tail07(): Bool =
    	LCons(1, lazy LList(lazy ENil)) |> LazyList1.tail == ENil

    @test
    def tail08(): Bool =
    	LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil)))) |> LazyList1.tail == LazyList1.range(2, 4)

    @test
    def tail09(): Bool =
    	LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil)))) |> LazyList1.tail == LazyList1.range(2, 4)

    @test
    def tail10(): Bool =
    	LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil))))) |> LazyList1.tail == LazyList1.range(2, 4)

    @test
    def tail11(): Bool =
    	LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil)))))) |> LazyList1.tail == LazyList1.range(2, 4)

    @test
    def tail12(): Bool =
    	LazyList1.range(0, 1000) |> LazyList1.tail == LazyList1.range(1, 1000)


    /////////////////////////////////////////////////////////////////////////////
    // take                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def take01(): Bool =
        (LazyList1.empty()): LazyList[Int32] |> LazyList1.take(1000) |> LazyList1.isEmpty

    @test
    def take02(): Bool =
        (LazyList1.empty()): LazyList[Int32] |> LazyList1.take(0) |> LazyList1.isEmpty

    @test
    def take03(): Bool =
        (LazyList1.range(0, 1000)): LazyList[Int32] |> LazyList1.take(0) |> LazyList1.isEmpty

    @test
    def take04(): Bool =
        (LazyList1.range(0, 1000)): LazyList[Int32]
            |> LazyList1.take(500)
            |> LazyList1.toList == List.range(0, 500)

    @test
    def take05(): Bool =
        (LazyList1.range(0, 1000)): LazyList[Int32]
            |> LazyList1.take(1)
            |> LazyList1.toList == List.range(0, 1)


    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def length01(): Bool =
        (LazyList1.empty()): LazyList[Int32] |> LazyList1.length == 0

    @test
    def length02(): Bool =
        LazyList1.range(0, 100000) |> LazyList1.length == 100000


    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reverse01(): Bool =
        LazyList1.reverse(LazyList1.empty(): LazyList[Unit]) |> LazyList1.toList == Nil

    @test
    def reverse02(): Bool =
        (1 :: Nil) |> List.toLazy |> LazyList1.reverse |> LazyList1.toList == 1 :: Nil

    @test
    def reverse03(): Bool =
        (1 :: 2 :: Nil) |> List.toLazy |> LazyList1.reverse |> LazyList1.toList == 2 :: 1 :: Nil

    @test
    def reverse04(): Bool =
        (1 :: 1 :: Nil) |> List.toLazy |> LazyList1.reverse |> LazyList1.toList == 1 :: 1 :: Nil

    @test
    def reverse05(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList1.reverse |> LazyList1.toList == 3 :: 2 :: 1 :: Nil

    @test
    def reverse06(): Bool =
        (1 :: 2 :: 3 :: 4 :: Nil) |> List.toLazy |> LazyList1.reverse |> LazyList1.toList == 4 :: 3 :: 2 :: 1 :: Nil

    @test
    def reverse07(): Bool =
        LazyList1.range(-100, 1000) |> LazyList1.reverse |> LazyList1.toList == List.range(-100, 1000) |> List.reverse


    /////////////////////////////////////////////////////////////////////////////
    // map (pure)                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapPure01(): Bool =
        Nil |> List.toLazy |> LazyList1.map(x -> x + 1) |> LazyList1.toList == Nil

    @test
    def mapPure02(): Bool =
        (1 :: Nil) |> List.toLazy |> LazyList1.map(x -> x + 1) |> LazyList1.toList == 2 :: Nil

    @test
    def mapPure03(): Bool =
        (1 :: 2 :: Nil) |> List.toLazy |> LazyList1.map(x -> x + 1) |> LazyList1.toList == 2 :: 3 :: Nil

    @test
    def mapPure04(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList1.map(x -> x + 1) |> LazyList1.toList == 2 :: 3 :: 4 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // map (impure)                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapImpure01(): Bool & Impure =
        Nil |> List.toLazy |> LazyList1.map(x -> x + 1 as & Impure) |> LazyList1.toList == Nil

    @test
    def mapImpure02(): Bool & Impure =
        (1 :: Nil) |> List.toLazy |> LazyList1.map(x -> x + 1 as & Impure) |> LazyList1.toList == 2 :: Nil

    @test
    def mapImpure03(): Bool & Impure =
        (1 :: 2 :: Nil) |> List.toLazy |> LazyList1.map(x -> x + 1 as & Impure) |> LazyList1.toList == 2 :: 3 :: Nil

    @test
    def mapImpure04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList1.map(x -> x + 1 as & Impure) |> LazyList1.toList == 2 :: 3 :: 4 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // map map                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapMap01(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList1.map(x -> x + 1) |> LazyList1.map(x -> x * 2) |> LazyList1.toList == 4 :: 6 :: 8 :: Nil

    @test
    def mapMap02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList1.map(x -> x + 1 as & Impure) |> LazyList1.map(x -> x * 2) |> LazyList1.toList == 4 :: 6 :: 8 :: Nil

    @test
    def mapMap03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList1.map(x -> x + 1) |> LazyList1.map(x -> x * 2 as & Impure) |> LazyList1.toList == 4 :: 6 :: 8 :: Nil

    @test
    def mapMap04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList1.map(x -> x + 1 as & Impure) |> LazyList1.map(x -> x * 2 as & Impure) |> LazyList1.toList == 4 :: 6 :: 8 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // map map fusion                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |>
        LazyList1.map(x -> { l := "a" :: deref l; x }) |>
        LazyList1.map(x -> { l := "b" :: deref l; x });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def mapFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |>
        LazyList1.map(x -> { l := "a" :: deref l; x } as & Pure) |>
        LazyList1.map(x -> { l := "b" :: deref l; x } as & Pure) |>
        LazyList1.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // filter (pure)                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterPure01(): Bool =
        Nil |> List.toLazy |> LazyList1.filter(x -> x > 100) |> LazyList1.toList == Nil

    @test
    def filterPure02(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList1.filter(x -> x > 0) |> LazyList1.toList == 1 :: 2 :: 3 :: Nil

    @test
    def filterPure03(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList1.filter(x -> x > 1) |> LazyList1.toList == 2 :: 3 :: Nil

    @test
    def filterPure04(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList1.filter(x -> x > 2) |> LazyList1.toList == 3 :: Nil

    @test
    def filterPure05(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList1.filter(x -> x > 3) |> LazyList1.toList == Nil

    @test
    def filterPure06(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList1.filter(x -> x > 100) |> LazyList1.toList == Nil


    /////////////////////////////////////////////////////////////////////////////
    // filter (impure)                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterImpure01(): Bool & Impure =
        Nil |> List.toLazy |> LazyList1.filter(x -> x > 100 as & Impure) |> LazyList1.toList == Nil

    @test
    def filterImpure02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList1.filter(x -> x > 0 as & Impure) |> LazyList1.toList == 1 :: 2 :: 3 :: Nil

    @test
    def filterImpure03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList1.filter(x -> x > 1 as & Impure) |> LazyList1.toList == 2 :: 3 :: Nil

    @test
    def filterImpure04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList1.filter(x -> x > 2 as & Impure) |> LazyList1.toList == 3 :: Nil

    @test
    def filterImpure05(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList1.filter(x -> x > 3 as & Impure) |> LazyList1.toList == Nil

    @test
    def filterImpure06(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList1.filter(x -> x > 100 as & Impure) |> LazyList1.toList == Nil


    /////////////////////////////////////////////////////////////////////////////
    // filter filter                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterFilter01(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList1.filter(x -> x > 1) |> LazyList1.filter(x -> x < 3) |> LazyList1.toList == 2 :: Nil

    @test
    def filterFilter02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList1.filter(x -> x > 1 as & Impure) |> LazyList1.filter(x -> x < 3) |> LazyList1.toList == 2 :: Nil

    @test
    def filterFilter03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList1.filter(x -> x > 1) |> LazyList1.filter(x -> x < 3 as & Impure) |> LazyList1.toList == 2 :: Nil

    @test
    def filterFilter04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList1.filter(x -> x > 1 as & Impure) |> LazyList1.filter(x -> x < 3 as & Impure) |> LazyList1.toList == 2 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // filter filter fusion                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |>
        LazyList1.filter(_ -> { l := "a" :: deref l; true }) |>
        LazyList1.filter(_ -> { l := "b" :: deref l; true });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def filterFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |>
        LazyList1.filter(_x -> { l := "a" :: deref l; true } as & Pure) |>
        LazyList1.filter(_x -> { l := "b" :: deref l; true } as & Pure) |>
        LazyList1.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // map filter fusion                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapFilterFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |>
        LazyList1.map(   x -> { l := "a" :: deref l; x }) |>
        LazyList1.filter(_ -> { l := "b" :: deref l; true });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def mapFilterFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |>
        LazyList1.map(   x -> { l := "a" :: deref l; x }    as & Pure) |>
        LazyList1.filter(_ -> { l := "b" :: deref l; true } as & Pure) |>
        LazyList1.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // filter map fusion                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterMapFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |>
        LazyList1.filter(_ -> { l := "a" :: deref l; true }) |>
        LazyList1.map(   x -> { l := "b" :: deref l; x    });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def filterMapFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |>
        LazyList1.filter(_ -> { l := "a" :: deref l; true } as & Pure) |>
        LazyList1.map(   x -> { l := "b" :: deref l; x    } as & Pure) |>
        LazyList1.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)
    
    /////////////////////////////////////////////////////////////////////////////    
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool =
    	LazyList1.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, ENil) == 100

    @test
    def foldLeft02(): Bool =
    	LazyList1.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, LList(lazy ENil)) == 100

    @test
    def foldLeft03(): Bool =
    	LazyList1.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, LCons(1, lazy ENil)) == 198

    @test
    def foldLeft04(): Bool =
    	LazyList1.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, LList(lazy ECons(1, LList(lazy ENil)))) == 198

    @test
    def foldLeft05(): Bool =
    	LazyList1.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, ECons(1, LList(lazy ENil))) == 198

    @test
    def foldLeft06(): Bool =
    	LazyList1.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, LCons(1, lazy LList(lazy ENil))) == 198

    @test
    def foldLeft07(): Bool =
    	LazyList1.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil))))) == 196

    @test
    def foldLeft08(): Bool =
    	LazyList1.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, LList(lazy LCons(1, lazy ECons(2, LList(lazy ENil))))) == 196

    @test
    def foldLeft09(): Bool =
    	LazyList1.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, LList(lazy LCons(1, lazy LCons(2, lazy ENil)))) == 196

    @test
    def foldLeft10(): Bool =
    	LazyList1.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, ECons(1, LCons(2, lazy ENil))) == 196

    @test
    def foldLeft11(): Bool =
    	LazyList1.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))) == 386

    @test
    def foldLeft12(): Bool =
    	LazyList1.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))) == 386

    @test
    def foldLeft13(): Bool =
    	LazyList1.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil)))))) == 386

    @test
    def foldLeft14(): Bool =
    	LazyList1.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) == 386

    @test
    def foldLeft15(): Bool =
    	LazyList1.foldLeft((acc, x) -> x :: acc, Nil, LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))) == 3 :: 2 :: 1 :: Nil

    @test
    def foldLeft16(): Bool =
    	LazyList1.foldLeft((acc, x) -> x :: acc, Nil, LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))) == 3 :: 2 :: 1 :: Nil

    @test
    def foldLeft17(): Bool =
    	LazyList1.foldLeft((acc, x) -> x :: acc, Nil, LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil)))))) == 3 :: 2 :: 1 :: Nil

    @test
    def foldLeft18(): Bool =
    	LazyList1.foldLeft((acc, x) -> x :: acc, Nil, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) == 3 :: 2 :: 1 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool =
    	LazyList1.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, ENil) == 100

    @test
    def foldRight02(): Bool =
    	LazyList1.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, LList(lazy ENil)) == 100

    @test
    def foldRight03(): Bool =
    	LazyList1.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, LCons(1, lazy ENil)) == 198

    @test
    def foldRight04(): Bool =
    	LazyList1.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, LList(lazy ECons(1, LList(lazy ENil)))) == 198

    @test
    def foldRight05(): Bool =
    	LazyList1.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, ECons(1, LList(lazy ENil))) == 198

    @test
    def foldRight06(): Bool =
    	LazyList1.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, LCons(1, lazy LList(lazy ENil))) == 198

    @test
    def foldRight07(): Bool =
    	LazyList1.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil))))) == 194

    @test
    def foldRight08(): Bool =
    	LazyList1.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, LList(lazy LCons(1, lazy ECons(2, LList(lazy ENil))))) == 194

    @test
    def foldRight09(): Bool =
    	LazyList1.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, LList(lazy LCons(1, lazy LCons(2, lazy ENil)))) == 194

    @test
    def foldRight10(): Bool =
    	LazyList1.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, ECons(1, LCons(2, lazy ENil))) == 194

    @test
    def foldRight11(): Bool =
    	LazyList1.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))) == 382

    @test
    def foldRight12(): Bool =
    	LazyList1.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))) == 382

    @test
    def foldRight13(): Bool =
    	LazyList1.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil)))))) == 382

    @test
    def foldRight14(): Bool =
    	LazyList1.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) == 382

    @test
    def foldRight15(): Bool =
    	LazyList1.foldRight((x, acc) -> x :: acc, Nil, LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))) == 1 :: 2 :: 3 :: Nil

    @test
    def foldRight16(): Bool =
    	LazyList1.foldRight((x, acc) -> x :: acc, Nil, LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))) == 1 :: 2 :: 3 :: Nil

    @test
    def foldRight17(): Bool =
    	LazyList1.foldRight((x, acc) -> x :: acc, Nil, LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil)))))) == 1 :: 2 :: 3 :: Nil

    @test
    def foldRight18(): Bool =
    	LazyList1.foldRight((x, acc) -> x :: acc, Nil, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) == 1 :: 2 :: 3 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toArray01(): Bool & Impure =
    	LazyList1.toArray(ENil: LazyList[Unit]) == []

    @test
    def toArray02(): Bool & Impure =
    	LazyList1.toArray(LList(lazy ENil): LazyList[Unit]) == []

    @test
    def toArray03(): Bool & Impure =
    	LazyList1.range(-100, 1000) |> LazyList1.toArray == Array.range(-100, 1000)

    @test
    def toArray04(): Bool & Impure =
    	LazyList1.toArray(LCons(1, lazy ENil)) == [1]

    @test
    def toArray05(): Bool & Impure =
    	LazyList1.toArray(LList(lazy ECons(1, LList(lazy ENil)))) == [1]

    @test
    def toArray06(): Bool & Impure =
    	LazyList1.toArray(ECons(1, LList(lazy ENil))) == [1]

    @test
    def toArray07(): Bool & Impure =
    	LazyList1.toArray(LCons(1, lazy LList(lazy ENil))) == [1]

    @test
    def toArray08(): Bool & Impure =
    	LazyList1.toArray(LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))) == [1, 2, 3]

    @test
    def toArray09(): Bool & Impure =
    	LazyList1.toArray(LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))) == [1, 2, 3]

    @test
    def toArray10(): Bool & Impure =
    	LazyList1.toArray(LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil)))))) == [1, 2, 3]

    @test
    def toArray11(): Bool & Impure =
    	LazyList1.toArray(LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) == [1, 2, 3]


    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toList01(): Bool =
    	LazyList1.toList(ENil): List[Unit] == Nil

    @test
    def toList02(): Bool =
    	LazyList1.toList(LList(lazy ENil)): List[Unit] == Nil

    @test
    def toList03(): Bool =
        LazyList1.range(-1000, 1000) |> LazyList1.toList == List.range(-1000, 1000)

    @test
    def toList04(): Bool =
        LazyList1.range(-1000, 1000000) |> LazyList1.take(1000) |> LazyList1.toList == List.range(-1000, 0)

    @test
    def toList05(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList1.toList == (1 :: 2 :: 3 :: Nil)

    @test
    def toList06(): Bool =
    	LazyList1.toList(LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))) == 1 :: 2 :: 3 :: Nil

    @test
    def toList07(): Bool =
    	LazyList1.toList(LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))) == 1 :: 2 :: 3 :: Nil

    @test
    def toList08(): Bool =
    	LazyList1.toList(LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil)))))) == 1 :: 2 :: 3 :: Nil

    @test
    def toList09(): Bool =
    	LazyList1.toList(LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) == 1 :: 2 :: 3 :: Nil

    @test
    def toList10(): Bool =
    	LazyList1.toList(LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, ENil))))))) == 1 :: 2 :: 3 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMap01(): Bool = LazyList1.toMap(ENil: LazyList[(Unit, Unit)]) == Map#{}

    @test
    def toMap02(): Bool = LazyList1.toMap(LList(lazy ENil): LazyList[(Unit, Unit)]) == Map#{}

    @test
    def toMap03(): Bool = LazyList1.toMap(ECons((1, true), ENil)) == Map#{1 -> true}

    @test
    def toMap04(): Bool = LazyList1.toMap(LCons((1, true), lazy ENil)) == Map#{1 -> true}

    @test
    def toMap05(): Bool = LazyList1.toMap(LList(lazy ECons((1, true), ENil))) == Map#{1 -> true}

    @test
    def toMap06(): Bool = LazyList1.toMap(LList(lazy LCons((1, true), lazy ENil))) == Map#{1 -> true}

    // ECons / LCons combinations
    @test
    def toMap07(): Bool = LazyList1.toMap(ECons((1, true), ECons((2, false), ENil))) == Map#{1 -> true, 2 -> false}

    @test
    def toMap08(): Bool = LazyList1.toMap(LCons((1, true), lazy ECons((2, false), ENil))) == Map#{1 -> true, 2 -> false}

    @test
    def toMap09(): Bool = LazyList1.toMap(ECons((1, true), LCons((2, false), lazy ENil))) == Map#{1 -> true, 2 -> false}

    @test
    def toMap10(): Bool = LazyList1.toMap(LCons((1, true), lazy LCons((2, false), lazy ENil))) == Map#{1 -> true, 2 -> false}

    @test
    def toMap11(): Bool = LazyList1.toMap(LList(lazy LCons((1, true), lazy LList(lazy ECons((2, false), ENil))))) == Map#{1 -> true, 2 -> false}

    @test
    def toMap12(): Bool = LazyList1.toMap(LList(lazy ECons((1, true), LList(lazy LCons((2, false), lazy ENil))))) == Map#{1 -> true, 2 -> false}

    @test
    def toMap13(): Bool = LazyList1.toMap(LList(lazy LCons((1, true), lazy LList(lazy LCons((2, false), lazy ENil))))) == Map#{1 -> true, 2 -> false}

    @test
    def toMap14(): Bool = LazyList1.toMap(ECons((1, true), ECons((1, false), ENil))) == Map#{1 -> false}


    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toSet01(): Bool =
    	LazyList1.toSet(ENil): Set[Unit] == Set.empty()

    @test
    def toSet02(): Bool =
    	LazyList1.toSet(LList(lazy ENil)): Set[Unit] == Set.empty()

    @test
    def toSet03(): Bool =
        LazyList1.range(-1000, 1000) |> LazyList1.toSet == Set.range(-1000, 1000)

    @test
    def toSet04(): Bool =
        LazyList1.range(-1000, 1000000) |> LazyList1.take(1000) |> LazyList1.toSet == Set.range(-1000, 0)

    @test
    def toSet05(): Bool =
    	LazyList1.toSet(LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))) == Set#{1, 2, 3}

    @test
    def toSet06(): Bool =
    	LazyList1.toSet(LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))) == Set#{1, 2, 3}

    @test
    def toSet07(): Bool =
    	LazyList1.toSet(LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil)))))) == Set#{1, 2, 3}

    @test
    def toSet08(): Bool =
    	LazyList1.toSet(LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) == Set#{1, 2, 3}

    @test
    def toSet09(): Bool =
    	LazyList1.toSet(LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, ENil))))))) == Set#{1, 2, 3}

    @test
    def toSet10(): Bool =
    	LazyList1.toSet(ECons(1, LCons(2, lazy ECons(3, ENil)))) == Set#{1, 2, 3}

        
    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def append01(): Bool =
    	LazyList1.append(ENil: LazyList[Unit], ENil: LazyList[Unit]) == ENil

    @test
    def append02(): Bool =
    	LazyList1.append(LList(lazy ENil): LazyList[Unit], ENil: LazyList[Unit]) == ENil

    @test
    def append03(): Bool =
    	LazyList1.append(ENil, LList(lazy ENil): LazyList[Unit]) == ENil

    @test
    def append04(): Bool =
    	LazyList1.append(LList(lazy ENil), LList(lazy ENil): LazyList[Unit]) == ENil

    @test
    def append05(): Bool =
    	LazyList1.append(LCons(1, lazy ENil), LList(lazy ENil)) == ECons(1, ENil)

    @test
    def append06(): Bool =
    	LazyList1.append(LList(lazy ECons(1, LList(lazy ENil))), LList(lazy ENil)) == ECons(1, ENil)

    @test
    def append07(): Bool =
    	LazyList1.append(ECons(1, LList(lazy ENil)), LList(lazy ENil)) == ECons(1, ENil)

    @test
    def append08(): Bool =
    	LazyList1.append(LCons(1, lazy ENil), ENil) == ECons(1, ENil)

    @test
    def append09(): Bool =
    	LazyList1.append(ENil, LCons(1, lazy ENil)) == ECons(1, ENil)

    @test
    def append10(): Bool =
    	LazyList1.append(ENil, LList(lazy ECons(1, LList(lazy ENil)))) == ECons(1, ENil)

    @test
    def append11(): Bool =
    	LazyList1.append(LList(lazy ENil), ECons(1, LList(lazy ENil))) == ECons(1, ENil)

    @test
    def append12(): Bool =
    	LazyList1.append(LList(lazy ENil), LCons(1, lazy LList(lazy ENil))) == ECons(1, ENil)

    @test
    def append13(): Bool =
    	LazyList1.append(ECons(1, ECons(2, ECons(3, ENil))), ECons(4, LList(lazy ENil))) == ECons(1, ECons(2, ECons(3, ECons(4, ENil))))

    @test
    def append14(): Bool =
    	LazyList1.append(ECons(1, ECons(2, ECons(3, ENil))), LCons(4, lazy LList(lazy ENil))) == ECons(1, ECons(2, ECons(3, ECons(4, ENil))))

    @test
    def append15(): Bool =
    	LazyList1.append(ECons(4, ENil), ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy ENil))))) == ECons(4, ECons(1, ECons(2, ECons(3, ENil))))

    @test
    def append16(): Bool =
    	LazyList1.append(ECons(4, ENil), LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))) == ECons(4, ECons(1, ECons(2, ECons(3, ENil))))


    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def count01(): Bool =
        LazyList1.count(x -> x > 3, ENil) == 0

    @test
    def count02(): Bool =
        LazyList1.count(x -> x > 3, LList(lazy ENil)) == 0

    @test
    def count03(): Bool =
    	LazyList1.count(x -> x > 3, LList(lazy LCons(1, lazy LList(lazy LCons(1, lazy LList(lazy LCons(1, lazy ENil))))))) == 0

    @test
    def count04(): Bool =
    	LazyList1.count(x -> x > 3, LCons(1, lazy LCons(1, lazy LCons(1, lazy ENil)))) == 0

    @test
    def count05(): Bool =
    	LazyList1.count(x -> x > 3, ECons(4, ENil)) == 1

    @test
    def count06(): Bool =
    	LazyList1.count(x -> x > 3, LList(lazy LCons(4, lazy ENil))) == 1

    @test
    def count07(): Bool =
        LazyList1.count(_ -> true, LazyList1.range(1000, 2000)) == 1000

    @test
    def count08(): Bool =
        LazyList1.count(_ -> false, LazyList1.range(1000, 2000)) == 0

    @test
    def count09(): Bool =
        LazyList1.count(x -> x > 3, ECons(1, ECons(2, ENil))) == 0

    @test
    def count10(): Bool =
        LazyList1.count(x -> x > 3, ECons(1, ECons(8, ENil))) == 1

    @test
    def count11(): Bool =
        LazyList1.count(x -> x > 3, ECons(8, ECons(1, ENil))) == 1


    /////////////////////////////////////////////////////////////////////////////
    // drop                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def drop01(): Bool =
        LazyList1.drop(-1, ENil: LazyList[Unit]) == ENil

    @test
    def drop02(): Bool =
        LazyList1.drop(0, ENil: LazyList[Unit]) == ENil

    @test
    def drop03(): Bool =
        LazyList1.drop(-1, LList(lazy ENil): LazyList[Unit]) == ENil

    @test
    def drop04(): Bool =
        LazyList1.drop(0, LList(lazy ENil): LazyList[Unit]) == ENil

    @test
    def drop05(): Bool =
        LazyList1.drop(1, ENil: LazyList[Unit]) == ENil

    @test
    def drop06(): Bool =
        LazyList1.drop(1, LList(lazy ENil): LazyList[Unit]) == ENil

    @test
    def drop07(): Bool =
        LazyList1.drop(-1, ECons(1, ENil)) == ECons(1, ENil)

    @test
    def drop08(): Bool =
        LazyList1.drop(0, ECons(1, ENil)) == ECons(1, ENil)

    @test
    def drop09(): Bool =
        LazyList1.drop(1, ECons(1, ENil)) == ENil

    @test
    def drop10(): Bool =
    	LazyList1.drop(1, LCons(1, lazy ENil)) == ENil

    @test
    def drop11(): Bool =
    	LazyList1.drop(1, LList(lazy ECons(1, LList(lazy ENil)))) == ENil

    @test
    def drop12(): Bool =
        LazyList1.drop(2, ECons(1, ENil)) == ENil

    @test
    def drop13(): Bool =
    	LazyList1.drop(2, LCons(1, lazy ENil)) == ENil

    @test
    def drop14(): Bool =
    	LazyList1.drop(2, LList(lazy ECons(1, LList(lazy ENil)))) == ENil

    @test
    def drop15(): Bool =
    	LazyList1.range(0, 1000) |> LazyList1.drop(500) == LazyList1.range(500, 1000)


    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatten01(): Bool =
        LazyList1.flatten(ENil: LazyList[LazyList[Unit]]) == ENil

    @test
    def flatten02(): Bool =
        LazyList1.flatten(LList(lazy ENil): LazyList[LazyList[Unit]]) == ENil

    @test
    def flatten03(): Bool =
        LazyList1.flatten(ECons(ENil, ENil)): LazyList[Unit] == ENil

    @test
    def flatten04(): Bool =
        LazyList1.flatten(ECons(LList(lazy ENil), ENil)): LazyList[Unit] == ENil

    @test
    def flatten05(): Bool =
        LazyList1.flatten(LList(lazy ECons(ENil, ENil))): LazyList[Unit] == ENil

    @test
    def flatten06(): Bool =
        LazyList1.flatten(ECons(ENil, LList(lazy ENil))): LazyList[Unit] == ENil

    @test
    def flatten07(): Bool =
        LazyList1.flatten(ECons(ENil, ECons(ENil, ENil))): LazyList[Unit] == ENil

    @test
    def flatten08(): Bool =
    	LazyList1.flatten(ECons(LCons(1, lazy ENil), ENil)) == ECons(1, ENil)

    @test
    def flatten09(): Bool =
    	LazyList1.flatten(ECons(LList(lazy ECons(1, LList(lazy ENil))), ENil)) == ECons(1, ENil)

    @test
    def flatten10(): Bool =
    	LazyList1.flatten(ECons(ECons(1, LList(lazy ENil)), ENil)) == ECons(1, ENil)

    @test
    def flatten11(): Bool =
    	LazyList1.flatten(ECons(LCons(1, lazy LList(lazy ENil)), ENil)) == ECons(1, ENil)

    @test
    def flatten12(): Bool =
        LazyList1.flatten(ECons((ECons(1, ECons(2, ENil))), ENil)) == ECons(1, ECons(2, ENil))

    @test
    def flatten13(): Bool =
    	LazyList1.flatten(ECons(LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil)))), ENil)) == ECons(1, ECons(2, ENil))

    @test
    def flatten14(): Bool =
    	LazyList1.flatten(ECons(ENil, ECons(ECons(1, ECons(2, ENil)), ENil))) == ECons(1, ECons(2, ENil))

    @test
    def flatten15(): Bool =
    	LazyList1.flatten(ECons(ENil, ECons(LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil)))), ENil))) == ECons(1, ECons(2, ENil))

    @test
    def flatten16(): Bool =
    	LazyList1.flatten(ECons(ENil, ECons(LList(lazy LCons(1, lazy ECons(2, LList(lazy ENil)))), ENil))) == ECons(1, ECons(2, ENil))

    @test
    def flatten17(): Bool =
    	LazyList1.flatten(ECons(LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil)))), ECons((ECons(1, ECons(2, ENil))), ENil))) == ECons(1, ECons(2, ECons(1, ECons(2, ENil))))

    @test
    def flatten18(): Bool =
    	LazyList1.flatten(ECons(LList(lazy LCons(1, lazy ECons(2, LList(lazy ENil)))), ECons((ECons(1, ECons(2, ENil))), ENil))) == ECons(1, ECons(2, ECons(1, ECons(2, ENil))))

    @test
    def flatten19(): Bool =
    	LazyList1.flatten(ECons((ECons(1, ECons(2, ENil))), ECons(LList(lazy LCons(1, lazy LCons(2, lazy ENil))), ENil))) == ECons(1, ECons(2, ECons(1, ECons(2, ENil))))

    @test
    def flatten20(): Bool =
    	LazyList1.flatten(ECons(ECons(1, LCons(2, lazy ENil)), ECons((ECons(1, ECons(2, ENil))), ENil))) == ECons(1, ECons(2, ECons(1, ECons(2, ENil))))

    @test
    def flatten21(): Bool =
    	LazyList1.flatten(ECons((ECons(1, ECons(2, ENil))), ECons(ENil, ECons(ENil, ECons(LList(lazy LCons(3, lazy LCons(4, lazy ENil))), ENil))))) == ECons(1, ECons(2, ECons(3, ECons(4, ENil))))

    @test
    def flatten22(): Bool =
    	LazyList1.flatten(ECons(ECons(1, LCons(2, lazy ENil)), ECons(ENil, ECons(ENil, ECons((ECons(3, ECons(4, ENil))), ENil))))) == ECons(1, ECons(2, ECons(3, ECons(4, ENil))))


    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def exists01(): Bool = LazyList1.exists(i -> i > 3, ENil) == false

    @test
    def exists02(): Bool = (1 :: Nil) |> List.toLazy |> LazyList1.exists(i -> i > 3) == false

    @test
    def exists03(): Bool = (5 :: Nil) |> List.toLazy |> LazyList1.exists(i -> i > 3) == true

    @test
    def exists04(): Bool =  (1 :: 2 :: Nil) |> List.toLazy |> LazyList1.exists(i -> i > 3) == false

    @test
    def exists05(): Bool =  (1 :: 6 :: Nil) |> List.toLazy |> LazyList1.exists(i -> i > 3) == true

    @test
    def exists06(): Bool =  (6 :: 1 :: Nil) |> List.toLazy |> LazyList1.exists(i -> i > 3) == true

    @test
    def exists07(): Bool = (16 :: 6 :: Nil) |> List.toLazy |> LazyList1.exists(i -> i > 3) == true

    @test
    def exists08(): Bool = (1 :: -9 :: 3 :: Nil) |> List.toLazy |> LazyList1.exists(i -> i > 3) == false

    @test
    def exists09(): Bool = (1 :: 9 :: 3 :: Nil) |> List.toLazy |> LazyList1.exists(i -> i > 3) == true


    /////////////////////////////////////////////////////////////////////////////
    // forall                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forall01(): Bool = LazyList1.forall(i -> i > 3, ENil) == true

    @test
    def forall02(): Bool = (1 :: Nil) |> List.toLazy |> LazyList1.forall(i -> i > 3) == false

    @test
    def forall03(): Bool = (5 :: Nil) |> List.toLazy |> LazyList1.forall(i -> i > 3) == true

    @test
    def forall04(): Bool =  (1 :: 2 :: Nil) |> List.toLazy |> LazyList1.forall(i -> i > 3) == false

    @test
    def forall05(): Bool =  (1 :: 6 :: Nil) |> List.toLazy |> LazyList1.forall(i -> i > 3) == false

    @test
    def forall06(): Bool =  (6 :: 1 :: Nil) |> List.toLazy |> LazyList1.forall(i -> i > 3) == false

    @test
    def forall07(): Bool = (16 :: 6 :: Nil) |> List.toLazy |> LazyList1.forall(i -> i > 3) == true

    @test
    def forall08(): Bool = (1 :: -9 :: 3 :: Nil) |> List.toLazy |> LazyList1.forall(i -> i > 3) == false

    @test
    def forall09(): Bool = (1 :: 9 :: 3 :: Nil) |> List.toLazy |> LazyList1.forall(i -> i > 3) == false

    @test
    def forall10(): Bool = (11 :: 9 :: 31 :: Nil) |> List.toLazy |> LazyList1.forall(i -> i > 3) == true


    /////////////////////////////////////////////////////////////////////////////
    // repeat                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def repeat01(): Bool =
        LazyList1.repeat(1) |> LazyList1.take(-1) == ENil

    @test
    def repeat02(): Bool =
        LazyList1.repeat(1) |> LazyList1.take(0) == ENil

    @test
    def repeat03(): Bool =
        LazyList1.repeat(1) |> LazyList1.take(1) == ECons(1, ENil)

    @test
    def repeat04(): Bool =
        LazyList1.repeat(1) |> LazyList1.take(2) == ECons(1, ECons(1, ENil))

    @test
    def repeat05(): Bool =
        LazyList1.repeat(1) |> LazyList1.take(3) == ECons(1, ECons(1, ECons(1, ENil)))

    @test
    def repeat07(): Bool =
        LazyList1.repeat(3) |> LazyList1.take(3) == ECons(3, ECons(3, ECons(3, ENil)))

    @test
    def repeat08(): Bool =
        LazyList1.repeat("a") |> LazyList1.take(3) == ECons("a", ECons("a", ECons("a", ENil)))


    /////////////////////////////////////////////////////////////////////////////
    // from                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def from01(): Bool =
        LazyList1.from(-1) |> LazyList1.take(0) == ENil

    @test
    def from02(): Bool =
        LazyList1.from(0) |> LazyList1.take(0) == ENil

    @test
    def from03(): Bool =
        LazyList1.from(-1) |> LazyList1.take(0) == LList(lazy ENil)

    @test
    def from04(): Bool =
        LazyList1.from(0) |> LazyList1.take(0) == LList(lazy ENil)

    @test
    def from05(): Bool =
        LazyList1.from(100) |> LazyList1.take(-1) == ENil

    @test
    def from06(): Bool =
        LazyList1.from(100) |> LazyList1.take(-1) == LList(lazy ENil)

    @test
    def from07(): Bool =
    	LazyList1.from(-1) |> LazyList1.take(1) == ECons(-1, ENil)

    @test
    def from08(): Bool =
    	LazyList1.from(-1) |> LazyList1.take(1) == LCons(-1, lazy ENil)

    @test
    def from09(): Bool =
    	LazyList1.from(5) |> LazyList1.take(1) == LList(lazy ECons(5, ENil))

    @test
    def from10(): Bool =
    	LazyList1.from(5) |> LazyList1.take(1) == LList(lazy LCons(5, lazy LList(lazy ENil)))

    @test
    def from11(): Bool =
    	LazyList1.from(1) |> LazyList1.take(3) == LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))

    @test
    def from12(): Bool =
    	LazyList1.from(1) |> LazyList1.take(3) == LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))

    @test
    def from13(): Bool =
    	LazyList1.from(-1000) |> LazyList1.take(1000) == LazyList1.range(-1000, 0)

    @test
    def from14(): Bool =
    	LazyList1.from(-1000) |> LazyList1.take(2000) == LazyList1.range(-1000, 1000)

    @test
    def from15(): Bool =
    	LazyList1.from(0) |> LazyList1.take(2000) == LazyList1.range(0, 2000)


    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def last01(): Bool =
        LazyList1.last(ENil: LazyList[Unit]) == None

    @test
    def last02(): Bool =
        LazyList1.last(LList(lazy ENil): LazyList[Unit]) == None

    @test
    def last03(): Bool =
        LazyList1.range(0, 1) |> LazyList1.last == Some(0)

    @test
    def last04(): Bool =
    	LCons(1, lazy ENil) |> LazyList1.last == Some(1)

    @test
    def last05(): Bool =
    	LList(lazy ECons(1, LList(lazy ENil))) |> LazyList1.last == Some(1)

    @test
    def last06(): Bool =
    	ECons(1, LList(lazy ENil)) |> LazyList1.last == Some(1)

    @test
    def last07(): Bool =
    	LCons(1, lazy LList(lazy ENil)) |> LazyList1.last == Some(1)

    @test
    def last08(): Bool =
    	LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil)))) |> LazyList1.last == Some(3)

    @test
    def last09(): Bool =
    	LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil)))) |> LazyList1.last == Some(3)

    @test
    def last10(): Bool =
    	LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil))))) |> LazyList1.last == Some(3)

    @test
    def last11(): Bool =
    	LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil)))))) |> LazyList1.last == Some(3)

    @test
    def last12(): Bool =
    	LazyList1.range(0, 1000) |> LazyList1.last == Some(999)


    /////////////////////////////////////////////////////////////////////////////
    // toIter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toIter01(): Bool & Impure =
    	LazyList1.toIter(ENil: LazyList[Unit]) |> Iterator.toList == Nil

    @test
    def toIter02(): Bool & Impure =
    	LazyList1.toIter(LList(lazy ENil): LazyList[Unit]) |> Iterator.toList == Nil

    @test
    def toIter03(): Bool & Impure =
    	LazyList1.toIter(LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))) |> Iterator.toList == 1 :: 2 :: 3 :: Nil

    @test
    def toIter04(): Bool & Impure =
    	LazyList1.toIter(LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))) |> Iterator.toList == 1 :: 2 :: 3 :: Nil

    @test
    def toIter05(): Bool & Impure =
    	LazyList1.toIter(LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil)))))) |> Iterator.toList == 1 :: 2 :: 3 :: Nil

    @test
    def toIter06(): Bool & Impure =
    	LazyList1.toIter(LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) |> Iterator.toList == 1 :: 2 :: 3 :: Nil

    @test
    def toIter07(): Bool & Impure =
    	LazyList1.toIter(LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, ENil))))))) |> Iterator.toList == 1 :: 2 :: 3 :: Nil

    @test
    def toIter08(): Bool & Impure =
    	LazyList1.toIter(ECons(1, LCons(2, lazy ECons(3, ENil)))) |> Iterator.toList == 1 :: 2 :: 3 :: Nil

    @test
    def toIter09(): Bool & Impure =
    	LazyList1.range(-100, 100) |> LazyList1.toIter |> Iterator.toList == List.range(-100, 100)


    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zip01(): Bool =
        LazyList1.zip(ENil: LazyList[Int32], ENil: LazyList[String]) == ENil

    @test
    def zip02(): Bool =
        LazyList1.zip(LList(lazy ENil): LazyList[Int32], ENil: LazyList[String]) == ENil

    @test
    def zip03(): Bool =
    	LazyList1.zip(ENil: LazyList[Int32], LList(lazy ENil): LazyList[String]) == ENil

    @test
    def zip04(): Bool =
    	LazyList1.zip(LList(lazy ENil): LazyList[Int32], LList(lazy ENil): LazyList[String]) == ENil

    @test
    def zip05(): Bool =
        LazyList1.zip(LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil)))), ENil: LazyList[String]) == ENil

    @test
    def zip06(): Bool =
        LazyList1.zip(LList(lazy LCons(1, lazy ECons(2, LList(lazy ENil)))), LList(lazy ENil): LazyList[String]) == ENil

    @test
    def zip07(): Bool =
        LazyList1.zip(LList(lazy LCons(1, lazy LCons(2, lazy ENil))), ENil: LazyList[String]) == ENil

    @test
    def zip08(): Bool =
    	LazyList1.zip(ENil: LazyList[String], LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil))))) == ENil

    @test
    def zip09(): Bool =
    	LazyList1.zip(LList(lazy ENil): LazyList[String], LList(lazy LCons(1, lazy ECons(2, LList(lazy ENil))))) == ENil

    @test
    def zip10(): Bool =
    	LazyList1.zip(ENil: LazyList[String], LList(lazy LCons(1, lazy LCons(2, lazy ENil)))) == ENil

    @test
    def zip11(): Bool =
    	LazyList1.zip(LCons("a", lazy ECons("b", LList(lazy ECons("c", ENil)))), LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil))))) == ECons(("a", 1), ECons(("b", 2), ENil))

    @test
    def zip12(): Bool =
    	LazyList1.zip(LCons("a", lazy ECons("b", LList(lazy ECons("c", ENil)))), LList(lazy LCons(1, lazy ECons(2, LList(lazy ENil))))) == ECons(("a", 1), ECons(("b", 2), ENil))

    @test
    def zip13(): Bool =
    	LazyList1.zip(LCons("a", lazy ECons("b", LList(lazy ECons("c", ENil)))), LList(lazy LCons(1, lazy LCons(2, lazy ENil)))) == ECons(("a", 1), ECons(("b", 2), ENil))

    @test
    def zip14(): Bool =
    	LazyList1.zip(LCons("a", lazy ECons("b", LList(lazy ECons("c", ENil)))), LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))) == ECons(("a", 1), ECons(("b", 2), ECons(("c", 3), ENil)))

    @test
    def zip15(): Bool =
    	LazyList1.zip(LCons("a", lazy ECons("b", LList(lazy ECons("c", ENil)))), LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))) == ECons(("a", 1), ECons(("b", 2), ECons(("c", 3), ENil)))

    @test
    def zip16(): Bool =
    	LazyList1.zip(LCons("a", lazy ECons("b", LList(lazy ECons("c", ENil)))), LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil)))))) == ECons(("a", 1), ECons(("b", 2), ECons(("c", 3), ENil)))

    @test
    def zip17(): Bool =
    	LazyList1.zip(LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil)))), LCons("a", lazy ECons("b", LList(lazy ECons("c", ENil))))) == ECons((1, "a"), ECons((2, "b"), ECons((3, "c"), ENil)))

    @test
    def zip18(): Bool =
    	LazyList1.zip(LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil)))), LCons("a", lazy ECons("b", LList(lazy ECons("c", ENil))))) == ECons((1, "a"), ECons((2, "b"), ECons((3, "c"), ENil)))

    @test
    def zip19(): Bool =
    	LazyList1.zip(LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil))))), LCons("a", lazy ECons("b", LList(lazy ECons("c", ENil))))) == ECons((1, "a"), ECons((2, "b"), ECons((3, "c"), ENil)))

    @test
    def zip20(): Bool =
    	LazyList1.zip(LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil)))), LCons("a", lazy ECons("b", LList(lazy ECons("c", ENil))))) == ECons((1, "a"), ECons((2, "b"), ENil))

    @test
    def zip21(): Bool =
    	LazyList1.zip(LList(lazy LCons(1, lazy ECons(2, LList(lazy ENil)))), LCons("a", lazy ECons("b", LList(lazy ECons("c", ENil))))) == ECons((1, "a"), ECons((2, "b"), ENil))


    /////////////////////////////////////////////////////////////////////////////
    // zipWith (pure)                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWithPure01(): Bool =
        ENil: LazyList[Int32] |> LazyList1.zipWith((x, y) -> x + y, ENil: LazyList[Int32]) == ENil

    @test
    def zipWithPure02(): Bool =
    	LList(lazy (ENil: LazyList[Int32])) |> LazyList1.zipWith((x, y) -> x + y, ENil: LazyList[Int32]) == ENil

    @test
    def zipWithPure03(): Bool =
    	ENil: LazyList[Int32] |> LazyList1.zipWith((x, y) -> x + y, LList(lazy (ENil: LazyList[Int32]))) == ENil

    @test
    def zipWithPure04(): Bool =
    	LList(lazy (ENil: LazyList[Int32])) |> LazyList1.zipWith((x, y) -> x + y, LList(lazy (ENil: LazyList[Int32]))) == ENil

    @test
    def zipWithPure05(): Bool =
    	ECons(1, ECons(2, ENil)) |> LazyList1.zipWith((x, y) -> x + y, ENil: LazyList[Int32]) == ENil

    @test
    def zipWithPure06(): Bool =
    	LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil)))) |> LazyList1.zipWith((x, y) -> x + y, ENil: LazyList[Int32]) == ENil

    @test
    def zipWithPure07(): Bool =
    	 ENil: LazyList[Int32] |> LazyList1.zipWith((x, y) -> x + y, ECons(1, ECons(2, ENil))) == ENil

    @test
    def zipWithPure08(): Bool =
    	 ENil: LazyList[Int32] |> LazyList1.zipWith((x, y) -> x + y, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil))))) == ENil

    @test
    def zipWithPure09(): Bool =
    	LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil)))) |> LazyList1.zipWith((x, y) -> x + y, ECons(1, ECons(2, ENil))) == ECons(2, ECons(4, ENil))

    @test
    def zipWithPure10(): Bool =
    	LList(lazy LCons(1, lazy ECons(2, LList(lazy ENil)))) |> LazyList1.zipWith((x, y) -> x + y, ECons(1, ECons(2, ENil))) == ECons(2, ECons(4, ENil))

    @test
    def zipWithPure11(): Bool =
    	LList(lazy LCons(1, lazy LCons(2, lazy ENil))) |> LazyList1.zipWith((x, y) -> x + y, ECons(1, ECons(2, ENil))) == ECons(2, ECons(4, ENil))

    @test
    def zipWithPure12(): Bool =
    	ECons(1, ECons(2, ENil)) |> LazyList1.zipWith((x, y) -> x + y, ECons(1, ECons(2, ENil))) == ECons(2, ECons(4, ENil))

    @test
    def zipWithPure13(): Bool =
    	LCons(1, lazy ENil) |> LazyList1.zipWith((x, y) -> x + y, ECons(1, ECons(2, ENil))) == ECons(2, ENil)

    @test
    def zipWithPure14(): Bool =
    	LList(lazy ECons(1, LList(lazy ENil))) |> LazyList1.zipWith((x, y) -> x + y, ECons(1, ECons(2, ENil))) == ECons(2, ENil)

    @test
    def zipWithPure15(): Bool =
    	ECons(1, ENil) |> LazyList1.zipWith((x, y) -> x + y, ECons(1, ECons(2, ENil))) == ECons(2, ENil)

    @test
    def zipWithPure016(): Bool =
    	 ECons(1, ECons(2, ENil)) |> LazyList1.zipWith((x, y) -> x + y, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil))))) == ECons(2, ECons(4, ENil))

    @test
    def zipWithPure17(): Bool =
    	 ECons(1, ECons(2, ENil)) |> LazyList1.zipWith((x, y) -> x + y, LList(lazy LCons(1, lazy ECons(2, LList(lazy ENil))))) == ECons(2, ECons(4, ENil))

    @test
    def zipWithPure18(): Bool =
    	ECons(1, ECons(2, ENil)) |> LazyList1.zipWith((x, y) -> x + y, LList(lazy LCons(1, lazy LCons(2, lazy ENil)))) == ECons(2, ECons(4, ENil))

    @test
    def zipWithPure19(): Bool =
    	ECons(1, ECons(2, ENil)) |> LazyList1.zipWith((x, y) -> x + y, ECons(1, ECons(2, ENil))) == ECons(2, ECons(4, ENil))

    @test
    def zipWithPure20(): Bool =
    	ECons(1, ECons(2, ENil)) |> LazyList1.zipWith((x, y) -> x + y, LCons(1, lazy ENil)) == ECons(2, ENil)

    @test
    def zipWithPure21(): Bool =
    	ECons(1, ECons(2, ENil)) |> LazyList1.zipWith((x, y) -> x + y, LList(lazy ECons(1, LList(lazy ENil)))) == ECons(2, ENil)

    @test
    def zipWithPure22(): Bool =
    	ECons(1, ECons(2, ENil)) |> LazyList1.zipWith((x, y) -> x + y, ECons(1, ENil)) == ECons(2, ENil)


    /////////////////////////////////////////////////////////////////////////////
    // zipWith (impure)                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWithImpure01(): Bool & Impure =
        ENil: LazyList[Int32] |> LazyList1.zipWith((x, y) -> x + y as & Impure, ENil: LazyList[Int32]) == ENil

    @test
    def zipWithImpure02(): Bool & Impure =
    	LList(lazy (ENil: LazyList[Int32])) |> LazyList1.zipWith((x, y) -> x + y as & Impure, ENil: LazyList[Int32]) == ENil

    @test
    def zipWithImpure03(): Bool & Impure =
    	ENil: LazyList[Int32] |> LazyList1.zipWith((x, y) -> x + y as & Impure, LList(lazy (ENil: LazyList[Int32]))) == ENil

    @test
    def zipWithImpure04(): Bool & Impure =
    	LList(lazy (ENil: LazyList[Int32])) |> LazyList1.zipWith((x, y) -> x + y as & Impure, LList(lazy (ENil: LazyList[Int32]))) == ENil

    @test
    def zipWithImpure05(): Bool & Impure =
    	ECons(1, ECons(2, ENil)) |> LazyList1.zipWith((x, y) -> x + y as & Impure, ENil: LazyList[Int32]) == ENil

    @test
    def zipWithImpure06(): Bool & Impure =
    	LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil)))) |> LazyList1.zipWith((x, y) -> x + y as & Impure, ENil: LazyList[Int32]) == ENil

    @test
    def zipWithImpure07(): Bool & Impure =
    	 ENil: LazyList[Int32] |> LazyList1.zipWith((x, y) -> x + y as & Impure, ECons(1, ECons(2, ENil))) == ENil

    @test
    def zipWithImpure08(): Bool & Impure =
    	 ENil: LazyList[Int32] |> LazyList1.zipWith((x, y) -> x + y as & Impure, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil))))) == ENil

    @test
    def zipWithImpure09(): Bool & Impure =
    	LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil)))) |> LazyList1.zipWith((x, y) -> x + y as & Impure, ECons(1, ECons(2, ENil))) == ECons(2, ECons(4, ENil))

    @test
    def zipWithImpure10(): Bool & Impure =
    	LList(lazy LCons(1, lazy ECons(2, LList(lazy ENil)))) |> LazyList1.zipWith((x, y) -> x + y as & Impure, ECons(1, ECons(2, ENil))) == ECons(2, ECons(4, ENil))

    @test
    def zipWithImpure11(): Bool & Impure =
    	LList(lazy LCons(1, lazy LCons(2, lazy ENil))) |> LazyList1.zipWith((x, y) -> x + y as & Impure, ECons(1, ECons(2, ENil))) == ECons(2, ECons(4, ENil))

    @test
    def zipWithImpure12(): Bool & Impure =
    	ECons(1, ECons(2, ENil)) |> LazyList1.zipWith((x, y) -> x + y as & Impure, ECons(1, ECons(2, ENil))) == ECons(2, ECons(4, ENil))

    @test
    def zipWithImpure13(): Bool & Impure =
    	LCons(1, lazy ENil) |> LazyList1.zipWith((x, y) -> x + y as & Impure, ECons(1, ECons(2, ENil))) == ECons(2, ENil)

    @test
    def zipWithImpure14(): Bool & Impure =
    	LList(lazy ECons(1, LList(lazy ENil))) |> LazyList1.zipWith((x, y) -> x + y as & Impure, ECons(1, ECons(2, ENil))) == ECons(2, ENil)

    @test
    def zipWithImpure15(): Bool & Impure =
    	ECons(1, ENil) |> LazyList1.zipWith((x, y) -> x + y as & Impure, ECons(1, ECons(2, ENil))) == ECons(2, ENil)

    @test
    def zipWithImpure016(): Bool & Impure =
    	 ECons(1, ECons(2, ENil)) |> LazyList1.zipWith((x, y) -> x + y as & Impure, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil))))) == ECons(2, ECons(4, ENil))

    @test
    def zipWithImpure17(): Bool & Impure =
    	 ECons(1, ECons(2, ENil)) |> LazyList1.zipWith((x, y) -> x + y as & Impure, LList(lazy LCons(1, lazy ECons(2, LList(lazy ENil))))) == ECons(2, ECons(4, ENil))

    @test
    def zipWithImpure18(): Bool & Impure =
    	ECons(1, ECons(2, ENil)) |> LazyList1.zipWith((x, y) -> x + y as & Impure, LList(lazy LCons(1, lazy LCons(2, lazy ENil)))) == ECons(2, ECons(4, ENil))

    @test
    def zipWithImpure19(): Bool & Impure =
    	ECons(1, ECons(2, ENil)) |> LazyList1.zipWith((x, y) -> x + y as & Impure, ECons(1, ECons(2, ENil))) == ECons(2, ECons(4, ENil))

    @test
    def zipWithImpure20(): Bool & Impure =
    	ECons(1, ECons(2, ENil)) |> LazyList1.zipWith((x, y) -> x + y as & Impure, LCons(1, lazy ENil)) == ECons(2, ENil)

    @test
    def zipWithImpure21(): Bool & Impure =
    	ECons(1, ECons(2, ENil)) |> LazyList1.zipWith((x, y) -> x + y as & Impure, LList(lazy ECons(1, LList(lazy ENil)))) == ECons(2, ENil)

    @test
    def zipWithImpure22(): Bool & Impure =
    	ECons(1, ECons(2, ENil)) |> LazyList1.zipWith((x, y) -> x + y as & Impure, ECons(1, ENil)) == ECons(2, ENil)


    /////////////////////////////////////////////////////////////////////////////
    // zipWith zipWith                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWithZipWith01(): Bool =
        ECons(1, ECons(2, ECons(3, ENil))) |>
            LazyList1.zipWith((x, y) -> x + y, ECons(1, ECons(2, ECons(3, ENil)))) |>
            LazyList1.zipWith((x, y) -> x + y, ECons(1, ECons(2, ECons(3, ENil)))) ==
            ECons(3, ECons(6, ECons(9, ENil)))

    @test
    def zipWithZipWith02(): Bool & Impure =
        ECons(1, ECons(2, ECons(3, ENil))) |>
            LazyList1.zipWith((x, y) -> x + y as & Impure, ECons(1, ECons(2, ECons(3, ENil)))) |>
            LazyList1.zipWith((x, y) -> x + y, ECons(1, ECons(2, ECons(3, ENil)))) ==
            ECons(3, ECons(6, ECons(9, ENil)))

    @test
    def zipWithZipWith03(): Bool & Impure =
        ECons(1, ECons(2, ECons(3, ENil))) |>
            LazyList1.zipWith((x, y) -> x + y, ECons(1, ECons(2, ECons(3, ENil)))) |>
            LazyList1.zipWith((x, y) -> x + y as & Impure, ECons(1, ECons(2, ECons(3, ENil)))) ==
            ECons(3, ECons(6, ECons(9, ENil)))

    @test
    def zipWithZipWith04(): Bool & Impure =
        ECons(1, ECons(2, ECons(3, ENil))) |>
            LazyList1.zipWith((x, y) -> x + y as & Impure, ECons(1, ECons(2, ECons(3, ENil)))) |>
            LazyList1.zipWith((x, y) -> x + y as & Impure, ECons(1, ECons(2, ECons(3, ENil)))) ==
            ECons(3, ECons(6, ECons(9, ENil)))


    /////////////////////////////////////////////////////////////////////////////
    // zipWith zipWith fusion                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWithFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |>
        LazyList1.zipWith((x, y) -> { l := "a" :: deref l; (x, y) }, ECons(1, ECons(2, ECons(3, ENil)))) |>
        LazyList1.zipWith((x, y) -> { l := "b" :: deref l; (x, y) }, ECons(1, ECons(2, ECons(3, ENil))));
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def zipWithFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |>
        LazyList1.zipWith((x, y) -> { l := "a" :: deref l; (x, y) } as & Pure, ECons(1, ECons(2, ECons(3, ENil)))) |>
        LazyList1.zipWith((x, y) -> { l := "b" :: deref l; (x, y) } as & Pure, ECons(1, ECons(2, ECons(3, ENil)))) |>
        LazyList1.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceLeft01(): Bool =
        LazyList1.reduceLeft((a, b) -> a + b, ENil: LazyList[Int]) == None

    @test
    def reduceLeft02(): Bool =
        LazyList1.reduceLeft((a, b) -> a + b, LList(lazy ENil): LazyList[Int]) == None

    @test
    def reduceLeft03(): Bool =
    	LazyList1.reduceLeft((a, b) -> a + b, LCons(1, lazy ENil)) == Some(1)

    @test
    def reduceLeft04(): Bool =
    	LazyList1.reduceLeft((a, b) -> a + b, LList(lazy ECons(1, LList(lazy ENil)))) == Some(1)

    @test
    def reduceLeft05(): Bool =
    	LazyList1.reduceLeft((a, b) -> a + b, ECons(1, LList(lazy ENil))) == Some(1)

    @test
    def reduceLeft06(): Bool =
    	LazyList1.reduceLeft((a, b) -> a + b, LCons(1, lazy LList(lazy ENil))) == Some(1)

    @test
    def reduceLeft07(): Bool =
    	LazyList1.reduceLeft((a, b) -> a + b, ECons(1, ENil)) == Some(1)

    @test
    def reduceLeft08(): Bool =
    	LazyList1.reduceLeft((a, b) -> a + b, LList(lazy LCons(1, lazy ENil))) == Some(1)

    @test
    def reduceLeft09(): Bool =
    	LazyList1.reduceLeft((a, b) -> a + b, LList(lazy LCons(1, lazy LList(lazy ENil)))) == Some(1)

    @test
    def reduceLeft10(): Bool =
    	LazyList1.reduceLeft((a, b) -> a + b, LList(lazy ECons(1, ENil))) == Some(1)

    @test
    def reduceLeft11(): Bool =
    	LazyList1.reduceLeft((a, b) -> a + b, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil))))) == Some(3)

    @test
    def reduceLeft12(): Bool =
    	LazyList1.reduceLeft((a, b) -> a + b, LList(lazy LCons(1, lazy ECons(2, LList(lazy ENil))))) == Some(3)

    @test
    def reduceLeft13(): Bool =
    	LazyList1.reduceLeft((a, b) -> a + b, ECons(1, LCons(2, lazy ENil))) == Some(3)

    @test
    def reduceLeft14(): Bool =
    	LazyList1.reduceLeft((a, b) -> a + b, LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))) == Some(6)

    @test
    def reduceLeft15(): Bool =
    	LazyList1.reduceLeft((a, b) -> a + b, LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))) == Some(6)

    @test
    def reduceLeft16(): Bool =
    	LazyList1.reduceLeft((a, b) -> a + b, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, ENil))))))) == Some(6)

    @test
    def reduceLeft17(): Bool =
    	LazyList1.reduceLeft((a, b) -> a + b, ECons(1, LCons(2, lazy ECons(3, ENil)))) == Some(6)

    @test
    def reduceLeft18(): Bool =
    	LazyList1.reduceLeft((a, b) -> a - b, LList(lazy ECons(1, LCons(2, lazy LCons(3, lazy ECons(4, ENil)))))) == Some(-8)

    @test
    def reduceLeft19(): Bool =
    	LazyList1.reduceLeft((a, b) -> a - b, LList(lazy ECons(1, LCons(2, lazy ECons(3, LList(lazy LCons(4, lazy ENil))))))) == Some(-8)

    @test
    def reduceLeft20(): Bool =
    	LazyList1.reduceLeft((a, b) -> a - b, ECons(1, LList(lazy LCons(2, lazy ECons(3, ECons(4, LList(lazy ENil))))))) == Some(-8)

    @test
    def reduceLeft21(): Bool =
        LazyList1.reduceLeft((a, b) -> b - a, ECons(1, ECons(2, ECons(3, ECons(4, ENil))))) == Some(2)


    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceRight01(): Bool =
        LazyList1.reduceRight((a, b) -> a + b, ENil: LazyList[Int]) == None

    @test
    def reduceRight02(): Bool =
        LazyList1.reduceRight((a, b) -> a + b, LList(lazy ENil): LazyList[Int]) == None

    @test
    def reduceRight03(): Bool =
    	LazyList1.reduceRight((a, b) -> a + b, LCons(1, lazy ENil)) == Some(1)

    @test
    def reduceRight04(): Bool =
    	LazyList1.reduceRight((a, b) -> a + b, LList(lazy ECons(1, LList(lazy ENil)))) == Some(1)

    @test
    def reduceRight05(): Bool =
    	LazyList1.reduceRight((a, b) -> a + b, ECons(1, LList(lazy ENil))) == Some(1)

    @test
    def reduceRight06(): Bool =
    	LazyList1.reduceRight((a, b) -> a + b, LCons(1, lazy LList(lazy ENil))) == Some(1)

    @test
    def reduceRight07(): Bool =
    	LazyList1.reduceRight((a, b) -> a + b, ECons(1, ENil)) == Some(1)

    @test
    def reduceRight08(): Bool =
    	LazyList1.reduceRight((a, b) -> a + b, LList(lazy LCons(1, lazy ENil))) == Some(1)

    @test
    def reduceRight09(): Bool =
    	LazyList1.reduceRight((a, b) -> a + b, LList(lazy LCons(1, lazy LList(lazy ENil)))) == Some(1)

    @test
    def reduceRight10(): Bool =
    	LazyList1.reduceRight((a, b) -> a + b, LList(lazy ECons(1, ENil))) == Some(1)

    @test
    def reduceRight11(): Bool =
    	LazyList1.reduceRight((a, b) -> a + b, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil))))) == Some(3)

    @test
    def reduceRight12(): Bool =
    	LazyList1.reduceRight((a, b) -> a + b, LList(lazy LCons(1, lazy ECons(2, LList(lazy ENil))))) == Some(3)

    @test
    def reduceRight13(): Bool =
    	LazyList1.reduceRight((a, b) -> a + b, ECons(1, LCons(2, lazy ENil))) == Some(3)

    @test
    def reduceRight14(): Bool =
    	LazyList1.reduceRight((a, b) -> a + b, LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))) == Some(6)

    @test
    def reduceRight15(): Bool =
    	LazyList1.reduceRight((a, b) -> a + b, LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))) == Some(6)

    @test
    def reduceRight16(): Bool =
    	LazyList1.reduceRight((a, b) -> a + b, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, ENil))))))) == Some(6)

    @test
    def reduceRight17(): Bool =
    	LazyList1.reduceRight((a, b) -> a + b, ECons(1, LCons(2, lazy ECons(3, ENil)))) == Some(6)

    @test
    def reduceRight18(): Bool =
    	LazyList1.reduceRight((a, b) -> a - b, LList(lazy ECons(1, LCons(2, lazy LCons(3, lazy ECons(4, ENil)))))) == Some(-2)

    @test
    def reduceRight19(): Bool =
    	LazyList1.reduceRight((a, b) -> a - b, LList(lazy ECons(1, LCons(2, lazy ECons(3, LList(lazy LCons(4, lazy ENil))))))) == Some(-2)

    @test
    def reduceRight20(): Bool =
    	LazyList1.reduceRight((a, b) -> a - b, ECons(1, LList(lazy LCons(2, lazy ECons(3, ECons(4, LList(lazy ENil))))))) == Some(-2)

    @test
    def reduceRight21(): Bool =
        LazyList1.reduceRight((a, b) -> b - a, ECons(1, ECons(2, ECons(3, ECons(4, ENil))))) == Some(-2)


    /////////////////////////////////////////////////////////////////////////////
    // minimum                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def minimum01(): Bool =
        LazyList1.minimum(ENil: LazyList[Int32]) == None

    @test
    def minimum02(): Bool =
        LazyList1.minimum(ECons(1, ENil)) == Some(1)

    @test
    def minimum03(): Bool =
        LazyList1.minimum(ECons(3, ECons(2, ECons(1, ENil)))) == Some(1)

    @test
    def minimum04(): Bool =
        LazyList1.minimum(ECons(0, ECons(3, ECons(2, ECons(1, ENil))))) == Some(0)

    @test
    def minimum05(): Bool =
        LazyList1.minimum(LazyList1.range(0, 99)) == Some(0)


    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def minimumBy01(): Bool =
        LazyList1.minimumBy((x, y) -> x <=> y, ENil: LazyList[Int32]) == None

    @test
    def minimumBy02(): Bool =
        LazyList1.minimumBy((x, y) -> x <=> y, ECons(1, ENil)) == Some(1)

    @test
    def minimumBy03(): Bool =
        LazyList1.minimumBy((x, y) -> x <=> y, ECons(3, ECons(2, ECons(1, ENil)))) == Some(1)

    @test
    def minimumBy04(): Bool =
        LazyList1.minimumBy((x, y) -> x <=> y, ECons(0, ECons(3, ECons(2, ECons(1, ENil))))) == Some(0)

    @test
    def minimumBy05(): Bool =
        LazyList1.minimumBy((_, _) -> LessThan, LazyList1.range(0, 99)) == Some(0)


    /////////////////////////////////////////////////////////////////////////////
    // maximum                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def maximum01(): Bool =
        LazyList1.maximum(ENil: LazyList[Int32]) == None

    @test
    def maximum02(): Bool =
        LazyList1.maximum(ECons(1, ENil)) == Some(1)

    @test
    def maximum03(): Bool =
        LazyList1.maximum(ECons(3, ECons(2, ECons(1, ENil)))) == Some(3)

    @test
    def maximum04(): Bool =
        LazyList1.maximum(ECons(0, ECons(3, ECons(2, ECons(1, ENil))))) == Some(3)

    @test
    def maximum05(): Bool =
        LazyList1.maximum(LazyList1.range(0, 99)) == Some(98)


    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def maximumBy01(): Bool =
        LazyList1.maximumBy((x, y) -> x <=> y, ENil: LazyList[Int32]) == None

    @test
    def maximumBy02(): Bool =
        LazyList1.maximumBy((x, y) -> x <=> y, ECons(1, ENil)) == Some(1)

    @test
    def maximumBy03(): Bool =
        LazyList1.maximumBy((x, y) -> x <=> y, ECons(3, ECons(2, ECons(1, ENil)))) == Some(3)

    @test
    def maximumBy04(): Bool =
        LazyList1.maximumBy((x, y) -> x <=> y, ECons(0, ECons(3, ECons(2, ECons(1, ENil))))) == Some(3)

    @test
    def maximumBy05(): Bool =
        LazyList1.maximumBy((_, _) -> LessThan, LazyList1.range(0, 99)) == Some(98)

    
    /////////////////////////////////////////////////////////////////////////////
    // flatMap (pure)                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatMapPure01(): Bool =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i), ENil: LazyList[Int32]) == ENil

    @test
    def flatMapPure02(): Bool =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i), LList(lazy ENil): LazyList[Int32]) == ENil

    @test
    def flatMapPure03(): Bool =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i), LCons(1, lazy ENil)) == ECons(1, ENil)

    @test
    def flatMapPure04(): Bool =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i), LList(lazy ECons(1, LList(lazy ENil)))) == ECons(1, ENil)

    @test
    def flatMapPure05(): Bool =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i), ECons(1, LList(lazy ENil))) == ECons(1, ENil)

    @test
    def flatMapPure06(): Bool =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i), LCons(1, lazy LList(lazy ENil))) == ECons(1, ENil)

    @test
    def flatMapPure07(): Bool =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i), ECons(1, ENil)) == ECons(1, ENil)

    @test
    def flatMapPure08(): Bool =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i), ECons(1, ECons(2, LCons(3, lazy ENil)))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

    @test
    def flatMapPure09(): Bool =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i), LCons(1, lazy LCons(2, lazy ECons(3, ENil)))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

    @test
    def flatMapPure10(): Bool =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i), LCons(1, lazy ECons(2, LCons(3, lazy ENil)))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

    @test
    def flatMapPure11(): Bool =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i), LList(lazy ECons(1, ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

    @test
    def flatMapPure12(): Bool =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i), LList(lazy ECons(1, LCons(2, lazy ECons(3, ENil))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

    @test
    def flatMapPure13(): Bool =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i), LCons(1, lazy LCons(2, lazy ECons(3, LList(lazy ENil))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

    @test
    def flatMapPure14(): Bool =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i), LList(lazy ECons(1, LList(lazy ECons(2, LCons(3, lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

    @test
    def flatMapPure15(): Bool =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i), ECons(1, ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

    @test
    def flatMapPure16(): Bool =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i), ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

    @test
    def flatMapPure17(): Bool =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i), LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil)))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

    @test
    def flatMapPure18(): Bool =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i), ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

    @test
    def flatMapPure19(): Bool =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i), ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))


    /////////////////////////////////////////////////////////////////////////////
    // flatMap (impure)                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatMapImpure01(): Bool & Impure =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i) as & Impure, ENil: LazyList[Int32]) == ENil

    @test
    def flatMapImpure02(): Bool & Impure =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i) as & Impure, LList(lazy ENil): LazyList[Int32]) == ENil

    @test
    def flatMapImpure03(): Bool & Impure =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i) as & Impure, LCons(1, lazy ENil)) == ECons(1, ENil)

    @test
    def flatMapImpure04(): Bool & Impure =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i) as & Impure, LList(lazy ECons(1, LList(lazy ENil)))) == ECons(1, ENil)

    @test
    def flatMapImpure05(): Bool & Impure =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i) as & Impure, ECons(1, LList(lazy ENil))) == ECons(1, ENil)

    @test
    def flatMapImpure06(): Bool & Impure =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i) as & Impure, LCons(1, lazy LList(lazy ENil))) == ECons(1, ENil)

    @test
    def flatMapImpure07(): Bool & Impure =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i) as & Impure, ECons(1, ENil)) == ECons(1, ENil)

    @test
    def flatMapImpure08(): Bool & Impure =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i) as & Impure, ECons(1, ECons(2, LCons(3, lazy ENil)))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

    @test
    def flatMapImpure09(): Bool & Impure =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i) as & Impure, LCons(1, lazy LCons(2, lazy ECons(3, ENil)))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

    @test
    def flatMapImpure10(): Bool & Impure =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i) as & Impure, LCons(1, lazy ECons(2, LCons(3, lazy ENil)))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

    @test
    def flatMapImpure11(): Bool & Impure =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i) as & Impure, LList(lazy ECons(1, ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

    @test
    def flatMapImpure12(): Bool & Impure =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i) as & Impure, LList(lazy ECons(1, LCons(2, lazy ECons(3, ENil))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

    @test
    def flatMapImpure13(): Bool & Impure =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i) as & Impure, LCons(1, lazy LCons(2, lazy ECons(3, LList(lazy ENil))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

    @test
    def flatMapImpure14(): Bool & Impure =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i) as & Impure, LList(lazy ECons(1, LList(lazy ECons(2, LCons(3, lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

    @test
    def flatMapImpure15(): Bool & Impure =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i) as & Impure, ECons(1, ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

    @test
    def flatMapImpure16(): Bool & Impure =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i) as & Impure, ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy LList(lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

    @test
    def flatMapImpure17(): Bool & Impure =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i) as & Impure, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil)))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

    @test
    def flatMapImpure18(): Bool & Impure =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i) as & Impure, ECons(1, LList(lazy LCons(2, lazy LCons(3, lazy LList(lazy ENil)))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

    @test
    def flatMapImpure19(): Bool & Impure =
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i) as & Impure, ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))


    /////////////////////////////////////////////////////////////////////////////
    // flatMap flatMap                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatMapFlatMap01(): Bool =
        ECons(1, ECons(2, ECons(3, ENil)))                             |>
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i)) |>
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i)) ==
            ECons(1,
                ECons(2, ECons(2, ECons(2, ECons(2,
                    ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ENil))))))))))))))

    @test
    def flatMapFlatMap02(): Bool & Impure =
        ECons(1, ECons(2, ECons(3, ENil)))                                         |>
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i) as & Impure) |>
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i))             ==
            ECons(1,
                ECons(2, ECons(2, ECons(2, ECons(2,
                    ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ENil))))))))))))))

    @test
    def flatMapFlatMap03(): Bool & Impure =
        ECons(1, ECons(2, ECons(3, ENil)))                                         |>
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i))             |>
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i) as & Impure) ==
            ECons(1,
                ECons(2, ECons(2, ECons(2, ECons(2,
                    ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ENil))))))))))))))

    @test
    def flatMapFlatMap04(): Bool & Impure =
        ECons(1, ECons(2, ECons(3, ENil)))                                         |>
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i) as & Impure) |>
        LazyList1.flatMap(i -> LazyList1.repeat(i) |> LazyList1.take(i) as & Impure) ==
            ECons(1,
                ECons(2, ECons(2, ECons(2, ECons(2,
                    ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ENil))))))))))))))


    /////////////////////////////////////////////////////////////////////////////
    // flatMap flatMap fusion                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatMapFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |>
        LazyList1.flatMap(i -> { l := "a" :: deref l; ECons(i, ENil) }) |>
        LazyList1.flatMap(i -> { l := "b" :: deref l; ECons(i, ENil) });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def flatMapFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |>
        LazyList1.flatMap(i -> { l := "a" :: deref l; ECons(i, ENil) } as & Pure) |>
        LazyList1.flatMap(i -> { l := "b" :: deref l; ECons(i, ENil) } as & Pure) |>
        LazyList1.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def memberOf01(): Bool =
        not (LazyList1.memberOf(0, ENil))

    @test
    def memberOf02(): Bool =
        not (LazyList1.memberOf(0, LList(lazy ENil)))

    @test
    def memberOf03(): Bool =
        not (LazyList1.memberOf(0, LCons(1, lazy ENil)))

    @test
    def memberOf04(): Bool =
        not (LazyList1.memberOf(0, ECons(1, LList(lazy ENil))))

    @test
    def memberOf05(): Bool =
        not (LazyList1.memberOf(0, LCons(1, lazy LList(lazy ENil))))

    @test
    def memberOf06(): Bool =
        not (LazyList1.memberOf(0, ECons(1, ENil)))

    @test
    def memberOf07(): Bool =
        not (LazyList1.memberOf(0, ECons(1, ECons(2, LList(lazy ECons(3, ENil))))))

    @test
    def memberOf08(): Bool =
        not (LazyList1.memberOf(0, LCons(1, lazy ECons(2, LList(lazy ECons(3, ENil))))))

    @test
    def memberOf09(): Bool =
        LazyList1.memberOf(0, ECons(1, ECons(2, LList(lazy ECons(3, LCons(0, lazy ENil))))))

    @test
    def memberOf10(): Bool =
        LazyList1.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LCons(0, lazy ENil)))))))

    @test
    def memberOf11(): Bool =
        LazyList1.memberOf(0, ECons(1, ECons(2, ECons(3, LCons(0, lazy ENil)))))

    @test
    def memberOf12(): Bool =
        LazyList1.memberOf(0, ECons(0, ECons(1, ECons(2, ECons(3, ENil)))))

    @test
    def memberOf13(): Bool =
        LazyList1.memberOf(0, ECons(1, ECons(1, ECons(1, ECons(0, ENil)))))

    @test
    def memberOf14(): Bool =
        LazyList1.memberOf(1, ECons(1, ECons(1, ECons(1, ECons(1, ENil)))))

    @test
    def memberOf15(): Bool =
        LazyList1.memberOf(0, ECons(1, ECons(1, ECons(0, ECons(1, ENil)))))

    @test
    def memberOf16(): Bool =
        LazyList1.memberOf(-2, ECons(1, ECons(1000, ECons(-2, ECons(1, ENil)))))


    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////
        
    @test
    def replace01(): Bool = 
        LazyList1.replace(3, 4, ENil) == ENil

    @test
    def replace02(): Bool = 
        LazyList1.replace(3, 4, LList(lazy ENil)) == ENil
        
    @test
    def replace03(): Bool = 
        LazyList1.replace(3, 4, LCons(1, lazy ENil)) == ECons(1, ENil)

    @test
    def replace04(): Bool = 
        LazyList1.replace(3, 4, LList(lazy ECons(1, LList(lazy ENil)))) == ECons(1, ENil)

    @test
    def replace05(): Bool = 
        LazyList1.replace(3, 4, LCons(1, lazy LList(lazy ENil))) == ECons(1, ENil)

    @test
    def replace06(): Bool = 
        LazyList1.replace(3, 4, ECons(1, ENil)) == ECons(1, ENil)

    @test
    def replace07(): Bool = 
        LazyList1.replace(3, 4, LCons(3, lazy ENil)) == ECons(4, ENil)

    @test
    def replace08(): Bool = 
        LazyList1.replace(3, 4, LList(lazy LCons(3, lazy ENil))) == ECons(4, ENil)

    @test
    def replace09(): Bool = 
        LazyList1.replace(3, 4, ECons(3, LList(lazy ENil))) == ECons(4, ENil)

    @test
    def replace10(): Bool = 
        LazyList1.replace(3, 4, ECons(3, ENil)) == ECons(4, ENil)

    @test
    def replace11(): Bool = 
        LazyList1.replace(3, 4, LCons(4, lazy ECons(4, ECons(4, ENil)))) == ECons(4, ECons(4, ECons(4, ENil)))

    @test
    def replace12(): Bool = 
        LazyList1.replace(3, 4, LList(lazy ECons(2, LList(lazy LCons(2, lazy LList(lazy LCons(2, lazy LList(lazy ENil)))))))) == ECons(2, ECons(2, ECons(2, ENil)))

    @test
    def replace13(): Bool = 
        LazyList1.replace(3, 4, ECons(2, LCons(2, lazy LList(lazy ECons(2, ENil))))) == ECons(2, ECons(2, ECons(2, ENil)))

    @test
    def replace14(): Bool = 
        LazyList1.replace(3, 4, ECons(2, ECons(3, LList(lazy ECons(2, ENil))))) == ECons(2, ECons(4, ECons(2, ENil)))

    @test
    def replace15(): Bool = 
        LazyList1.replace(3, 4, ECons(2, LList(lazy LCons(3, lazy ECons(2, ENil))))) == ECons(2, ECons(4, ECons(2, ENil)))

    @test
    def replace16(): Bool = 
        LazyList1.replace(3, 4, ECons(2, ECons(3, LList(lazy ECons(2, LList(lazy ENil)))))) == ECons(2, ECons(4, ECons(2, ENil)))


    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findLeft01(): Bool = 
        LazyList1.findLeft(i -> i > 2, ENil: LazyList[Int32]) == None

    @test
    def findLeft02(): Bool = 
        LazyList1.findLeft(i -> i > 2, LList(lazy ENil): LazyList[Int32]) == None

    @test
    def findLeft03(): Bool = 
        LazyList1.findLeft(i -> i > 2, LCons(1, lazy ENil)) == None

    @test
    def findLeft04(): Bool = 
        LazyList1.findLeft(i -> i > 2, ECons(1, ENil)) == None

    @test
    def findLeft05(): Bool = 
        LazyList1.findLeft(i -> i > 2, LCons(3, lazy ENil)) == Some(3)

    @test
    def findLeft06(): Bool = 
        LazyList1.findLeft(i -> i > 2, LList(lazy LCons(3, lazy ENil))) == Some(3)

    @test
    def findLeft07(): Bool = 
        LazyList1.findLeft(i -> i > 2, LList(lazy LCons(3, lazy LList(lazy ENil)))) == Some(3)

    @test
    def findLeft08(): Bool = 
        LazyList1.findLeft(i -> i > 2, ECons(3, LList(lazy ENil))) == Some(3)

    @test
    def findLeft09(): Bool = 
        LazyList1.findLeft(i -> i > 2, ECons(3, ENil)) == Some(3)

    @test
    def findLeft10(): Bool = 
        LazyList1.findLeft(i -> i > 2, LList(lazy LCons(1, lazy LCons(2, lazy ENil)))) == None

    @test
    def findLeft11(): Bool = 
        LazyList1.findLeft(i -> i > 2, ECons(1, LCons(2, lazy ENil))) == None

    @test
    def findLeft12(): Bool = 
        LazyList1.findLeft(i -> i > 2, LCons(1, lazy ECons(2, LList(lazy ENil)))) == None

    @test
    def findLeft13(): Bool = 
        LazyList1.findLeft(i -> i > 2, ECons(6, ECons(-6, LList(lazy ENil)))) == Some(6)

    @test
    def findLeft14(): Bool = 
        LazyList1.findLeft(i -> i > 2, ECons(6, LList(lazy ECons(-6, ENil)))) == Some(6)

    @test
    def findLeft15(): Bool = 
        LazyList1.findLeft(i -> i > 2, LCons(6, lazy ECons(-6, ENil))) == Some(6)

    @test
    def findLeft16(): Bool = 
        LazyList1.findLeft(i -> i > 2, ECons(-6, ECons(6, LList(lazy ENil)))) == Some(6)

    @test
    def findLeft17(): Bool = 
        LazyList1.findLeft(i -> i > 2, LCons(6, lazy ECons(7, ENil))) == Some(6)

    @test
    def findLeft18(): Bool = 
        LazyList1.findLeft(i -> i > 2, ECons(7, LCons(6, lazy ENil))) == Some(7)


    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findRight01(): Bool = 
        LazyList1.findRight(i -> i > 2, ENil: LazyList[Int32]) == None

    @test
    def findRight02(): Bool = 
        LazyList1.findRight(i -> i > 2, LList(lazy ENil): LazyList[Int32]) == None

    @test
    def findRight03(): Bool = 
        LazyList1.findRight(i -> i > 2, LCons(1, lazy ENil)) == None

    @test
    def findRight04(): Bool = 
        LazyList1.findRight(i -> i > 2, ECons(1, ENil)) == None

    @test
    def findRight05(): Bool = 
        LazyList1.findRight(i -> i > 2, LCons(3, lazy ENil)) == Some(3)

    @test
    def findRight06(): Bool = 
        LazyList1.findRight(i -> i > 2, LList(lazy LCons(3, lazy ENil))) == Some(3)

    @test
    def findRight07(): Bool = 
        LazyList1.findRight(i -> i > 2, LList(lazy LCons(3, lazy LList(lazy ENil)))) == Some(3)

    @test
    def findRight08(): Bool = 
        LazyList1.findRight(i -> i > 2, ECons(3, LList(lazy ENil))) == Some(3)

    @test
    def findRight09(): Bool = 
        LazyList1.findRight(i -> i > 2, ECons(3, ENil)) == Some(3)

    @test
    def findRight10(): Bool = 
        LazyList1.findRight(i -> i > 2, LList(lazy LCons(1, lazy LCons(2, lazy ENil)))) == None

    @test
    def findRight11(): Bool = 
        LazyList1.findRight(i -> i > 2, ECons(1, LCons(2, lazy ENil))) == None

    @test
    def findRight12(): Bool = 
        LazyList1.findRight(i -> i > 2, LCons(1, lazy ECons(2, LList(lazy ENil)))) == None

    @test
    def findRight13(): Bool = 
        LazyList1.findRight(i -> i > 2, ECons(6, ECons(-6, LList(lazy ENil)))) == Some(6)

    @test
    def findRight14(): Bool = 
        LazyList1.findRight(i -> i > 2, ECons(6, LList(lazy ECons(-6, ENil)))) == Some(6)

    @test
    def findRight15(): Bool = 
        LazyList1.findRight(i -> i > 2, LCons(6, lazy ECons(-6, ENil))) == Some(6)

    @test
    def findRight16(): Bool = 
        LazyList1.findRight(i -> i > 2, ECons(-6, ECons(6, LList(lazy ENil)))) == Some(6)

    @test
    def findRight17(): Bool = 
        LazyList1.findRight(i -> i > 2, LCons(6, lazy ECons(7, ENil))) == Some(7)

    @test
    def findRight18(): Bool = 
        LazyList1.findRight(i -> i > 2, ECons(7, LCons(6, lazy ENil))) == Some(6)


    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex (pure)                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithIndexPure01(): Bool =
    	ENil |> LazyList1.mapWithIndex((x, i) -> x + i) == ENil

    @test
    def mapWithIndexPure02(): Bool =
    	LList(lazy ENil) |> LazyList1.mapWithIndex((x, i) -> x + i) == ENil

    @test
    def mapWithIndexPure03(): Bool =
    	LCons(1, lazy ENil) |> LazyList1.mapWithIndex((x, i) -> x + i) == ECons(1, ENil)

    @test
    def mapWithIndexPure04(): Bool =
    	ECons(1, ENil) |> LazyList1.mapWithIndex((x, i) -> x + i) == ECons(1, ENil)

    @test
    def mapWithIndexPure05(): Bool =
    	LList(lazy LCons(1, lazy LCons(2, lazy ENil))) |> LazyList1.mapWithIndex((x, i) -> x + i) == ECons(1, ECons(3, ENil))

    @test
    def mapWithIndexPure06(): Bool =
    	ECons(1, LCons(2, lazy ENil)) |> LazyList1.mapWithIndex((x, i) -> x + i) == ECons(1, ECons(3, ENil))

    @test
    def mapWithIndexPure07(): Bool =
    	ECons(1, LList(lazy LCons(2, lazy ENil))) |> LazyList1.mapWithIndex((x, i) -> x + i) == ECons(1, ECons(3, ENil))

    @test
    def mapWithIndexPure08(): Bool =
    	LList(lazy ECons(1, ECons(2, LList(lazy ENil)))) |> LazyList1.mapWithIndex((x, i) -> x + i) == ECons(1, ECons(3, ENil))

    @test
    def mapWithIndexPure09(): Bool =
    	LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil)))) |> LazyList1.mapWithIndex((x, i) -> x + i) == ECons(1, ECons(3, ECons(5, ENil)))

    @test
    def mapWithIndexPure10(): Bool =
    	LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil)))) |> LazyList1.mapWithIndex((x, i) -> x + i) == ECons(1, ECons(3, ECons(5, ENil)))

    @test
    def mapWithIndexPure11(): Bool =
    	LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil))))) |> LazyList1.mapWithIndex((x, i) -> x + i) == ECons(1, ECons(3, ECons(5, ENil)))

    @test
    def mapWithIndexPure12(): Bool =
    	LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil)))))) |> LazyList1.mapWithIndex((x, i) -> x + i) == ECons(1, ECons(3, ECons(5, ENil)))

    @test
    def mapWithIndexPure13(): Bool =
    	LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, ENil)))))) |> LazyList1.mapWithIndex((x, i) -> x + i) == ECons(1, ECons(3, ECons(5, ENil)))

    @test
    def mapWithIndexPure14(): Bool =
    	ECons(1, LCons(2, lazy ECons(3, ENil))) |> LazyList1.mapWithIndex((x, i) -> x + i) == ECons(1, ECons(3, ECons(5, ENil)))

    @test
    def mapWithIndexPure15(): Bool =
    	LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, ENil)))))) |> LazyList1.mapWithIndex((x, i) -> x + i) == ECons(1, ECons(3, ECons(5, ENil)))

    @test
    def mapWithIndexPure16(): Bool =
    	LList(lazy ECons(1, LCons(2, lazy LCons(3, lazy ECons(4, ENil))))) |> LazyList1.mapWithIndex((x, i) -> x + i) == ECons(1, ECons(3, ECons(5, ECons(7, ENil))))

    @test
    def mapWithIndexPure17(): Bool =
    	LList(lazy ECons(1, LCons(2, lazy ECons(3, LList(lazy LCons(4, lazy ENil)))))) |> LazyList1.mapWithIndex((x, i) -> x + i) == ECons(1, ECons(3, ECons(5, ECons(7, ENil))))

    @test
    def mapWithIndexPure18(): Bool =
    	LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil))))))))) |> LazyList1.mapWithIndex((x, i) -> x + i) == ECons(1, ECons(3, ECons(5, ECons(7, ENil))))

    @test
    def mapWithIndexPure19(): Bool =
    	ECons(1, LList(lazy LCons(2, lazy ECons(3, ECons(4, LList(lazy ENil)))))) |> LazyList1.mapWithIndex((x, i) -> x + i) == ECons(1, ECons(3, ECons(5, ECons(7, ENil))))

    @test
    def mapWithIndexPure20(): Bool =
        ECons(1, ECons(2, ECons(3, ECons(4, ENil)))) |> LazyList1.mapWithIndex((x, i) -> x + i) == ECons(1, ECons(3, ECons(5, ECons(7, ENil))))


    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex (impure)                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithIndexImpure01(): Bool & Impure =
        ENil |> LazyList1.mapWithIndex((x, i) -> x + i as & Impure) == ENil

    @test
    def mapWithIndexImpure02(): Bool & Impure =
        LList(lazy ENil) |> LazyList1.mapWithIndex((x, i) -> x + i as & Impure) == ENil

    @test
    def mapWithIndexImpure03(): Bool & Impure =
        LCons(1, lazy ENil) |> LazyList1.mapWithIndex((x, i) -> x + i as & Impure) == ECons(1, ENil)

    @test
    def mapWithIndexImpure04(): Bool & Impure =
        ECons(1, ENil) |> LazyList1.mapWithIndex((x, i) -> x + i as & Impure) == ECons(1, ENil)

    @test
    def mapWithIndexImpure05(): Bool & Impure =
        LList(lazy LCons(1, lazy LCons(2, lazy ENil))) |> LazyList1.mapWithIndex((x, i) -> x + i as & Impure) == ECons(1, ECons(3, ENil))

    @test
    def mapWithIndexImpure06(): Bool & Impure =
        ECons(1, LCons(2, lazy ENil)) |> LazyList1.mapWithIndex((x, i) -> x + i as & Impure) == ECons(1, ECons(3, ENil))

    @test
    def mapWithIndexImpure07(): Bool & Impure =
        ECons(1, LList(lazy LCons(2, lazy ENil))) |> LazyList1.mapWithIndex((x, i) -> x + i as & Impure) == ECons(1, ECons(3, ENil))

    @test
    def mapWithIndexImpure08(): Bool & Impure =
        LList(lazy ECons(1, ECons(2, LList(lazy ENil)))) |> LazyList1.mapWithIndex((x, i) -> x + i as & Impure) == ECons(1, ECons(3, ENil))

    @test
    def mapWithIndexImpure09(): Bool & Impure =
        LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil)))) |> LazyList1.mapWithIndex((x, i) -> x + i as & Impure) == ECons(1, ECons(3, ECons(5, ENil)))

    @test
    def mapWithIndexImpure10(): Bool & Impure =
        LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil)))) |> LazyList1.mapWithIndex((x, i) -> x + i as & Impure) == ECons(1, ECons(3, ECons(5, ENil)))

    @test
    def mapWithIndexImpure11(): Bool & Impure =
        LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil))))) |> LazyList1.mapWithIndex((x, i) -> x + i as & Impure) == ECons(1, ECons(3, ECons(5, ENil)))

    @test
    def mapWithIndexImpure12(): Bool & Impure =
        LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil)))))) |> LazyList1.mapWithIndex((x, i) -> x + i as & Impure) == ECons(1, ECons(3, ECons(5, ENil)))

    @test
    def mapWithIndexImpure13(): Bool & Impure =
        LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, ENil)))))) |> LazyList1.mapWithIndex((x, i) -> x + i as & Impure) == ECons(1, ECons(3, ECons(5, ENil)))

    @test
    def mapWithIndexImpure14(): Bool & Impure =
        ECons(1, LCons(2, lazy ECons(3, ENil))) |> LazyList1.mapWithIndex((x, i) -> x + i as & Impure) == ECons(1, ECons(3, ECons(5, ENil)))

    @test
    def mapWithIndexImpure15(): Bool & Impure =
        LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, ENil)))))) |> LazyList1.mapWithIndex((x, i) -> x + i as & Impure) == ECons(1, ECons(3, ECons(5, ENil)))

    @test
    def mapWithIndexImpure16(): Bool & Impure =
        LList(lazy ECons(1, LCons(2, lazy LCons(3, lazy ECons(4, ENil))))) |> LazyList1.mapWithIndex((x, i) -> x + i as & Impure) == ECons(1, ECons(3, ECons(5, ECons(7, ENil))))

    @test
    def mapWithIndexImpure17(): Bool & Impure =
        LList(lazy ECons(1, LCons(2, lazy ECons(3, LList(lazy LCons(4, lazy ENil)))))) |> LazyList1.mapWithIndex((x, i) -> x + i as & Impure) == ECons(1, ECons(3, ECons(5, ECons(7, ENil))))

    @test
    def mapWithIndexImpure18(): Bool & Impure =
        LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil))))))))) |> LazyList1.mapWithIndex((x, i) -> x + i as & Impure) == ECons(1, ECons(3, ECons(5, ECons(7, ENil))))

    @test
    def mapWithIndexImpure19(): Bool & Impure =
        ECons(1, LList(lazy LCons(2, lazy ECons(3, ECons(4, LList(lazy ENil)))))) |> LazyList1.mapWithIndex((x, i) -> x + i as & Impure) == ECons(1, ECons(3, ECons(5, ECons(7, ENil))))

    @test
    def mapWithIndexImpure20(): Bool & Impure =
        ECons(1, ECons(2, ECons(3, ECons(4, ENil)))) |> LazyList1.mapWithIndex((x, i) -> x + i as & Impure) == ECons(1, ECons(3, ECons(5, ECons(7, ENil))))


    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex mapWithIndex                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithIndexMapWithIndex01(): Bool =
        ECons(1, ECons(2, ECons(3, ENil))) |> LazyList1.mapWithIndex((x, i) -> x + i) |> LazyList1.mapWithIndex((x, i) -> x * i) == ECons(0, ECons(3, ECons(10, ENil)))

    @test
    def mapWithIndexMapWithIndex02(): Bool & Impure =
        ECons(1, ECons(2, ECons(3, ENil))) |> LazyList1.mapWithIndex((x, i) -> x + i as & Impure) |> LazyList1.mapWithIndex((x, i) -> x * i) == ECons(0, ECons(3, ECons(10, ENil)))

    @test
    def mapWithIndexMapWithIndex03(): Bool & Impure =
        ECons(1, ECons(2, ECons(3, ENil))) |> LazyList1.mapWithIndex((x, i) -> x + i) |> LazyList1.mapWithIndex((x, i) -> x * i as & Impure) == ECons(0, ECons(3, ECons(10, ENil)))

    @test
    def mapWithIndexMapWithIndex04(): Bool & Impure =
        ECons(1, ECons(2, ECons(3, ENil))) |> LazyList1.mapWithIndex((x, i) -> x + i as & Impure) |> LazyList1.mapWithIndex((x, i) -> x * i as & Impure) == ECons(0, ECons(3, ECons(10, ENil)))


    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex mapWithIndex fusion                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithIndexFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |>
        LazyList1.mapWithIndex((x, _) -> { l := "a" :: deref l; x }) |>
        LazyList1.mapWithIndex((x, _) -> { l := "b" :: deref l; x }) |>
        LazyList.toList;
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def mapWithIndexFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |>
        LazyList1.mapWithIndex((x, _) -> { l := "a" :: deref l; x } as & Pure) |>
        LazyList1.mapWithIndex((x, _) -> { l := "b" :: deref l; x } as & Pure) |>
        LazyList.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)

    
    /////////////////////////////////////////////////////////////////////////////
    // intercalate                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def intercalate01(): Bool =
        LazyList1.intercalate(ECons(1, ECons(2, ENil)), ENil) == ENil

    @test
    def intercalate02(): Bool =
        LazyList1.intercalate(ECons(1, ECons(2, ENil)), LList(lazy ENil)) == ENil

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
        LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil)))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

    @test
    def intercalate07(): Bool = 
        LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil)))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

    @test
    def intercalate08(): Bool = 
        LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

    @test
    def intercalate09(): Bool = 
        LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil)))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))

    @test
    def intercalate10(): Bool = 
        LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), LCons(LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, ENil)))))), lazy ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ENil)))))))))
    
    @test
    def intercalate11(): Bool =
        LazyList1.intercalate(ECons(10, ECons(11, ECons(12, ENil))), ECons(ECons(1, ECons(2, ECons(3, ENil))), ECons(ECons(100, ECons(101, ECons(102, ENil))), ECons(ECons(200, ECons(201, ECons(202, ENil))), ENil)))) == ECons(1, ECons(2, ECons(3, ECons(10, ECons(11, ECons(12, ECons(100, ECons(101, ECons(102, ECons(10, ECons(11, ECons(12, ECons(200, ECons(201, ECons(202, ENil)))))))))))))))

    @test
    def intercalate12(): Bool =
        LazyList1.intercalate(ECons(12, ECons(11, ECons(10, ENil))), ECons(ECons(1, ECons(2, ECons(3, ENil))), ECons(ECons(100, ECons(101, ECons(102, ENil))), ECons(ECons(200, ECons(201, ECons(202, ENil))), ENil)))) == ECons(1, ECons(2, ECons(3, ECons(12, ECons(11, ECons(10, ECons(100, ECons(101, ECons(102, ECons(12, ECons(11, ECons(10, ECons(200, ECons(201, ECons(202, ENil)))))))))))))))

    @test
    def intercalate13(): Bool =
        LazyList1.intercalate(ECons(-1, ECons(-2, ECons(0, ENil))), ECons(ECons(1, ECons(2, ECons(3, ENil))), ECons(ECons(100, ECons(101, ECons(102, ENil))), ECons(ECons(200, ECons(201, ECons(202, ENil))), ENil)))) == ECons(1, ECons(2, ECons(3, ECons(-1, ECons(-2, ECons(0, ECons(100, ECons(101, ECons(102, ECons(-1, ECons(-2, ECons(0, ECons(200, ECons(201, ECons(202, ENil)))))))))))))))


    /////////////////////////////////////////////////////////////////////////////
    // intersperse                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def intersperse01(): Bool = 
        LazyList1.intersperse(-11, ENil) == ENil

    @test
    def intersperse02(): Bool = 
        LazyList1.intersperse(-11, LList(lazy ENil)) == ENil

    @test
    def intersperse03(): Bool = 
        LazyList1.intersperse(-11, ECons(1, ENil)) == ECons(1, ENil)

    @test
    def intersperse04(): Bool = 
        LazyList1.intersperse(-11, LList(lazy ECons(1, LList(lazy ENil)))) == ECons(1, ENil)

    @test
    def intersperse05(): Bool = 
        LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

    @test
    def intersperse06(): Bool = 
        LazyList1.intersperse(-11, LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

    @test
    def intersperse07(): Bool = 
        LazyList1.intersperse(-11, LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

    @test
    def intersperse08(): Bool = 
        LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

    @test
    def intersperse09(): Bool = 
        LazyList1.intersperse(-11, LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

    @test
    def intersperse10(): Bool = 
        LazyList1.intersperse(-11, ECons(1, LCons(2, lazy ECons(3, ENil)))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

    @test
    def intersperse11(): Bool = 
        LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy LCons(3, lazy ECons(4, ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

    @test
    def intersperse12(): Bool = 
        LazyList1.intersperse(-11, LList(lazy ECons(1, LCons(2, lazy ECons(3, LList(lazy LCons(4, lazy ENil))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

    @test
    def intersperse13(): Bool = 
        LazyList1.intersperse(-11, LCons(1, lazy LCons(2, lazy LCons(3, lazy ECons(4, LList(lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

    @test
    def intersperse14(): Bool = 
        LazyList1.intersperse(-11, ECons(1, LList(lazy ECons(2, LList(lazy ECons(3, LList(lazy LCons(4, lazy ENil)))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

    @test
    def intersperse15(): Bool = 
        LazyList1.intersperse(-11, LList(lazy ECons(1, ECons(2, ECons(3, LCons(4, lazy ENil)))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))


    /////////////////////////////////////////////////////////////////////////////
    // takeWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

   
    @test
    def takeWhilePure08(): Bool =
        LazyList1.takeWhile(i -> i > 3, ECons(4, ECons(6, ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil))))))))))) == ECons(4, ECons(6, ENil))


/*
@test
def takeWhilePure01(): Bool =
    LazyList.takeWhile(i -> i > 3, ENil: LazyList[Int32]) == ENil

@test
def takeWhilePure02(): Bool =
    LazyList.takeWhile(i -> i > 3, 1 :: ENil) == ENil

@test
def takeWhilePure03(): Bool =
    LazyList.takeWhile(i -> i > 3, 4 :: ENil) == 4 :: ENil

@test
def takeWhilePure04(): Bool =
    LazyList.takeWhile(i -> i > 3, 1 :: 2 :: ENil) == ENil

@test
def takeWhilePure05(): Bool =
    LazyList.takeWhile(i -> i > 3, 1 :: 5 :: ENil) == ENil

@test
def takeWhilePure06(): Bool =
    LazyList.takeWhile(i -> i > 3, 5 :: 1 :: ENil) == 5 :: ENil

@test
def takeWhilePure07(): Bool =
    LazyList.takeWhile(i -> i > 3, 5 :: 8 :: ENil) == 5 :: 8 :: ENil

@test
def takeWhilePure08(): Bool =
    LazyList.takeWhile(i -> i > 3, ECons(4, ECons(6, ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil))))))))))) == ECons(4, ECons(6, ENil))
*/
}