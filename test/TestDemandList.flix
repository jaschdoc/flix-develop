namespace TestDemandList {
/* turn into test cases

    pub def ex1(): Int32 =
        let l1 = range(1, 10);              // l1 must be eager
        let l2 = filter(x -> x > 5, l1);    // l2 must be eager
        sum(l2)

    pub def ex2(): Int32 & Impure =
        let l1 = range(1, 10);                          // l1 must be eager
        let l2 = filter(x -> x > 5 as & Impure, l1);    // l2 must be eager
        sum(l2)

    pub def ex3(): Option[Int32] =
        let l1 = range(1, 10);                          // l1 must be lazy (i.e. a free variable and not true).
        let l2 = filter(x -> x > 5, l1);                // l2 must be lazy (i.e. a free variable and not true).
        head(l2)

    pub def ex4(): Option[Int32] & Impure =
        let l1 = range(1, 10);                            // l1 must be eager
        let l2 = filter(x -> x > 5 as & Impure, l1);      // l2 must be eager
        head(l2)
*/

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isEmpty01(): Bool =
        DemandList1.empty() |> DemandList1.isEmpty

    @test
    def isEmpty02(): Bool =
        not (List1.toDemandList(1 :: Nil) |> DemandList1.isEmpty)


    /////////////////////////////////////////////////////////////////////////////
    // toDemandList & toList                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toDemandList01(): Bool =
        List1.toDemandList(Nil) |> DemandList1.toList == Nil

    @test
    def toDemandList02(): Bool =
        List1.toDemandList(1 :: Nil) |> DemandList1.toList == 1 :: Nil
    
    @test
    def toDemandList03(): Bool =
        List1.toDemandList(1 :: 2 :: 3 :: Nil) |> DemandList1.toList == 1 :: 2 :: 3 :: Nil

    @test
    def toDemandList04(): Bool =
        List.range(-1000, 1000) |>
            List1.toDemandList |>
            DemandList1.toList == List.range(-1000, 1000)


    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def range01(): Bool =
        DemandList1.range(0, 0) |> DemandList1.isEmpty

    @test
    def range02(): Bool =
        DemandList1.range(100, 100) |> DemandList1.isEmpty

    @test
    def range03(): Bool =
        DemandList1.range(1, 0) |> DemandList1.isEmpty

    @test
    def range04(): Bool =
        not (DemandList1.range(0, 100) |> DemandList1.isEmpty)

    @test
    def range05(): Bool =
        DemandList1.range(0, 100) |> DemandList1.toList == List.range(0, 100)

    @test
    def range06(): Bool =
        not (DemandList1.range(-100, 100) |> DemandList1.isEmpty)

    @test
    def range07(): Bool =
        DemandList1.range(-100, 100) |> DemandList1.toList == List.range(-100, 100)


    /////////////////////////////////////////////////////////////////////////////
    // map (pure)                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapPure01(): Bool =
        Nil |> List1.toDemandList |> DemandList1.map(x -> x + 1) |> DemandList1.toList == Nil

    @test
    def mapPure02(): Bool =
        (1 :: Nil) |> List1.toDemandList |> DemandList1.map(x -> x + 1) |> DemandList1.toList == 2 :: Nil

    @test
    def mapPure03(): Bool =
        (1 :: 2 :: Nil) |> List1.toDemandList |> DemandList1.map(x -> x + 1) |> DemandList1.toList == 2 :: 3 :: Nil

    @test
    def mapPure04(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List1.toDemandList |> DemandList1.map(x -> x + 1) |> DemandList1.toList == 2 :: 3 :: 4 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // map (impure)                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapImpure01(): Bool & Impure =
        Nil |> List1.toDemandList |> DemandList1.map(x -> x + 1 as & Impure) |> DemandList1.toList == Nil

    @test
    def mapImpure02(): Bool & Impure =
        (1 :: Nil) |> List1.toDemandList |> DemandList1.map(x -> x + 1 as & Impure) |> DemandList1.toList == 2 :: Nil

    @test
    def mapImpure03(): Bool & Impure =
        (1 :: 2 :: Nil) |> List1.toDemandList |> DemandList1.map(x -> x + 1 as & Impure) |> DemandList1.toList == 2 :: 3 :: Nil

    @test
    def mapImpure04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List1.toDemandList |> DemandList1.map(x -> x + 1 as & Impure) |> DemandList1.toList == 2 :: 3 :: 4 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // map map                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapMap01(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List1.toDemandList |> DemandList1.map(x -> x + 1) |> DemandList1.map(x -> x * 2) |> DemandList1.toList == 4 :: 6 :: 8 :: Nil

    @test
    def mapMap02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List1.toDemandList |> DemandList1.map(x -> x + 1 as & Impure) |> DemandList1.map(x -> x * 2) |> DemandList1.toList == 4 :: 6 :: 8 :: Nil

    @test
    def mapMap03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List1.toDemandList |> DemandList1.map(x -> x + 1) |> DemandList1.map(x -> x * 2 as & Impure) |> DemandList1.toList == 4 :: 6 :: 8 :: Nil

    @test
    def mapMap04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List1.toDemandList |> DemandList1.map(x -> x + 1 as & Impure) |> DemandList1.map(x -> x * 2 as & Impure) |> DemandList1.toList == 4 :: 6 :: 8 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // map map fusion                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List1.toDemandList |>
        DemandList1.map(x -> { l := "a" :: deref l; x }) |>
        DemandList1.map(x -> { l := "b" :: deref l; x });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def mapFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List1.toDemandList |>
        DemandList1.map(x -> { l := "a" :: deref l; x } as & Pure) |>
        DemandList1.map(x -> { l := "b" :: deref l; x } as & Pure) |>
        DemandList1.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // filter (pure)                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterPure01(): Bool =
        Nil |> List1.toDemandList |> DemandList1.filter(x -> x > 100) |> DemandList1.toList == Nil

    @test
    def filterPure02(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List1.toDemandList |> DemandList1.filter(x -> x > 0) |> DemandList1.toList == 1 :: 2 :: 3 :: Nil

    @test
    def filterPure03(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List1.toDemandList |> DemandList1.filter(x -> x > 1) |> DemandList1.toList == 2 :: 3 :: Nil

    @test
    def filterPure04(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List1.toDemandList |> DemandList1.filter(x -> x > 2) |> DemandList1.toList == 3 :: Nil

    @test
    def filterPure05(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List1.toDemandList |> DemandList1.filter(x -> x > 3) |> DemandList1.toList == Nil

    @test
    def filterPure06(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List1.toDemandList |> DemandList1.filter(x -> x > 100) |> DemandList1.toList == Nil


    /////////////////////////////////////////////////////////////////////////////
    // filter (impure)                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterImpure01(): Bool & Impure =
        Nil |> List1.toDemandList |> DemandList1.filter(x -> x > 100 as & Impure) |> DemandList1.toList == Nil

    @test
    def filterImpure02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List1.toDemandList |> DemandList1.filter(x -> x > 0 as & Impure) |> DemandList1.toList == 1 :: 2 :: 3 :: Nil

    @test
    def filterImpure03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List1.toDemandList |> DemandList1.filter(x -> x > 1 as & Impure) |> DemandList1.toList == 2 :: 3 :: Nil

    @test
    def filterImpure04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List1.toDemandList |> DemandList1.filter(x -> x > 2 as & Impure) |> DemandList1.toList == 3 :: Nil

    @test
    def filterImpure05(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List1.toDemandList |> DemandList1.filter(x -> x > 3 as & Impure) |> DemandList1.toList == Nil

    @test
    def filterImpure06(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List1.toDemandList |> DemandList1.filter(x -> x > 100 as & Impure) |> DemandList1.toList == Nil


    /////////////////////////////////////////////////////////////////////////////
    // filter filter                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterFilter01(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List1.toDemandList |> DemandList1.filter(x -> x > 1) |> DemandList1.filter(x -> x < 3) |> DemandList1.toList == 2 :: Nil

    @test
    def filterFilter02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List1.toDemandList |> DemandList1.filter(x -> x > 1 as & Impure) |> DemandList1.filter(x -> x < 3) |> DemandList1.toList == 2 :: Nil

    @test
    def filterFilter03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List1.toDemandList |> DemandList1.filter(x -> x > 1) |> DemandList1.filter(x -> x < 3 as & Impure) |> DemandList1.toList == 2 :: Nil

    @test
    def filterFilter04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List1.toDemandList |> DemandList1.filter(x -> x > 1 as & Impure) |> DemandList1.filter(x -> x < 3 as & Impure) |> DemandList1.toList == 2 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // filter filter fusion                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List1.toDemandList |>
        DemandList1.filter(_ -> { l := "a" :: deref l; true }) |>
        DemandList1.filter(_ -> { l := "b" :: deref l; true });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def filterFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List1.toDemandList |>
        DemandList1.filter(_x -> { l := "a" :: deref l; true } as & Pure) |>
        DemandList1.filter(_x -> { l := "b" :: deref l; true } as & Pure) |>
        DemandList1.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // map filter fusion                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapFilterFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List1.toDemandList |>
        DemandList1.map(   x -> { l := "a" :: deref l; x }) |>
        DemandList1.filter(_ -> { l := "b" :: deref l; true });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def mapFilterFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List1.toDemandList |>
        DemandList1.map(   x -> { l := "a" :: deref l; x }    as & Pure) |>
        DemandList1.filter(_ -> { l := "b" :: deref l; true } as & Pure) |>
        DemandList1.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // filter map fusion                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterMapFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List1.toDemandList |>
        DemandList1.filter(_ -> { l := "a" :: deref l; true }) |>
        DemandList1.map(   x -> { l := "b" :: deref l; x    });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def filterMapFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List1.toDemandList |>
        DemandList1.filter(_ -> { l := "a" :: deref l; true } as & Pure) |>
        DemandList1.map(   x -> { l := "b" :: deref l; x    } as & Pure) |>
        DemandList1.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)

    
    /////////////////////////////////////////////////////////////////////////////    
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool =
    	DemandList1.foldLeft((i, e) -> (i - e) * (e % 2 + 1), 100, DemandList1.empty()) == 100

    @test
    def foldLeft02(): Bool =
    	(1 :: Nil) |> List1.toDemandList |> DemandList1.foldLeft((i, e) -> (i - e) * (e % 2 + 1), 100) == 198

    @test
    def foldLeft03(): Bool =
    	(1 :: 2 :: Nil) |> List1.toDemandList |> DemandList1.foldLeft((i, e) -> (i - e) * (e % 2 + 1), 100) == 196

    @test
    def foldLeft04(): Bool =
    	(1 :: 2 :: 3 :: Nil) |> List1.toDemandList |> DemandList1.foldLeft((i, e) -> (i - e) * (e % 2 + 1), 100) == 386

    @test
    def foldLeft05(): Bool =
    	(1 :: 2 :: 3 :: Nil) |> List1.toDemandList |> DemandList1.foldLeft((acc, x) -> x :: acc, Nil) == 3 :: 2 :: 1 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool =
    	DemandList1.foldRight((e, i) -> (i - e) * (e % 2 + 1), 100, DemandList1.empty()) == 100

    @test
    def foldRight02(): Bool =
    	(1 :: Nil) |> List1.toDemandList |> DemandList1.foldRight((e, i) -> (i - e) * (e % 2 + 1), 100) == 198

    @test
    def foldRight03(): Bool =
    	(1 :: 2 :: Nil) |> List1.toDemandList |> DemandList1.foldRight((e, i) -> (i - e) * (e % 2 + 1), 100) == 194

    @test
    def foldRight04(): Bool =
    	(1 :: 2 :: 3 :: Nil) |> List1.toDemandList |> DemandList1.foldRight((e, i) -> (i - e) * (e % 2 + 1), 100) == 382

    @test
    def foldRight05(): Bool =
    	(1 :: 2 :: 3 :: Nil) |> List1.toDemandList |> DemandList1.foldRight((x, acc) -> x :: acc, Nil) == 1 :: 2 :: 3 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // take                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def take01(): Bool =
        (DemandList1.empty()) |> DemandList1.take(1000) |> DemandList1.isEmpty

    @test
    def take02(): Bool =
        (DemandList1.empty()) |> DemandList1.take(0) |> DemandList1.isEmpty

    @test
    def take03(): Bool =
        (DemandList1.range(0, 1000)) |> DemandList1.take(0) |> DemandList1.isEmpty

    @test
    def take04(): Bool =
        (DemandList1.range(0, 1000))
            |> DemandList1.take(500)
            |> DemandList1.toList == List.range(0, 500)

    @test
    def take05(): Bool =
        (DemandList1.range(0, 1000))
            |> DemandList1.take(1)
            |> DemandList1.toList == List.range(0, 1)

        
    /////////////////////////////////////////////////////////////////////////////
    // drop                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    
    @test
    def drop01(): Bool =
        Nil |> List1.toDemandList |> DemandList1.drop(-1) |> DemandList1.toList == Nil

    @test
    def drop02(): Bool =
        Nil |> List1.toDemandList |> DemandList1.drop(0) |> DemandList1.toList == Nil

    @test
    def drop03(): Bool =
        Nil |> List1.toDemandList |> DemandList1.drop(1) |> DemandList1.toList == Nil

    @test
    def drop04(): Bool =
        (1 :: Nil) |> List1.toDemandList |> DemandList1.drop(-1) |>  DemandList1.toList == 1 :: Nil

    @test
    def drop05(): Bool =
        (1 :: Nil) |> List1.toDemandList |> DemandList1.drop(0) |>  DemandList1.toList == 1 :: Nil

    @test
    def drop06(): Bool =
        (1 :: Nil) |> List1.toDemandList |> DemandList1.drop(1) |>  DemandList1.toList == Nil

    @test
    def drop07(): Bool =
        (1 :: Nil) |> List1.toDemandList |> DemandList1.drop(2) |>  DemandList1.toList == Nil

    @test
    def drop08(): Bool =
        (1 :: 2 :: Nil) |> List1.toDemandList |> DemandList1.drop(0) |>  DemandList1.toList == 1 :: 2 :: Nil

    @test
    def drop09(): Bool =
        (1 :: 2 :: Nil) |> List1.toDemandList |> DemandList1.drop(1) |>  DemandList1.toList == 2 :: Nil

    @test
    def drop10(): Bool =
        (1 :: 2 :: Nil) |> List1.toDemandList |> DemandList1.drop(2) |>  DemandList1.toList == Nil

    @test
    def drop11(): Bool =
        (1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil) |> List1.toDemandList |> DemandList1.drop(2) |>  DemandList1.toList == 3 :: 4 :: 5 :: 6 :: Nil

    @test
    def drop12(): Bool =
        (1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil) |> List1.toDemandList |> DemandList1.drop(4) |>  DemandList1.toList == 5 :: 6 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def append01(): Bool =
        DemandList1.append(List1.toDemandList(Nil: List[Unit]), List1.toDemandList(Nil)) |> DemandList1.toList == Nil

    @test
    def append02(): Bool =
        DemandList1.append(List1.toDemandList(Nil), List1.toDemandList(1 :: Nil)) |> DemandList1.toList == 1 :: Nil

    @test
    def append03(): Bool =
        DemandList1.append(List1.toDemandList(Nil), List1.toDemandList(1 :: 2 :: Nil)) |> DemandList1.toList == 1 :: 2 :: Nil

    @test
    def append04(): Bool =
        DemandList1.append(List1.toDemandList(1 :: Nil), List1.toDemandList(Nil)) |> DemandList1.toList == 1 :: Nil

    @test
    def append05(): Bool =
        DemandList1.append(List1.toDemandList(1 :: 2 :: Nil), List1.toDemandList(Nil)) |> DemandList1.toList == 1 :: 2 :: Nil

    @test
    def append06(): Bool =
        DemandList1.append(List1.toDemandList(1 :: Nil), List1.toDemandList(2 :: Nil)) |> DemandList1.toList == 1 :: 2 :: Nil

    @test
    def append07(): Bool =
        DemandList1.append(List1.toDemandList(1 :: 2 :: Nil), List1.toDemandList(3 :: Nil)) |> DemandList1.toList == 1 :: 2 :: 3 :: Nil

    @test
    def append08(): Bool =
        DemandList1.append(List1.toDemandList(1 :: Nil), List1.toDemandList(2 :: 3 :: Nil)) |> DemandList1.toList == 1 :: 2 :: 3 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // flatMap (pure)                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatMapPure01(): Bool =
        DemandList1.flatMap(i -> DemandList1.range(i, i + 100), DemandList1.empty()) |> DemandList1.toList == Nil

    @test
    def flatMapPure02(): Bool =
        List1.toDemandList(0 :: Nil) |> DemandList1.flatMap(i -> DemandList1.range(i, i + 100)) |> DemandList1.toList == List.range(0, 100)

    @test
    def flatMapPure03(): Bool =
        List1.toDemandList(1 :: Nil) |> DemandList1.flatMap(i -> DemandList1.range(i, i + 2)) |> DemandList1.toList == 1 :: 2 :: Nil

    @test
    def flatMapPure04(): Bool =
        List1.toDemandList(2 :: Nil) |> DemandList1.flatMap(i -> DemandList1.range(i, i + 2)) |> DemandList1.toList == 2 :: 3 :: Nil

    @test
    def flatMapPure05(): Bool =
        List1.toDemandList(1 :: 2 :: Nil) |> DemandList1.flatMap(i -> DemandList1.range(i, i + 2)) |> DemandList1.toList == 1 :: 2 :: 2 :: 3 :: Nil

    @test
    def flatMapPure06(): Bool =
        List1.toDemandList(2 :: 3 :: Nil) |> DemandList1.flatMap(i -> DemandList1.range(i, i + 2)) |> DemandList1.toList == 2 :: 3 :: 3 :: 4 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // flatMap (impure)                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatMapImpure01(): Bool & Impure =
        DemandList1.flatMap(i -> DemandList1.range(i, i + 100) as & Impure, DemandList1.empty()) |> DemandList1.toList == Nil

    @test
    def flatMapImpure02(): Bool & Impure =
        List1.toDemandList(0 :: Nil) |> DemandList1.flatMap(i -> DemandList1.range(i, i + 100) as & Impure) |> DemandList1.toList == List.range(0, 100)

    @test
    def flatMapImpure03(): Bool & Impure =
        List1.toDemandList(1 :: Nil) |> DemandList1.flatMap(i -> DemandList1.range(i, i + 2) as & Impure) |> DemandList1.toList == 1 :: 2 :: Nil

    @test
    def flatMapImpure04(): Bool & Impure =
        List1.toDemandList(2 :: Nil) |> DemandList1.flatMap(i -> DemandList1.range(i, i + 2) as & Impure) |> DemandList1.toList == 2 :: 3 :: Nil

    @test
    def flatMapImpure05(): Bool & Impure =
        List1.toDemandList(1 :: 2 :: Nil) |> DemandList1.flatMap(i -> DemandList1.range(i, i + 2) as & Impure) |> DemandList1.toList == 1 :: 2 :: 2 :: 3 :: Nil

    @test
    def flatMapImpure06(): Bool & Impure =
        List1.toDemandList(2 :: 3 :: Nil) |> DemandList1.flatMap(i -> DemandList1.range(i, i + 2) as & Impure) |> DemandList1.toList == 2 :: 3 :: 3 :: 4 :: Nil
}