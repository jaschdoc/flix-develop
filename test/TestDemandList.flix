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

}