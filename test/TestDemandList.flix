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


}