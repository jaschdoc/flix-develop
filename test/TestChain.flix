namespace TestChain {

    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinWith01(): Bool =
        (Nil: List[Int32]) |> List.toChain |>
            Chain.joinWith(x -> "${x + 1}", ",") == ""

    @test
    def joinWith02(): Bool =
        (1 :: Nil) |> List.toChain |>
            Chain.joinWith(x -> "${x + 1}", ",") == "2"

    @test
    def joinWith03(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toChain |>
            Chain.joinWith(x -> "${x + 1}", ",") == "2,3,4"

    @test
    def joinWith04(): Bool =
        ("1" :: "2" :: "3" :: Nil) |> List.toChain |>
            Chain.joinWith(x -> x + x, ",") == "11,22,33"

}
