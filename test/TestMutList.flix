namespace TestMutList {

    /////////////////////////////////////////////////////////////////////////////
    // product                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def product01(): Bool & Impure =
        List.toMutList(Nil) |> MutList.product == 0

    @test
    def product02(): Bool & Impure =
        List.toMutList(1 :: Nil) |> MutList.product == 1

    @test
    def product03(): Bool & Impure =
        List.toMutList(1 :: 2 :: 3 :: Nil) |> MutList.product == 6

    @test
    def product04(): Bool & Impure =
        List.toMutList(1 :: 2 :: 3 :: -3 :: Nil) |> MutList.product == -18

    @test
    def product05(): Bool & Impure =
        List.toMutList(-1 :: -2 :: -3 :: -4 :: Nil) |> MutList.product == 24

    @test
    def product06(): Bool & Impure =
        List.toMutList(10 :: -10 :: Nil) |> MutList.product == -100


    /////////////////////////////////////////////////////////////////////////////
    // productWith                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productWith01(): Bool & Impure =
        List.toMutList(Nil) |> MutList.productWith(x -> x + 1) == 0

    @test
    def productWith02(): Bool & Impure =
        List.toMutList(1 :: Nil) |> MutList.productWith(x -> x + 1) == 2

    @test
    def productWith03(): Bool & Impure =
        List.toMutList(1 :: 2 :: 3 :: Nil) |> MutList.productWith(x -> x + 1) == 24

    @test
    def productWith04(): Bool & Impure =
        List.toMutList(1 :: 2 :: 3 :: -3 :: Nil) |> MutList.productWith(x -> x + 1) == -48

    @test
    def productWith05(): Bool & Impure =
        List.toMutList(-2 :: -3 :: -4 :: -5 :: Nil) |> MutList.productWith(x -> x + 1) == 24

    @test
    def productWith06(): Bool & Impure =
        List.toMutList(10 :: -10 :: Nil) |> MutList.productWith(x -> x + 1) == -99


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinWith01(): Bool & Impure =
        List.toMutList(Nil: List[Int32]) |> MutList.joinWith(x -> "${x + 1}", ",") == ""

    @test
    def joinWith02(): Bool & Impure =
        List.toMutList(1 :: Nil) |> MutList.joinWith(x -> "${x + 1}", ",") == "2"

    @test
    def joinWith03(): Bool & Impure =
        List.toMutList(1 :: 2 :: 3 :: Nil) |> MutList.joinWith(x -> "${x + 1}", ",") == "2,3,4"

    @test
    def joinWith04(): Bool & Impure =
        List.toMutList("1" :: "2" :: "3" :: Nil) |> MutList.joinWith(x -> x + x, ",") == "11,22,33"


}
