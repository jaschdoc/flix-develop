namespace TestNel {

    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinWith01(): Bool =
        Nel.joinWith(x -> "${x + 1}", ",", Nel(1, Nil)) == "2"

    @test
    def joinWith02(): Bool =
        Nel.joinWith(x -> "${x + 1}", ",", Nel(1, 2 :: 3 :: Nil)) == "2,3,4"

    @test
    def joinWith03(): Bool =
        Nel.joinWith(x -> x + x, ",", Nel("1", "2" :: "3" :: Nil)) == "11,22,33"

}
