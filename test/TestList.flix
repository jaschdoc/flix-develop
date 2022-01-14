namespace TestList1 {

    /////////////////////////////////////////////////////////////////////////////
    // product                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def product01(): Bool =
        List1.product(Nil) == 0

    @test
    def product02(): Bool =
        List1.product(1 :: Nil) == 1

    @test
    def product03(): Bool =
        List1.product(1 :: 2 :: 3 :: Nil) == 6

    @test
    def product04(): Bool =
        List1.product(1 :: 2 :: 3 :: -3 :: Nil) == -18

    @test
    def product05(): Bool =
        List1.product(-1 :: -2 :: -3 :: -4 :: Nil) == 24

    @test
    def product06(): Bool =
        List1.product(10 :: -10 :: Nil) == -100


    /////////////////////////////////////////////////////////////////////////////
    // productWith                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productWith01(): Bool =
        List1.productWith(x -> x + 1, Nil) == 0

    @test
    def productWith02(): Bool =
        List1.productWith(x -> x + 1, 1 :: Nil) == 2

    @test
    def productWith03(): Bool =
        List1.productWith(x -> x + 1, 1 :: 2 :: 3 :: Nil) == 24

    @test
    def productWith04(): Bool =
        List1.productWith(x -> x + 1, 1 :: 2 :: 3 :: -3 :: Nil) == -48

    @test
    def productWith05(): Bool =
        List1.productWith(x -> x + 1, -2 :: -3 :: -4 :: -5 :: Nil) == 24

    @test
    def productWith06(): Bool =
        List1.productWith(x -> x + 1, 10 :: -10 :: Nil) == -99

}
