namespace TestOption {

    /////////////////////////////////////////////////////////////////////////////
    // toIterator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toIterator01(): Bool & Impure =
        None: Option[Int32] |> Option1.toIterator |> Iterator1.toList == Nil

    @test
    def toIterator02(): Bool & Impure =
        Some(1) |> Option1.toIterator |> Iterator1.toList == 1 :: Nil

    @test
    def toIterator03(): Bool & Impure =
        Some(1 :: 2 :: 3 :: Nil) |> Option1.toIterator |> Iterator1.toList == (1 :: 2 :: 3 :: Nil) :: Nil

}
