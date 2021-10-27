namespace TestResult {

    /////////////////////////////////////////////////////////////////////////////
    // toIterator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toIterator01(): Bool & Impure =
        Err(1): Result[Int32, Int32] |> Result1.toIterator |> Iterator1.toList == Nil

    @test
    def toIterator02(): Bool & Impure =
        Ok(1) |> Result1.toIterator |> Iterator1.toList == 1 :: Nil

    @test
    def toIterator03(): Bool & Impure =
        Ok(1 :: 2 :: 3 :: Nil) |> Result1.toIterator |> Iterator1.toList == (1 :: 2 :: 3 :: Nil) :: Nil

}
