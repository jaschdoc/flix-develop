namespace TestSet {

    /////////////////////////////////////////////////////////////////////////////
    // toIterator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toIterator01(): Bool & Impure =
        Set.empty(): Set[Int32] |> Set1.toIterator |> Iterator.toSet == Set.empty()

    @test
    def toIterator02(): Bool & Impure =
        Set#{1, 2, 3, 4} |> Set1.toIterator |> Iterator.toSet == Set#{1, 2, 3, 4}

    @test
    def toIterator03(): Bool & Impure =
        Set.range(0, 100) |> Set1.toIterator |> Iterator.toSet == Set.range(0, 100)

}
