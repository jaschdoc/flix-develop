namespace TestArray {

    /////////////////////////////////////////////////////////////////////////////
    // toIterator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toIterator01(): Bool & Impure =
        []: Array[Int32] |> Array1.toIterator |> Iterator1.toList == Nil

    @test
    def toIterator02(): Bool & Impure =
        [1, 2, 3] |> Array1.toIterator |> Iterator1.toList == 1 :: 2 :: 3 :: Nil

    @test
    def toIterator03(): Bool & Impure =
        Array.sameElements([1, 2, 3] |> Array1.toIterator |> Iterator1.toArray,[1, 2, 3])

    @test
    def toIterator04(): Bool & Impure =
        Array.sameElements(
            Array.range(-100, 100) |> Array1.toIterator |> Iterator1.toArray, Array.range(-100, 100))

    @test
    def toIterator05(): Bool & Impure = {
        let a = [1, 2, 3];
        let iter = Array1.toIterator(a);
        a[0] = 10000;
        Array.sameElements(Iterator1.toArray(iter), [1, 2, 3])
    }
}
