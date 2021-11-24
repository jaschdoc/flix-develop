namespace TestRedBlackTree {

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map01(): Bool =
        let Map(t) = List.zip(List.range(0, 5000), List.range(0, 5000)) |> List.toMap;
        let m = Map(RedBlackTree1.map(x -> x + 1, t));
        Map.toList(m) == List.zip(List.range(0, 5000), List.range(1, 5001))


    ////////////////////
    // Helper tests   //
    ////////////////////

    @test
    def map02(): Bool =
        let Map(t) = List.zip(List.range(0, 10), List.range(0, 10)) |> List.toMap;
        Map.toList(Map(t)) == List.zip(List.range(0, 10), List.range(0, 10))

    @test
    def map03(): Bool =
        let Map(t) = List.zip(List.range(0, 10), List.range(0, 10)) |> List.toMap;
        let m = Map(RedBlackTree1.map(x -> x + 1, t));
        Map.toList(m) == List.zip(List.range(0, 10), List.range(1, 11))

}
