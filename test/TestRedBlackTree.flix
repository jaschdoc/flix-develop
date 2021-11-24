namespace TestRedBlackTree {

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map01(): Bool =
        let Map(t) = List.zip(List.range(0, 1000000), List.range(0, 1000000)) |> List.toMap;
        let m = Map(RedBlackTree1.map(x -> x + 1, t));
        Map.toList(m) == List.zip(List.range(0, 1000000), List.range(1, 1000001))

}
