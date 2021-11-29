namespace TestRedBlackTree {

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map01(): Bool =
        let Map(t) = List.zip(List.range(0, 16), List.range(0, 16)) |> List.toMap;
        let m = Map(RedBlackTree1.map(x -> x + 1, t));
        Map.toList(m) == List.zip(List.range(0, 16), List.range(1, 17))

    @test
    def map02(): Bool =
        let Map(t) = Map#{2 -> -1, 5 -> 15, 11 -> -9, 8 -> 8};
        let m = Map(RedBlackTree1.map(x -> x + 1, t));
        m == Map#{2 -> 0, 5 -> 16, 11 -> -8, 8 -> 9}

    @test
    def map03(): Bool =
        let Map(t) = List.zip(List.range(0, 5000), List.range(0, 5000)) |> List.toMap;
        let m = Map(RedBlackTree1.map(x -> x + 1, t));
        Map.toList(m) == List.zip(List.range(0, 5000), List.range(1, 5001))

    @test
    def map04(): Bool =
        let Map(t) = List.zip(List.range(-10000, 10000), List.range(-10000, 10000)) |> List.toMap;
        let m = Map(RedBlackTree1.map(x -> x + 1, t));
        Map.toList(m) == List.zip(List.range(-10000, 10000), List.range(-9999, 10001))
}
