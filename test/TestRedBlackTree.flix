namespace TestRedBlackTree {

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def count01(): Bool =
        let Map(t) = Map#{}: Map[Int32, Int32];
        RedBlackTree1.count((k, v) -> k == v, t) == 0

    @test
    def count02(): Bool =
        let Map(t) = Map#{1 -> 2};
        RedBlackTree1.count((k, v) -> k == v, t) == 0

    @test
    def count03(): Bool =
        let Map(t) = Map#{1 -> 1};
        RedBlackTree1.count((k, v) -> k == v, t) == 1

    @test
    def count04(): Bool =
        let Map(t) = Map#{1 -> 2, 2 -> 3};
        RedBlackTree1.count((k, v) -> k == v, t) == 0

    @test
    def count05(): Bool =
        let Map(t) = Map#{1 -> 1, 2 -> 3};
        RedBlackTree1.count((k, v) -> k == v, t) == 1

    @test
    def count06(): Bool =
        let Map(t) = List.zip(List.range(0, 2000), List.range(0, 2000)) |> List.toMap;
        RedBlackTree1.count((k, v) -> k == v, t) == 2000

    @test
    def count07(): Bool =
        let Map(t) = List.zip(List.range(0, 2000), List.range(-1000, 1000)) |> List.toMap;
        let res = RedBlackTree1.count((k, v) -> k - 1000 == v, t);
        res == 2000

    @test
    def count08(): Bool & Impure =
        let range = List.range(0, 2000);
        let Map(t) = List.zip(range, range) |> List.toMap;
        let a = [default; 2000];
        let i = ref 0;
        RedBlackTree1.count((_, v) -> { a[deref i] = v; i := 1 + deref i; true }, t);
        Array.sameElements(a, List.toArray(range))

    @test
    def count09(): Bool & Impure =
        let range = List.range(0, 2000);
        let Map(t) = List.zip(range, range) |> List.toMap;
        let a = MutList.new();
        RedBlackTree1.count((_, v) -> { let b = MutList.new(); MutList.push!(v, b); MutList.append!(a, b); true }, t);
        MutList.eq(a, List.toMutList(range))

}
