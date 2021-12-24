namespace TestRedBlackTree {

    /////////////////////////////////////////////////////////////////////////////
    // mapWithKey                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithKey01(): Bool =
        let Map(t) = Map#{}: Map[Int32, Int32];
        Map(RedBlackTree.mapWithKey((k, v) -> k + v, t)) == Map#{}

    @test
    def mapWithKey02(): Bool =
        let Map(t) = Map#{1 => 4};
        Map(RedBlackTree.mapWithKey((k, v) -> k + v, t)) == Map#{1 => 5}

    @test
    def mapWithKey03(): Bool =
        let Map(t) = Map#{2 => -1, 0 => 0};
        Map(RedBlackTree.mapWithKey((k, v) -> k + v, t)) == Map#{2 => 1, 0 => 0}

    @test
    def mapWithKey04(): Bool =
        let Map(t) = Map#{2 => -1, 5 => 15, 11 => -9};
        Map(RedBlackTree.mapWithKey((k, v) -> k + v, t)) == Map#{2 => 1, 5 => 20, 11 => 2}

    @test
    def mapWithKey05(): Bool =
        let Map(t) = Map#{2 => -1, 5 => 15, 11 => -9, 8 => 8};
        Map(RedBlackTree.mapWithKey((k, v) -> k + v, t)) == Map#{2 => 1, 5 => 20, 11 => 2, 8 => 16}

    @test
    def mapWithKey06(): Bool =
        let Map(t) = List.zip(List.range(0, 2000), List.range(0, 2000)) |> List.toMap;
        let expected = List.zip(List.range(0, 2000), List.range(1, 2001)) |> List.toMap;
        Map(RedBlackTree.mapWithKey((_, v) -> v + 1, t)) == expected

    @test
    def mapWithKey07(): Bool =
        let Map(t) = List.zip(List.range(0, 2000), List.range(-1000, 1000)) |> List.toMap;
        let expected = List.zip(List.range(0, 2000), List.range(-999, 1001)) |> List.toMap;
        Map(RedBlackTree.mapWithKey((_, v) -> v + 1, t)) == expected

    @test
    def mapWithKey08(): Bool & Impure =
        let range = List.range(0, 2000);
        let Map(t) = List.zip(range, range) |> List.toMap;
        let a = Array.new(-1, 2000);
        let i = ref 0;
        RedBlackTree.mapWithKey((_, v) -> { a[deref i] = v; i := 1 + deref i; v }, t);
        Array.sameElements(a, List.toArray(range))

    @test
    def mapWithKey09(): Bool & Impure =
        let range = List.range(0, 2000);
        let Map(t) = List.zip(range, range) |> List.toMap;
        let a = MutList.new();
        RedBlackTree.mapWithKey((_, v) -> { let b = MutList.new(); MutList.push!(v, b); MutList.append!(a, b); v }, t);
        MutList.eq(a, List.toMutList(range))


    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def count01(): Bool =
        let Map(t) = Map#{}: Map[Int32, Int32];
        RedBlackTree1.count((k, v) -> k == v, t) == 0

    @test
    def count02(): Bool =
        let Map(t) = Map#{1 => 2};
        RedBlackTree1.count((k, v) -> k == v, t) == 0

    @test
    def count03(): Bool =
        let Map(t) = Map#{1 => 1};
        RedBlackTree1.count((k, v) -> k == v, t) == 1

    @test
    def count04(): Bool =
        let Map(t) = Map#{1 => 2, 2 => 3};
        RedBlackTree1.count((k, v) -> k == v, t) == 0

    @test
    def count05(): Bool =
        let Map(t) = Map#{1 => 1, 2 => 3};
        RedBlackTree1.count((k, v) -> k == v, t) == 1

    @test
    def count06(): Bool =
        let Map(t) = List.zip(List.range(0, 2000), List.range(0, 2000)) |> List.toMap;
        RedBlackTree1.count((k, v) -> k == v, t) == 2000

    @test
    def count07(): Bool =
        let Map(t) = List.zip(List.range(0, 2000), List.range(-1000, 1000)) |> List.toMap;
        RedBlackTree1.count((k, v) -> k - 1000 == v, t) == 2000

    @test
    def count08(): Bool & Impure =
        let range = List.range(0, 2000);
        let Map(t) = List.zip(range, range) |> List.toMap;
        let a = Array.new(-1, 2000);
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


    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def minimumBy01(): Bool =
        let Map(t) = Map#{}: Map[Int32, Int32];
        RedBlackTree1.minimumBy((x, _, y, _) -> x <=> y, t) == None

    @test
    def minimumBy02(): Bool =
        let Map(t) = Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5};
        RedBlackTree1.minimumBy((x, _, y, _) -> x <=> y, t) == Some((1, 2))

    @test
    def minimumBy03(): Bool =
        let Map(t) = Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5, 0 => 2};
        RedBlackTree1.minimumBy((x, _, y, _) -> x <=> y, t) == Some((0, 2))

    @test
    def minimumBy04(): Bool =
        let Map(t) = Map#{1 => 2, 2 => 3, -44 => 4, 5 => 5, 0 => 2};
        RedBlackTree1.minimumBy((x, _, y, _) -> x <=> y, t) == Some((-44, 4))

    @test
    def minimumBy05(): Bool & Impure =
        let range = List.range(0, 2000);
        let Map(t) = List.zip(range, range) |> List.toMap;
        let a = Array.new(-1, 2000);
        let i = ref 0;
        RedBlackTree1.minimumBy((_, x, _, y) -> { a[deref i] = x; i := 1 + deref i; x <=> y }, t);
        Array.sameElements(a, List.toArray(range))

    @test
    def minimumBy06(): Bool & Impure =
        let range = List.range(0, 2000);
        let Map(t) = List.zip(range, range) |> List.toMap;
        let a = MutList.new();
        RedBlackTree1.minimumBy((_, x, _, y) -> { let b = MutList.new(); MutList.push!(x, b); MutList.append!(a, b); x <=> y }, t);
        MutList.toList(a) |> println;
        MutList.eq(a, List.toMutList(range))

    @test
    def minimumBy07(): Bool =
        let range = List.range(0, 2000);
        let Map(t) = List.zip(range, List.reverse(range)) |> List.toMap;
        RedBlackTree1.minimumBy((_, x, _, y) -> x <=> y, t) == Some(1999, 0)
}
