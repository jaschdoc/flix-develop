mod Graph.FordFulkerson {

    use Path.Path;

    enum Path {
        case Path(List[Int32])
    }

    instance Eq[Path] {
        pub def eq(x: Path, y: Path): Bool =
            let Path(xs) = x;
            let Path(ys) = y;
            List.length(xs) == List.length(ys)
    }

    instance Order[Path] {
        pub def compare(x: Path, y: Path): Comparison =
            let Path(xs) = x;
            let Path(ys) = y;
            if (List.length(xs) ==  List.length(ys))
                Comparison.EqualTo
            else if (List.length(xs) <= List.length(ys))
                Comparison.GreaterThan
            else
                Comparison.LessThan
    }

    instance LowerBound[Path] {
        // The longest list
        pub def minValue(): Path = Path(List.range(1, 100))
    }

    instance PartialOrder[Path] {
        pub def lessEqual(x: Path, y: Path): Bool =
            let Path(xs) = x;
            let Path(ys) = y;
            List.length(xs) >= List.length(ys)
    }

    instance JoinLattice[Path] {
        pub def leastUpperBound(x: Path, y: Path): Path =
            let Path(xs) = x;
            let Path(ys) = y;
            if (List.length(xs) <= List.length(ys)) x else y
    }

    instance MeetLattice[Path] {
        pub def greatestLowerBound(x: Path, y: Path): Path = ???
    }


    pub def init(y: Int32, x: Int32): Path =
        Path(y :: x :: Nil)

    pub def cons(z: Int32, p: Path): Path = match p {
        case Path(xs) => Path(z :: xs)
    }

    type alias From = Int32
    type alias To = Int32
    type alias UpperCapacity = Int32
    type alias Flow = Int32
    type alias FlowNetwork = (From, To, UpperCapacity, Flow)

    pub def maxFlow(src: { src = From }, dst: { dst = To }, g: m[(From, To, UpperCapacity)]): Flow with Foldable[m] =
        def fordFulkerson(flowNetwork) = match augmentingPath(src, dst, flowNetwork) {
            case None       => getMaxFlow(dst.dst, flowNetwork)
            case Some(path) =>
                let incr = pathMinCapacity(path, flowNetwork);
                let updatedNetwork = increaseFlow(path, incr, flowNetwork);
                fordFulkerson(updatedNetwork)
        };
        // Init with 0 flow
        let flowNetwork = Foldable.toVector(g) |> Vector.map(match (x, y, z) -> (x, y, z, 0));
        fordFulkerson(flowNetwork)

    def getMaxFlow(dst: To, g: m[FlowNetwork]): Flow with Foldable[m] =
        let edges = inject g into Edge;
        let rules = #{
            Flow(f) :- Edge(_, dst, _, f).
        };
        let result = query edges, rules select f from Flow(f);
        Vector.sum(result)

    def augmentingPath(src: { src = From }, dst: { dst = To }, g: m[FlowNetwork]): Option[Path] with Foldable[m] =
        let edges = inject g into Edge;
        let rules = #{
            Reach(x, y; init(y, x)) :- Edge(x, y, u, f),                 if (u - f) > 0. // Forward edge
            Reach(x, z; cons(z, p)) :- Reach(x, y; p), Edge(y, z, u, f), if (u - f) > 0. // Forward edge
            Reach(x, y; init(y, x)) :- Edge(y, x, u, f),                 if f > 0.       // Back edge
            Reach(x, z; cons(z, p)) :- Reach(x, y; p), Edge(z, y, u, f), if f > 0.       // Back edge
        };
        let s = src.src;
        let t = dst.dst;
        let result = query edges, rules select fn from Reach(s, t; fn);
        Vector.head(result) |> Option.flatMap(match Path(xs) -> if (List.isEmpty(xs)) None else Some(Path(xs)))

    def pathMinCapacity(p: Path, g: m[FlowNetwork]): Int32 with Foldable[m] =
        let mem = p |> flip(memberOf);
        let optMin = g |> Foldable.filter(match (s, d, _, _) -> mem(s) and mem(d))
            |> List.map(match (_, _, u, f) -> u - f)
            |> List.minimum;
        match optMin {
            case Some(u) => u
            case None    => unreachable!() // Handle this better somehow
        }

    def increaseFlow(p: Path, incr: Int32, g: m[FlowNetwork]): Vector[FlowNetwork] with Foldable[m] =
        g
        |> Foldable.toVector
        |> Vector.map(match (s, d, u, f) ->
            if (isForwardEdge(src = s, dst = d, p))
                (s, d, u, f + incr)
            else if (isBackEdge(src = s, dst = d, p))
                (s, d, u, f - incr)
            else
                (s, d, u, f)
        )

    def isForwardEdge(src: { src = From }, dst: { dst = To }, p: Path): Bool =
        memberOf(src.src, p) and memberOf(dst.dst, p) and indexOf(src.src, p) > indexOf(dst.dst, p) // A path is sorted in reverse order

    def isBackEdge(src: { src = From }, dst: { dst = To }, p: Path): Bool =
        memberOf(src.src, p) and memberOf(dst.dst, p) and indexOf(dst.dst, p) > indexOf(src.src, p) // A path is sorted in reverse order

    def memberOf(a: Int32, p: Path): Bool =
        let Path(xs) = p;
        List.memberOf(a, xs)

    def indexOf(a: Int32, p: Path): Option[Int32] =
        let Path(xs) = p;
        List.indexOf(a, xs)

    //////////////////////////////////////////
    // Tests                                //
    //////////////////////////////////////////

    ///
    /// Returns the following graph:
    ///
    /// ```
    ///      1---2
    ///     /|\  |\
    ///    0 | \ | 5
    ///     \|  \|/
    ///      3---4
    /// ```
    ///
    /// The edges are directed as follows (ordered from left to right, top to bottom):
    /// ```
    /// 0 -> 1, capacity 10
    /// 0 -> 3, capacity 10
    /// 1 -> 3, capacity 2
    /// 1 -> 2, capacity 4
    /// 1 -> 4, capacity 8
    /// 3 -> 4, capacity 9
    /// 4 -> 2, capacity 6
    /// 2 -> 5, capacity 10
    /// 4 -> 5, capacity 10
    /// ```
    ///
    /// The maximum flow is `19`.
    ///
    def g1(): Set[(From, To, UpperCapacity)] =
        Set#{ (0, 1, 10), (0, 3, 10), (1, 3, 2), (1, 2, 4), (1, 4, 8), (3, 4, 9), (4, 2, 6), (2, 5, 10), (4, 5, 10) }

    @Test
    def testAugmentingPath01(): Bool =
        let s = 1;
        let t = 5;
        let g = Set#{ (1, 2, 1, 0), (2, 3, 1, 0), (3, 4, 1, 0), (3, 8, 1, 0), (4, 5, 1, 0), (3, 5, 1, 0) };
        let Path(xs) = augmentingPath(src = s, dst = t, g) |> Option.getWithDefault(Path(Nil));
        Assert.eq(5 :: 3 :: 2 :: 1 :: Nil, xs)

    @Test
    def testAugmentingPath02(): Bool =
        let s = 0;
        let t = 5;
        let g = g1() |> Set.map(match (a, b, u) -> (a, b, u, 0));
        let Path(xs) = augmentingPath(src = s, dst = t, g) |> Option.getWithDefault(Path(Nil));
        Assert.eq(5 :: 4 :: 3 :: 0 :: Nil, xs) // Shortest path in reverse order

    @Test
    def testAugmentingPath03(): Bool =
        let s = 0;
        let t = 5;
        let g = g1() |> Set.map(x -> match x {
            case (0, 3, 10) => (0, 3, 10, 9)
            case (3, 4, 9)  => (3, 4, 9 , 9)
            case (4, 5, 10) => (4, 5, 10, 9)
            case (a, b, u)  => (a, b, u , 0)
        });
        let Path(xs) = augmentingPath(src = s, dst = t, g) |> Option.getWithDefault(Path(Nil));
        Assert.eq(5 :: 4 :: 1 :: 0 :: Nil, xs)

    @Test
    def testPathMinCapacity01(): Bool =
        let p = Path(5 :: 4 :: 3 :: 2 :: 1 :: Nil);
        let g = Set#{(1, 2, 3, 0), (1, 3, 4, 0), (2, 3, 1, 0), (3, 4, 5, 0), (4, 5, 10, 0)};
        Assert.eq(1, pathMinCapacity(p, g))

    @Test
    def testPathMinCapacity02(): Bool =
        let p = Path(5 :: 4 :: 3 :: 2 :: 1 :: Nil);
        let g = Set#{(1, 2, 3, 0), (1, 3, 4, 0), (2, 3, 3, 0), (3, 4, 5, 0), (4, 5, 10, 0), (6, 7, 1, 0)};
        Assert.eq(3, pathMinCapacity(p, g))

    @Test
    def testPathMinCapacity03(): Bool =
        let s = 0;
        let t = 5;
        let g = g1() |> Set.map(match (a, b, u) -> (a, b, u, 0));
        let p = augmentingPath(src = s, dst = t, g) |> Option.getWithDefault(Path(Nil));
        Assert.eq(9, pathMinCapacity(p, g)) // The path is picked as in testAugmentingPath02

    @Test
    def testPathMinCapacity04(): Bool =
        let s = 0;
        let t = 5;
        let g = g1() |> Set.map(x -> match x {
            case (0, 3, 10) => (0, 3, 10, 9)
            case (3, 4, 9)  => (3, 4, 9 , 9)
            case (4, 5, 10) => (4, 5, 10, 9)
            case (a, b, u)  => (a, b, u , 0)
        });
        let p = augmentingPath(src = s, dst = t, g) |> Option.getWithDefault(Path(Nil));
        Assert.eq(1, pathMinCapacity(p, g)) // The path is picked as in testAugmentingPath03

    @Test
    def testIncreaseFlow01(): Bool =
        let s = 0;
        let t = 5;
        let g = g1() |> Set.map(match (a, b, u) -> (a, b, u, 0));
        let p = augmentingPath(src = s, dst = t, g) |> Option.getWithDefault(Path(Nil));
        let incr = pathMinCapacity(p, g); // is 9, see testPathMinCapacity03
        let g2 = g |> Set.map(x -> match x {
            case (0, 3, 10, 0) => (0, 3, 10, 9)
            case (3, 4, 9 , 0) => (4, 5, 9 , 9)
            case (4, 5, 10, 0) => (4, 5, 10, 9)
            case _             => x
        }) |> Foldable.toVector;
        Assert.eq(g2, increaseFlow(p, incr, g))

    @Test @Skip
    def testMaxFlow01(): Bool =
        Assert.eq(19, maxFlow(src = 0, dst = 5, g1()))

}

