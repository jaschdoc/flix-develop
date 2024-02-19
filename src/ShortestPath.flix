mod ShortestPath {

    use Down.Down;

    ///
    /// Returns a mapping from distances to the set of reachable vertices for which the
    /// shortest path from `src` in the directed graph `g` is of a given length.
    ///
    pub def sssp(src: t, g: m[(t, t)]): Map[Int32, Set[t]] with Foldable[m], Order[t] = {
        let edges = inject g into Edge;
        let frontiers = #{
            Dist(src; Down(0)).
            Dist(y; n + Down(1)) :- Dist(x; n), Edge(x, y).

            // Find the max frontier.
            Frontiers(;Set#{coerce(n)}) :- fix Dist(_; n).
            MaxFrontier(Set.maximum(s) |> Option.getWithDefault(0)) :- fix Frontiers(;s).

            // Initialize all frontiers (with non-bot element).
            Frontier(0; Some(Set#{})) :- MaxFrontier(m), if m > 0.
            Frontier(n+1; Some(Set#{})) :- Frontier(n; _), MaxFrontier(m), if n < m.

            // Collect the frontiers.
            Frontier(coerce(n); Some(Set#{x})) :- fix Dist(x; n).
        };
        let res = query edges, frontiers select (n, s) from Frontier(n; s);
        let unwrapOption = match (n, s) -> match s {
            case Some(v) => Some((n, v))
            case None => None
        };
        res |> Vector.filterMap(unwrapOption)
            |> Vector.toMap
    }

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
    ///
    /// ```
    /// 0 -> 1
    /// 0 -> 3
    /// 1 -> 3
    /// 1 -> 2
    /// 1 -> 4
    /// 3 -> 4
    /// 4 -> 2
    /// 2 -> 5
    /// 4 -> 5
    /// ```
    ///
    /// The shortest path from 0 to 5 is `0 -> 3 -> 4 -> 5`.
    ///
    pub def exampleGraph01(): Set[(Int32, Int32)] =
        Set#{ (0, 1), (0, 3), (1, 4), (1, 2), (1, 3), (2, 5), (3, 4), (4, 2), (4, 5) }

    ///
    /// Returns the following graph:
    ///
    /// ```
    ///      1---2
    ///     / \   \
    ///    0   \   5
    ///     \   \ /
    ///      3---4
    /// ```
    ///
    /// The edges are directed as follows (ordered from left to right, top to bottom):
    ///
    /// ```
    /// 0 -> 1
    /// 0 -> 3
    /// 1 -> 2
    /// 1 -> 4
    /// 3 -> 4
    /// 2 -> 5
    /// 4 -> 5
    /// ```
    ///
    /// The shortest path from 0 to 5 is `0 -> 3 -> 4 -> 5`.
    ///
    pub def exampleGraph02(): Set[(Int32, Int32)] =
        Set#{ (0, 1), (0, 3), (1, 2), (1, 4), (2, 5), (3, 4), (4, 5) }

    ///
    /// Returns the following graph:
    ///
    /// ```
    ///      1
    ///     /|\
    ///    0 | 3
    ///     \|/
    ///      2
    /// ```
    ///
    /// The edges are directed as follows (ordered from left to right, top to bottom):
    ///
    /// ```
    /// 0 -> 1
    /// 0 -> 2
    /// 1 -> 2
    /// 1 -> 3
    /// 2 -> 3
    /// ```
    ///
    /// The shortest path from 0 to 3 is `0 -> 2 -> 3`.
    ///
    pub def exampleGraph03(): Set[(Int32, Int32)] =
        Set#{ (0, 1), (0, 2), (1, 2), (1, 3), (2, 3) }

    ///
    /// Returns the following graph:
    ///
    /// ```
    ///       1--2--3
    ///      /|  |\  \
    ///     / |  | \  \
    ///    0--4--5--6--10
    ///     \   /  /| /
    ///      \ /  / |/
    ///       7--8--9
    /// ```
    ///
    /// The edges are directed as follows (ordered from left to right, top to bottom):
    ///
    /// ```
    /// 0 -> 1
    /// 0 -> 4
    /// 0 -> 7
    /// 4 -> 1
    /// 1 -> 2
    /// 4 -> 5
    /// 5 -> 7
    /// 7 -> 8
    /// 2 -> 5
    /// 2 -> 3
    /// 6 -> 2
    /// 5 -> 6
    /// 8 -> 6
    /// 8 -> 9
    /// 6 -> 9
    /// 3 -> 10
    /// 6 -> 10
    /// 9 -> 10
    /// ```
    ///
    /// The shortest path from 0 to 10 is `0 -> 7 -> 8 -> 9 -> 10`.
    ///
    pub def exampleGraph04(): Set[(Int32, Int32)] =
        Set#{  (0, 1), (0, 4 ), (0, 7 ), (1, 2 ), (2, 3),
               (2, 5), (3, 10), (4, 1 ), (4, 5 ), (5, 7),
               (5, 6), (6, 9 ), (6, 2 ), (6, 10), (7, 8),
               (8, 6), (8, 9 ), (9, 10)
            }

    ///
    /// Returns the following graph:
    ///
    /// ```
    ///      1---2
    ///     / \   \
    ///    0   \   6
    ///     \   \ /
    ///      3---5
    ///      |
    ///      4
    /// ```
    ///
    /// The edges are directed as follows (ordered from left to right, top to bottom):
    ///
    /// ```
    /// 0 -> 1
    /// 0 -> 3
    /// 1 -> 2
    /// 1 -> 5
    /// 3 -> 5
    /// 3 -> 4
    /// 2 -> 6
    /// 5 -> 6
    /// ```
    ///
    /// The shortest path from 0 to 6 is `0 -> 3 -> 5 -> 6`.
    ///
    pub def exampleGraph05(): Set[(Int32, Int32)] =
        Set#{ (0, 1), (0, 3), (1, 2), (1, 5), (2, 6), (3, 5), (3, 4), (5, 6) }

    @Test
    def testExampleGraphShortestPaths01(): Bool =
        Assert.eq(Map#{0 => Set#{0}, 1 => Set#{0, 1}, 2 => Set#{}, 3 => Set#{}, 4 => Set#{}, 5 => Set#{}}, sssp(0, exampleGraph01()))

    @Test
    def testExampleGraphShortestPaths02(): Bool =
        Assert.eq(Map#{0 => Set#{}, 1 => Set#{}, 2 => Set#{}, 3 => Set#{}, 4 => Set#{}, 5 => Set#{}}, sssp(0, exampleGraph02()))

    @Test
    def testExampleGraphShortestPaths03(): Bool =
        Assert.eq(Map#{0 => Set#{}, 1 => Set#{}, 2 => Set#{}, 3 => Set#{}}, sssp(0, exampleGraph03()))

    @Test
    def testExampleGraphShortestPaths04(): Bool =
        Assert.eq(Map#{0 => Set#{}, 1 => Set#{}, 2 => Set#{}, 3 => Set#{}, 4 => Set#{}, 5 => Set#{}, 6 => Set#{}, 7 => Set#{}, 8 => Set#{}, 9 => Set#{}, 10 => Set#{}}, sssp(0, exampleGraph04()))

    @Test
    def testExampleGraphShortestPaths05(): Bool =
        Assert.eq(Map#{0 => Set#{}, 1 => Set#{}, 2 => Set#{}, 3 => Set#{}, 4 => Set#{}, 5 => Set#{}, 6 => Set#{}}, sssp(0, exampleGraph05()))

}
