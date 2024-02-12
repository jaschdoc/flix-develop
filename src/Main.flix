///
/// The Ford-Fulkerson algorithm finds the maximum flow of a flow network.
/// Here it is implemented using a combination of functional programming
/// and datalog.
///
pub def main(): Unit \ IO =
    let g1 = FordFulkerson.exampleGraph01() |> FordFulkerson.maxFlow(src = 0, dst = 5);
    let g2 = FordFulkerson.exampleGraph02() |> FordFulkerson.maxFlow(src = 0, dst = 5);
    let g3 = FordFulkerson.exampleGraph03() |> FordFulkerson.maxFlow(src = 0, dst = 3);
    let g4 = FordFulkerson.exampleGraph04() |> FordFulkerson.maxFlow(src = 0, dst = 10);
    println("Expected maximum flow 19, got ${g1}");
    println("Expected maximum flow 20, got ${g2}");
    println("Expected maximum flow 2000, got ${g3}");
    println("Expected maximum flow 20, got ${g4}")
