def main(_args: Array[String]): Int32 & Impure =
    let Map(t) = List.zip(List.range(0, 10000), List.range(0, 10000)) |> List.toMap;

    let t0Old = Time/Epoch.nanoseconds();
    let _ = Test.insertOld(10001, 10001, t);
    let t1Old = Time/Epoch.nanoseconds();

    let t0Inner = Time/Epoch.nanoseconds();
    let _ = Test.insertInner(10001, 10001, t);
    let t1Inner = Time/Epoch.nanoseconds();

    let t0InnerTail = Time/Epoch.nanoseconds();
    let _ = Test.insertInnerTail(10001, 10001, t);
    let t1InnerTail = Time/Epoch.nanoseconds();

    print("Old time               : ${t1Old - t0Old} nano seconds\n");
    print("Inner function time    : ${t1Inner - t0Inner} nano seconds\n");
    print("Tail rec inner fun time: ${t1InnerTail - t0InnerTail} nano seconds\n");

    0


namespace Test {


    use RedBlackTree.RedBlackTree;
    use RedBlackTree.RedBlackTree.{Node, Leaf, DoubleBlackLeaf};
    use RedBlackTree.Color;
    use RedBlackTree.Color.{Red, Black, DoubleBlack};

    pub def insertOld(k: k, v: v, tree: RedBlackTree[k, v]): RedBlackTree[k, v] with Order[k] =
        blacken(insertHelper(k, v, tree))

    pub def insertInner(k: k, v: v, tree: RedBlackTree[k, v]): RedBlackTree[k, v] with Order[k] =
        def loop(t) = match t {
            case Leaf                  => Node(Red, Leaf, k, v, Leaf)
            case Node(c, a, k1, v1, b) => match k <=> k1 {
                case LessThan    => balance(Node(c, loop(a), k1, v1, b))
                case EqualTo     => Node(c , a, k, v, b)
                case GreaterThan => balance(Node(c, a, k1, v1, loop(b)))
            }
            case _ => t
        };
        blacken(loop(tree))


    pub def insertInnerTail(k: k, v: v, tree: RedBlackTree[k, v]): RedBlackTree[k, v] with Order[k] =
        def loop(t, sk) = match t {
            case Leaf                  => sk(Node(Red, Leaf, k, v, Leaf))
            case Node(c, a, k1, v1, b) => match k <=> k1 {
                case LessThan    => loop(a, ts -> sk(balance(Node(c, ts, k1, v1, b))))
                case EqualTo     => sk(Node(c, a, k, v, b))
                case GreaterThan => loop(b, ts -> sk(balance(Node(c, a, k1, v1, ts))))
            }
            case _ => sk(t)
        };
        blacken(loop(tree, identity))


    def insertHelper(k: k, v: v, tree: RedBlackTree[k, v]): RedBlackTree[k, v] with Order[k] = match tree {
        case Leaf => Node(Red, Leaf, k, v, Leaf)
        case Node(color, a, k1, v1, b) =>
            match k <=> k1 {
                case LessThan => balance(Node(color, insertHelper(k, v, a), k1, v1, b))
                case EqualTo => Node(color, a, k, v, b)
                case GreaterThan => balance(Node(color, a, k1, v1, insertHelper(k, v, b)))
            }
        case _ => tree
    }


    def balance(tree: RedBlackTree[k, v]): RedBlackTree[k, v] = match tree {
        case Node(Black, Node(Red, Node(Red, a, k1, v1, b), k2, v2, c), k3, v3, d) =>
            Node(Red, Node(Black, a, k1, v1, b), k2, v2, Node(Black, c, k3, v3, d))
        case Node(Black, Node(Red, a, k1, v1, Node(Red, b, k2, v2, c)), k3, v3, d) =>
            Node(Red, Node(Black, a, k1, v1, b), k2, v2, Node(Black, c, k3, v3, d))
        case Node(Black, a, k1, v1, Node(Red, Node(Red, b, k2, v2, c), k3, v3, d)) =>
            Node(Red, Node(Black, a, k1, v1, b), k2, v2, Node(Black, c, k3, v3, d))
        case Node(Black, a, k1, v1, Node(Red, b, k2, v2, Node(Red, c, k3, v3, d))) =>
            Node(Red, Node(Black, a, k1, v1, b), k2, v2, Node(Black, c, k3, v3, d))
        case Node(DoubleBlack, a, k1, v1, Node(Red, Node(Red, b, k2, v2, c), k3, v3, d)) =>
            Node(Black, Node(Black, a, k1, v1, b), k2, v2, Node(Black, c, k3, v3, d))
        case Node(DoubleBlack, Node(Red, a, k1, v1, Node(Red, b, k2, v2, c)), k3, v3, d) =>
            Node(Black, Node(Black, a, k1, v1, b), k2, v2, Node(Black, c, k3, v3, d))
        case _ => tree
    }

    def blacken(tree: RedBlackTree[k, v]): RedBlackTree[k, v] = match tree {
        case Node(Red, Node(Red, a, k1, v1, b), k2, v2, c) =>
            Node(Black, Node(Red, a, k1, v1, b), k2, v2, c)
        case Node(Red, a, k1, v1, Node(Red, b, k2, v2, c)) =>
            Node(Black, a, k1, v1, Node(Red, b, k2, v2, c))
        case _ => tree
    }

}

