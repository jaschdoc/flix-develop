mod Path {

    use Path.{Path, Bot};

    pub enum Path with ToString {
        case Path(List[Int32])
        case Bot // Infinitely long path
    }

    instance Eq[Path] {
        pub def eq(x: Path, y: Path): Bool = match (x, y) {
            case (Path(xs), Path(ys)) => List.length(xs) == List.length(ys)
            case (Bot, _)             => false
            case (_, Bot)             => false
        }
    }

    instance Order[Path] {
        pub def compare(x: Path, y: Path): Comparison = match (x, y) {
            case (Path(xs), Path(ys)) => List.length(xs) <=> List.length(ys)
            case (Bot, _)             => Comparison.GreaterThan
            case (_, Bot)             => Comparison.GreaterThan
        }
    }

    instance LowerBound[Path] {
        // The longest list
        pub def minValue(): Path = Bot
    }

    instance PartialOrder[Path] {
        pub def lessEqual(x: Path, y: Path): Bool = match (x, y) {
            case (Path(xs), Path(ys)) => List.length(xs) >= List.length(ys)
            case (Bot, _)             => false
            case (_, Bot)             => false
        }
    }

    instance JoinLattice[Path] {
        pub def leastUpperBound(x: Path, y: Path): Path = match (x, y) {
            case (Path(xs), Path(ys)) => if (List.length(xs) <= List.length(ys)) x else y
            case (Bot, _)             => Bot
            case (_, Bot)             => Bot
        }
    }

    instance MeetLattice[Path] {
        pub def greatestLowerBound(x: Path, y: Path): Path = match (x, y) {
            case (Path(xs), Path(ys)) => if (List.length(xs) > List.length(ys)) x else y
            case (Bot, _)             => Bot
            case (_, Bot)             => Bot
        }
    }

    ///
    /// Returns a `Path` from `x` to `y`.
    ///
    pub def init(y: Int32, x: Int32): Path =
        Path(y :: x :: Nil)

    ///
    /// Extends the `Path` `p` with `z`.
    ///
    pub def cons(z: Int32, p: Path): Path = match p {
        case Path(xs) => Path(z :: xs)
        case Bot      => Bot
    }

    ///
    /// Returns `Some(p)` if `p` is a non-empty `Path`.
    ///
    pub def toOption(p: Path): Option[Path] = match p {
        case Path(xs) if List.isEmpty(xs) => None
        case Path(xs)                     => Some(Path(xs))
        case Bot                          => None
    }

    ///
    /// Returns the index of `a` in the `Path` `p`.
    /// Note that a `Path` is sorted in descending order.
    ///
    pub def indexOf(a: Int32, p: Path): Option[Int32] = match p {
        case Path(xs) => List.indexOf(a, xs)
        case Bot      => None
    }

    ///
    /// Reverses the `Path` `p`.
    ///
    pub def reverse(p: Path): Path = match p {
        case Path(xs) => Path(List.reverse(xs))
        case Bot      => Bot
    }

}
