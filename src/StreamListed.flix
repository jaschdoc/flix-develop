pub enum StreamListed[a, s] {
    case StreamL(s -> Step[a, s], s)
    case Listed(List[a])
}

namespace StreamListed {
    

    pub def empty(): StreamListed[a, s] =
        StreamL(_ -> Done, default)


    ///
    /// Returns true if and only if the step function of `s` evaluates to `Done`.
    ///
    pub def isEmpty(s: StreamListed[a, s]): Bool = match s {
        case StreamL(next0, s0) => match next0(s0) {
            case Done => true
            case _    => false
        }
        case Listed(l)          => l |> List.isEmpty
    }
        

    pub def toList(s: StreamListed[a, s]): List[a] = match s {
        case StreamL(next0, s0) => toListAcc(next0, s0, ks -> ks)
        case Listed(l)          => l
    }
    
    ///
    /// Helper function for `toList`.
    ///
    def toListAcc(next: s -> Step[a, s], s0: s, k: List[a] -> List[a]): List[a] = match next(s0) {
        case Done         => k(Nil)
        case Skip(s1)     => toListAcc(next, s1, k)
        case Yield(x, s1) => toListAcc(next, s1, ks -> k(x :: ks))
    }


}