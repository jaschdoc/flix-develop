///
/// The type of the done function.
///
type alias RegionDone[ef: Region] = Unit -> Bool & ef

///
/// The type of the next function.
///
type alias RegionNext[a: Type, ef: Region] = Unit -> a & ef


pub enum RegionIterator[a: Type, r: Region] {
    case RegionIterator(RegionDone[r], RegionNext[a, r])
}


namespace RegionIterator {

    ///
    /// Returns `Some(x)` if `iter` is not empty. Returns `None` otherwise.
    ///
    /// Consumes the head element of `iter`.
    ///
    pub def next(iter: RegionIterator[a, r]): Option[a] \ Read(r) =
        let RegionIterator(done, next) = iter;
        if (done())
            None
        else
            Some(next())

}

namespace List {

    pub def toRegionIterator(r: Region[r], l: List[a]): RegionIterator[a, r] \ Write(r) =
        let cursor = ref l @ r;
        let done = () -> match (deref cursor) {
            case Nil => true
            case _   => false
        };
        let next = () -> match (deref cursor) {
            case Nil     => bug!("Empty Iterator!")
            case x :: xs =>
                cursor := xs;
                x
        };
        RegionIterator(done, next)

}
