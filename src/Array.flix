namespace Array1 {

    // TODO: Add documentation.
    // TODO: Add tests
    // TODO: Rename to slice?
    def _copyOfRange(b: Int32, e: Int32, a: Array[a]): Array[a] & Impure =
        match (reifyType a) {
            case ReifiedInt8 =>
                let arr = a as Array[Int8];
                import java.util.Arrays:copyOfRange(Array[Int8], Int32, Int32) as copy;
                copy(arr, b, e) as Array[a]
            case ReifiedInt16 =>
                let arr = a as Array[Int16];
                import java.util.Arrays:copyOfRange(Array[Int16], Int32, Int32) as copy;
                copy(arr, b, e) as Array[a]
            case ReifiedInt32 =>
                let arr = a as Array[Int32];
                import java.util.Arrays:copyOfRange(Array[Int32], Int32, Int32) as copy;
                copy(arr, b, e) as Array[a]
            case ReifiedInt64 =>
                let arr = a as Array[Int64];
                import java.util.Arrays:copyOfRange(Array[Int64], Int32, Int32) as copy;
                copy(arr, b, e) as Array[a]
            case ReifiedFloat32 =>
                let arr = a as Array[Float32];
                import java.util.Arrays:copyOfRange(Array[Float32], Int32, Int32) as copy;
                copy(arr, b, e) as Array[a]
            case ReifiedFloat64 =>
                let arr = a as Array[Float64];
                import java.util.Arrays:copyOfRange(Array[Float64], Int32, Int32) as copy;
                copy(arr, b, e) as Array[a]
            case ReifiedBool =>
                let arr = a as Array[Bool];
                import java.util.Arrays:copyOfRange(Array[Bool], Int32, Int32) as copy;
                copy(arr, b, e) as Array[a]
            case ReifiedChar =>
                let arr = a as Array[Char];
                import java.util.Arrays:copyOfRange(Array[Char], Int32, Int32) as copy;
                copy(arr, b, e) as Array[a]
            case ReifiedArray(_) =>
                let arr = a as Array[##java.lang.Object];
                import java.util.Arrays:copyOfRange(Array[##java.lang.Object], Int32, Int32) as copy;
                copy(arr, b, e) as Array[a]
            case ErasedType =>
                let arr = a as Array[##java.lang.Object];
                import java.util.Arrays:copyOfRange(Array[##java.lang.Object], Int32, Int32) as copy;
                copy(arr, b, e) as Array[a]
            }
}