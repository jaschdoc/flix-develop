
instance SemiGroup[Int8] {
    pub def combine(x: Int8, y: Int8): Int8 = x + y
}

instance SemiGroup[Int16] {
    pub def combine(x: Int16, y: Int16): Int16 = x + y
}

instance SemiGroup[Int32] {
    pub def combine(x: Int32, y: Int32): Int32 = x + y
}

instance SemiGroup[Int64] {
    pub def combine(x: Int64, y: Int64): Int64 = x + y
}

instance SemiGroup[BigInt] {
    pub def combine(x: BigInt, y: BigInt): BigInt = x + y
}

instance SemiGroup[Float32] {
    pub def combine(x: Float32, y: Float32): Float32 = x + y
}

instance SemiGroup[Float64] {
    pub def combine(x: Float64, y: Float64): Float64 = x + y
}
