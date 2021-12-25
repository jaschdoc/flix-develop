def main(_args: Array[String]): Int32 & Impure =
    let t1 = Epoch.nanoseconds();
    let _ = 17 mod 8;
    let t2 = Epoch.nanoseconds();
    let t3 = Epoch.nanoseconds();
    let _ = 17 &&& (8 - 1);
    let t4 = Epoch.nanoseconds();
    println("Regular mod time: ${t2 - t1} nano seconds");
    println("Bitwise mod time: ${t4 - t3} nano seconds");
    println("${12.34 mod 56.78}");
    0

