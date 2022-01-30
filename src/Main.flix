def over(f: a -> b & ef, x: a): b & ef = f(x)

def main(_args: Array[String]): Int32 & Impure =
    let t1 = Time/Epoch.nanoseconds();
    let _ = 17 mod 8;
    let t2 = Time/Epoch.nanoseconds();
    let t3 = Time/Epoch.nanoseconds();
    let _ = 17 &&& (8 - 1);
    let t4 = Time/Epoch.nanoseconds();
    println("Regular mod time: ${t2 - t1} nano seconds");
    println("Bitwise mod time: ${t4 - t3} nano seconds");
    println("${0xFF}");
    println `over` 9124;
    println("a" + "b");
    println(7 rem 2 + 2);
    0

// pub def foo(): Unit =
//     true
//     +
//     42




