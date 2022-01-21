def over(f: a -> b & ef, x: a): b & ef = f(x)

def main(_args: Array[String]): Int32 & Impure =
    let t1 = Epoch.nanoseconds();
    let _ = 17 mod 8;
    let t2 = Epoch.nanoseconds();
    let t3 = Epoch.nanoseconds();
    let _ = 17 &&& (8 - 1);
    let t4 = Epoch.nanoseconds();
    println("Regular mod time: ${t2 - t1} nano seconds");
    println("Bitwise mod time: ${t4 - t3} nano seconds");
    println("${0xFF}");
    println `over` 9124;
    0

// pub def foo(): Unit =
//     true
//     +
//     42




