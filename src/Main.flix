def main(_args: Array[String]): Int32 & Impure =
    Console.printLine("");
    Console.printLine(Epoch.seconds() |> ToString.toString);
    Console.printLine(Epoch.milliseconds() |> ToString.toString);
    Console.printLine(Epoch.microseconds() |> ToString.toString);
    Console.printLine(Epoch.nanoseconds() |> ToString.toString);
    let l = 1 :: 2 :: 3 :: Nil;
    println(l);
    println(Set.range(-1000, 1000) |> Set1.toLazyList);
    (-11 mod 9) |> println;
    let t1 = Epoch.nanoseconds();
    let _ = 17 mod 8;
    let t2 = Epoch.nanoseconds();
    let t3 = Epoch.nanoseconds();
    let _ = 17 &&& (8 - 1);
    let t4 = Epoch.nanoseconds();
    println("Regular mod time: ${t2 - t1} nano seconds");
    println("Bitwise mod time: ${t4 - t3} nano seconds");
    0

