def main(_args: Array[String]): Int32 & Impure =
    Console.printLine("");
    Console.printLine(Epoch.seconds() |> ToString.toString);
    Console.printLine(Epoch.milliseconds() |> ToString.toString);
    Console.printLine(Epoch.microseconds() |> ToString.toString);
    Console.printLine(Epoch.nanoseconds() |> ToString.toString);
    let l = 1 :: 2 :: 3 :: Nil;
    println(l);
    0