def main(_args: Array[String]): Int32 & Impure =
    Console.printLine("");
    let l = List.range(0, 1000);
    Console.printLine(ToString.toString(l));
    0