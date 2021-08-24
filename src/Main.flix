def main(_args: Array[String]): Int32 & Impure =
    Console.printLine("");
    Console.printLine(Epoch.miliSeconds() |> ToString.toString);
    Console.printLine(Epoch.seconds() |> ToString.toString);
    0