def main(_args: Array[String]): Int32 & Impure =
    Console.printLine("");
    Console.printLine(Epoch.miliSeconds() |> ToString.toString);
    0