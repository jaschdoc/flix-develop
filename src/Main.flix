def main(_args: Array[String]): Int32 & Impure =
    Console.printLine("");

    Console.print(Epoch.miliSeconds() |> ToString.toString);
    
    Console.printLine("");
    0