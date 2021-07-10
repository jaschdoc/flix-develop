def main(_args: Array[String]): Int32 & Impure =
    let exitWith = msg: String -> {
        Console.printLine("");
        Console.printLine(msg);
        Console.printLine("");
        0
    };


    let e = {
        let size = 2;
        let a: Ref[Array[Int32]] = ref [default; size];

        Console.printLine((deref a) |> Array.formatWith(x -> "${x}", ","));
        
        (deref a) == [default, default]
        and Array.length(deref a) == size
        and { (deref a)[0] = 10; (deref a) == [10, default] }
        and {Console.printLine((deref a) |> Array.formatWith(x -> "${x}", ",")); true }
        and (deref a) == [10, default]
        and (deref a) != [default, default]
    };

    if (e) exitWith("SUCCESS") else exitWith("FAILURE")