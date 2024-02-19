pub def main(): Unit \ IO =
    let sum = EffectExamples.Range.sumTo(42);
    println("The sum from 0..42 is: ${sum}")
