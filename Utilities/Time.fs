namespace AoC2022.Utilities

open System.Diagnostics
      
module Time =

    let displayElapsed (sw: Stopwatch) =
        printfn "Took %.1f minutes, (%.2fs, %dms)"
            (double sw.ElapsedMilliseconds / 60000.0)
            (double sw.ElapsedMilliseconds/1000.)
            sw.ElapsedMilliseconds