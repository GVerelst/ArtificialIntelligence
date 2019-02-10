// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =
    printfn "K-means, simple version"
    KmeansOneValue.calc

    printfn "\nK-means, generic version"
    KmeansGeneric.calc

    0 // return an integer exit code
