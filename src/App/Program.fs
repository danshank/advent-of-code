// Learn more about F# at http://fsharp.org

open System
open Say

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    let name = argv.[0]
    hello name
    0 // return an integer exit code
