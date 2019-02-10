module IO

let showData values =
    values |> List.iter (printfn "%A")
