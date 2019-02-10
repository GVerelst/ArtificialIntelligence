module KmeansOneValue
open System
open IO

[<StructuredFormatDisplay("[x:{x}, c:{cluster}]")>]
type Row =
    {
        x: float;
        cluster: int;
    }

let showClustered values =
    values |> List.groupBy(fun r -> r.cluster)
           |> List.iter(fun (c, r) -> 
                printfn "Cluster %d" c 
                r |> showData)

let random = new Random(5);

let getValues = [ 1.0; 2.0; 3.0; 7.0; 8.0; 10.0; 11.0; 15.0; 16.0; 18.0; ]

let assignRandomClusters k xs =
    xs |> List.map (fun y -> {x = y; cluster = random.Next(k)} )

let calcIntDistance t u = Math.Abs(t.x - u.x)

let calcCentroids rows =
    rows |> List.groupBy(fun r -> r.cluster)
         |> List.map(fun (c, r) -> 
                    { 
                        x = r |> List.averageBy (fun x -> x.x);
                        cluster = c
                    })

let rec algo' calcDistance values =
    printfn "\nCall algo"
    values |> showClustered

    let centroids = values |> calcCentroids
    printfn "Centroids:"
    centroids |> showData

    let values' = values |> List.map (fun v -> 
            let nearest = centroids |> List.minBy(fun c -> calcDistance c v)
            { v with cluster = nearest.cluster }
        )

    if values = values' then printfn "\n\nSolution:"
                             values'
    else algo' calcDistance values'

// this is the condensed version of the alfo function
let rec algo calcDistance values =
    let values' = values |> List.map (fun v -> 
            let shortest = values |> calcCentroids 
                                  |> List.minBy(fun c -> calcDistance c v)
            { v with cluster = shortest.cluster }
        )

    if values = values' then values'
    else algo calcDistance values'


let calc =
    let k = 3       // 3 clusters
    getValues |> assignRandomClusters k |> algo calcIntDistance |> showClustered

    0
