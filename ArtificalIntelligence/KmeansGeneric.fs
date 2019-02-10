module KmeansGeneric

open System
open IO

[<StructuredFormatDisplay("[xs:{xs}, c:{cluster}]")>]
type GenericRow =
    {
        xs: float list;
        cluster: int;
    }

let showClustered values =
    values |> List.groupBy(fun r -> r.cluster)
           |> List.iter(fun (c, r) -> 
                printfn "\nCluster %d" c 
                r |> List.map (fun row -> row.xs) |> showData)

let random = new Random();

let sqr x = x * x

let getValues rows cols =
    List.init rows (fun _ -> List.init cols (fun _ -> random.NextDouble() * 500.0))

let rec assignRandomClusters k xss =
    let clustered = xss |> List.map (fun ys -> { xs = ys; cluster = random.Next(k)} )
    let nClusters = clustered |> List.groupBy(fun r -> r.cluster) |> List.length
    if nClusters = k then clustered
    else assignRandomClusters k xss     // try again

let calcListDistance (xs: float list) (ys: float list) =
    List.zip xs ys |> List.sumBy (fun (x,y) -> sqr(x - y))

let calcCentroids rows =
    let row1 = List.head rows
    let len  = List.length row1.xs

    rows |> List.groupBy(fun r -> r.cluster)
         |> List.map(fun (c, cxs) -> 
                    let init = List.init len (fun _ -> 0.0)
                    let n = (float) (List.length cxs)
                    let avgs =  cxs |> List.fold
                                        (fun acc r -> List.zip acc r.xs |> List.map(fun (a, b) -> a + b)) 
                                        init
                                    |> List.map(fun x -> x / n)
                    { 
                        xs = avgs;
                        cluster = c
                    })
                    
let rec algo calcDistance values =
    printfn "\nCall algo"
    values |> showClustered

    let centroids = values |> calcCentroids 
    printfn "Centroids:"
    centroids |> showData

    let values' = values |> List.map (fun v -> 
            let nearest = centroids |> List.minBy(fun c -> calcDistance c.xs v.xs)
            { v with cluster = nearest.cluster }
        )

    if values = values' then printfn "\n\nSolution:"
                             values'
    else algo calcDistance values'


let calc =
    let k = 3       // 3 clusters
    getValues 5 2 |> assignRandomClusters k |> algo calcListDistance |> showClustered

    0
