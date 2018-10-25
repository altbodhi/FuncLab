module Util

open Game
let loadMap fn = 
    let map = System.IO.File.ReadAllLines fn 
    [ for i = 0 to (map |> Array.length) - 1 do
        for j = 0 to map.[0].Length - 1 do
            let word = string map.[i].[j]
            yield (createElement word (j, i)) ] 
            |> List.filter (function | Some e -> true | None -> false)
            |> List.map (fun x -> x.Value)