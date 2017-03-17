module Scraffle.Core.Util

module List =
    let replaceAt index newEl input =
        input |> List.mapi (fun i el -> if i = index then [newEl; el] else [el])
              |> List.concat

