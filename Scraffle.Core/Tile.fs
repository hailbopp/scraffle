module Scraffle.Core.Tile

open System

type ScoreValue = int
type Tile = 
    | Letter of char * ScoreValue
    | BlankTile
type TileBag = Tile list

module TileBag = 
    let private letterScores =
        [ 1, "AEIOULNRST"; 2, "DG"; 3, "BCMP"; 4, "FHVWY"; 5, "K"; 8, "JX"; 10, "QZ"; ]
        |> List.collect (fun (score, letters) -> 
                            letters |> Seq.map (fun l -> (l, score)) |> List.ofSeq)
        |> Map.ofList

    let private defaultTileSet = 
        [ 1, "JKQXZ"; 2, "BCFHMPVWY*"; 3, "G"; 4, "DLSU"; 6, "NRT"; 8, "O"; 9, "AI"; 12, "E"; ]
        |> List.map (fun (cnt, letters) -> String.replicate cnt letters)
        |> String.concat ""
        |> Seq.sort
        |> Seq.map (fun c -> match c with
                              | '*' -> BlankTile
                              | _ -> Letter (c, letterScores.[c]))
                              
    let randomize = Seq.sortBy (fun _ -> Guid.NewGuid())
    
    let create () : TileBag = defaultTileSet |> randomize |> List.ofSeq
    
    let draw (bag : TileBag) =
        let drawn = List.head bag
        let newBagState : TileBag = bag |> List.skip 1

        drawn, newBagState

    
