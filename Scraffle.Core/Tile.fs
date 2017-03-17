module Scraffle.Core.Tile

open System

type ScoreValue = int
type Tile = 
    | LetterTile of char * ScoreValue
    | BlankTile of char option
type TileBag = Tile list

module Tile =
    let string tile = 
        match tile with
        | LetterTile (c, s) -> string c
        | BlankTile c -> "*"

module TileBag = 
    open System.Collections.Generic

    let letterScores =
        [ 1, "AEIOULNRST"; 2, "DG"; 3, "BCMP"; 4, "FHVWY"; 5, "K"; 8, "JX"; 10, "QZ"; 0, "*" ]
        |> List.collect (fun (score, letters) -> 
                            letters |> Seq.map (fun l -> (l, score)) |> List.ofSeq)
        |> Map.ofList :> IDictionary<_,_> |> Dictionary

    let defaultTileSet = 
        [ 1, "JKQXZ"; 2, "BCFHMPVWY*"; 3, "G"; 4, "DLSU"; 6, "NRT"; 8, "O"; 9, "AI"; 12, "E"; ]
        |> List.map (fun (cnt, letters) -> String.replicate cnt letters)
        |> String.concat ""
        |> Seq.sort
        |> Seq.map (fun c -> match c with
                              | '*' -> BlankTile None
                              | _ -> LetterTile (c, letterScores.[c]))
        |> List.ofSeq
                              
    let randomize = List.sortBy (fun _ -> Guid.NewGuid())
    
    let create () = defaultTileSet |> randomize |> List.ofSeq
    
    let draw bag =
        let drawn = List.head bag
        let newBagState : TileBag = List.skip 1 bag

        drawn, newBagState

    let drawMany count bag =
        [ 1 .. count ]
            |> List.fold (fun (d, b) _ -> 
                            let (nd, nb) = draw b
                            (nd :: d, nb)) ([], bag)

    let exchange bag tiles =
        let (drawn, newBag) = drawMany (Seq.length tiles) bag
            
        let bagWithExchanged = List.concat [ tiles; newBag; ]
        drawn, randomize bagWithExchanged

    let drawSpecific bag ch =
        let tile = bag |> List.find (fun t -> Tile.string t = ch) 
        tile, List.except [tile] bag
    
