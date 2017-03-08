module Scraffle.Core.Board

open Scraffle.Core.Space

type Board = Space seq seq

module Board =    
    let create size : Board =
        let (xSize, ySize) = (size, size)
        let rows = seq { 1 .. xSize } 
                    |> Seq.map (fun xc -> seq { 1 .. ySize } |> Seq.map (fun yc -> (xc, yc)))
        
        rows |> Seq.map (fun r -> r |> Seq.map (fun (xc, yc) -> 
                                                    { tile = None; 
                                                      multiplier = (ScoreMultiplier.getSpaceMultiplier size xc yc) }))

    let bareString (board:Board) : string =
        let getStrRep space = match space.multiplier |> Option.isSome with
                                | false -> "  "
                                | true -> ScoreMultiplier.string space.multiplier.Value

        board |> Seq.map (fun row -> 
                                row |> Seq.map getStrRep
                                    |> String.concat "|")
              |> String.concat "\n"
        
