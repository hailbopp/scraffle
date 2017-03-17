module Scraffle.Core.Board

open Scraffle.Core.Space

type Board = Space seq seq

type WordDirection =
    | Across
    | Down

module Board =    
    open System
    open System.Linq

    let create size : Board =
        let (xSize, ySize) = (size, size)
        let rows = { 1 .. xSize } 
                    |> Seq.map (fun xc -> { 1 .. ySize } |> Seq.map (fun yc -> (xc, yc)))
        
        rows |> Seq.map (fun r -> r |> Seq.map (fun (xc, yc) -> 
                                                    { tile = None; 
                                                      multiplier = (ScoreMultiplier.getSpaceMultiplier size xc yc) }))

    // Characters for drawing a board: ┼─├┤┬┴└┘┌┐
    let private leftPad = String.replicate (Int32.MaxValue |> string |> String.length) " "
    let private stringifyRow : Space seq -> string = Seq.map Space.string >> String.concat "│" >> sprintf "│%s│"
    let private enumerator count = Enumerable.Range(0, count)
    let private rowSeparator = 
        enumerator >> Seq.map (fun _ -> "───") >> String.concat "┼" 
            >> sprintf "\n%s ├%s┤\n" (String.replicate (Int32.MaxValue |> string |> String.length) " ")

    let private chars = [| 'A' .. 'Z' |]
    let private columnIdentifiers = chars |> Seq.map string

        // TODO: Support more than 26 columns. e.g. AA, AB, etc.
        //Seq.initInfinite (fun n ->
        //    let alphabetLen = Array.length chars
        //    let rec b26 num (lst: char list) =
        //        let cn = num / alphabetLen
        //        match cn with
        //        | 0 -> chars.[num] :: lst
        //        | _ -> b26 (num - alphabetLen) ( chars.[(num / alphabetLen) - 1] :: lst )
        //    b26 n List.empty
        //    |> List.map string |> List.rev 
        //    |> Seq.ofList |> String.concat "")

    let private rowIdentifiers = 
        let maxLen = Int32.MaxValue |> string |> String.length
        { 1 .. Int32.MaxValue } 
            |> Seq.map (fun n -> sprintf "%s%d " (String.replicate (maxLen - (n |> string |> String.length)) " ") n)
    
    let column i board = board |> Seq.map (Seq.item i)
    let row : int -> Board -> Space seq = Seq.item 
    let pivot rows =
        let colCount = rows |> Seq.head |> Seq.length
        { 0 .. colCount - 1 } |> Seq.map (fun i -> column i rows)

    let normalize location direction board =
        let (startX, startY) = location
        match direction with
            | Across -> board, startX, startY
            | Down -> pivot board, startY, startX

    let standardize direction board = match direction with | Across -> board | Down -> pivot board

    /// Retrieves a section of `length` spaces starting at `position` on row `height` of `board`
    let spacesSection position height length board = row height board |> Seq.skip (position - 1) |> Seq.take length
    
    /// Splice a sequence of new spaces of length into a row height, at position, and return the resulting board
    let replaceSpaces position height newSpacesSegment board = 
        let spaces = row height board
        let newSpaces = Seq.concat [Seq.skip (position - 1) spaces; Seq.ofList newSpacesSegment; Seq.skip (position + (Seq.length newSpacesSegment) - 1) spaces;]
        Seq.concat [Seq.skip (height - 1) board; seq [ newSpaces ]; Seq.skip (height) board]

    let string (board: Board) =         
        let rows = board |> Seq.map stringifyRow |> Seq.map2 (sprintf "%s%s") rowIdentifiers 
        let numColumns = Seq.length rows
        let separator = rowSeparator numColumns
        let gridInterior = rows |> String.concat separator
        let header = columnIdentifiers |> Seq.take numColumns |> Seq.map (sprintf " %s  ") |> String.concat ""
                     |> sprintf "%s  %s" leftPad
        let topEdge = sprintf "%s ┌%s┐" leftPad (enumerator numColumns |> Seq.map (fun _ -> "───") |> String.concat "┬")
        let bottomEdge = sprintf "%s └%s┘" leftPad (enumerator numColumns |> Seq.map (fun _ -> "───") |> String.concat "┴")
        [ header; topEdge; gridInterior; bottomEdge; ] |> String.concat "\n"

    let bareString board =
        let getStrRep space = ScoreMultiplier.string space.multiplier

        board |> Seq.map (fun row -> 
                                row |> Seq.map getStrRep
                                    |> String.concat "│")
              |> String.concat "\n"
        
