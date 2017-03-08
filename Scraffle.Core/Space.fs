module Scraffle.Core.Space

open Scraffle.Core.Tile

type ScoreMultiplierScope = 
    | Letter 
    | Word
type ScoreMultiplier = { coefficient: double; scope: ScoreMultiplierScope; }
type Space = { tile: Tile option; multiplier: ScoreMultiplier option; }


module ScoreMultiplier =
    (* A MultiplierStrategy is a function that takes in board size, X, and Y, and returns a ScoreMultiplier *)
    type MultiplierStrategy = int -> int -> int -> ScoreMultiplier option

    /// Convert top-left-to-bottom-right coordinates for a board of `size` into cartesian coordinates.
    let cartesian size x y =
        let middle = (float size / float 2) |> ceil |> int
        let cartX = match x with
                     | xc when xc = middle -> 0
                     | xc when xc < middle -> -1 * (middle - xc)
                     | xc when xc > middle -> xc - middle
        let cartY = match y with
                     | xc when xc = middle -> 0
                     | yc when yc < middle -> middle - yc
                     | yc when yc > middle -> -1 * (yc - middle)
        cartX, cartY

    let getDoubleWord size x y = 
        let (cx, cy) = cartesian size x y
        match abs cx with
            | i when (abs cy) = i -> 
                    Some { scope = Word; coefficient = 2.0 }
            | _ -> None

    let getTripleWord size x y = 
        let (minCart, maxCart) = cartesian size 1 1
        let (cx, cy) = cartesian size x y

        let onEdge = abs cx = maxCart || abs cy = maxCart
        let onAxis = cx = 0 || cy = 0
        let onMiddleEdge = onAxis && onEdge
        let onCorner = (abs cx, abs cy) = (maxCart, maxCart)
        
        match onMiddleEdge || onCorner with
            | true -> Some { scope = Word; coefficient = 3.0 }
            | _ -> None

    /// Each cartesian quadrant contains four triple letter score tiles, one on each interior corner,
    /// padded by one tile.
    /// e.g. on a standard 15x15 board, [(2,2); (2,6); (6,2); (6,6);] would be triple letter tiles,
    /// mirrored across all 4 quadrants.
    let getTripleLetter size x y =
        let high = cartesian size 1 1 |> snd |> fun x -> x - 1
        let coords = cartesian size x y |> (fun (a, b) -> abs a, abs b)        
        let validSet = [(2, 2); (2, high); (high, 2); (high, high);]
        match List.contains coords validSet with
            | true -> Some { scope = Letter; coefficient = 3.0 }
            | _ -> None

    let getDoubleLetter size x y =
        let max = cartesian size 1 1 |> snd
        let (cx, cy) = cartesian size x y |> (fun (a, b) -> abs a, abs b) 
        let axisVal = (float max / float 2) |> ceil |> int
        let innerVal = axisVal + 1
        let atAxisVal = (cx, cy) = (0, axisVal) || (cx, cy) = (axisVal, 0)
                        || (cx, cy) = (axisVal, max) || (cx, cy) = (max, axisVal)
        let atInner = (cx, cy) = (innerVal, 1) || (cx, cy) = (1, innerVal)
        let atFirst = (cx, cy) = (1,1)
        match atFirst || atInner || atAxisVal with
            | true -> Some { scope = Letter; coefficient = 2.0 }
            | _ -> None     
    
    let strategies : MultiplierStrategy list = [ getDoubleWord; getTripleWord; getTripleLetter; getDoubleLetter; ]

    let distanceFromOrigin cx cy =
        let x = abs cx
        let y = abs cy
        (x * x) + (y * y) |> float |> sqrt

    /// Determine which multiplier applies for the given space.
    /// The further out on the board that the space is from the origin, the higher potential score
    /// from multipliers. This implies that 3W > 2W > 3L > 2L.
    let getSpaceMultiplier size x y =
        let max = cartesian size 1 1 |> snd
        let multipliers = strategies
                            |> List.map (fun strategy -> strategy size x y)
                            |> List.filter Option.isSome
                            |> List.map Option.get

        match List.length multipliers with
        | 0 -> None
        | _ ->
            let (cx, cy) = cartesian size x y
            let distance = distanceFromOrigin cx cy

            multipliers 
                |> List.sortByDescending
                    (match distance with
                        | d when (d / (float max)) < 0.5 -> 
                                (fun m -> m.scope = Letter, m.coefficient * -1.0)
                        | _ -> 
                                (fun m -> m.scope = Word, m.coefficient))
                |> List.map Some
                |> List.head
            

    let string multiplier = 
        let coef = multiplier.coefficient |> int |> string
        let scope = match multiplier.scope with
                        | Letter -> "L"
                        | Word -> "W"
        coef + scope