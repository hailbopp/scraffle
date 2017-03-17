module Scraffle.Core.Game

open System.Linq
open Scraffle.Core.Util
open Scraffle.Core.Tile
open Scraffle.Core.Space
open Scraffle.Core.Board

type Player = {
    hand: Tile list;
    score: ScoreValue;
}

type GameState = {
    turnsElapsed: int;
    players: Player list;
    board: Board;
    tileBag: TileBag;
}

type WordLocation = { coordinates: int * int; direction: WordDirection; }

type TurnAction =
    | PlayWord of string * Tile option list * WordLocation
    | ExchangeTiles of Tile list
    | Pass

type GameTurn = GameState -> TurnAction -> GameState
type Game = GameTurn seq

module Player =

    let private currentIndex gamestate = gamestate.turnsElapsed % (List.length gamestate.players)

    let current gamestate = List.item (currentIndex gamestate) gamestate.players

    let update gameState playerState =
        let idx = currentIndex gameState
        List.replaceAt idx playerState gameState.players

    let string player =
        let handString = player.hand |> List.map Tile.string |> String.concat " "
        sprintf "Score: %d\t\tTile Rack: %s" player.score handString
        

module Game =
    let handSize = 7
    let boardSize = 15

    let private createPlayer bag =
        let (hand, bagState) = TileBag.drawMany handSize bag
        {hand = hand; score = 0}, bagState
    
    let create players =
        let newBag = TileBag.create()
        let (playerSet, bagState) = { 1 .. players }
                                    |> Seq.fold (fun (p: Player list, b) _ ->
                                                    let (player, bag) = createPlayer b
                                                    (player :: p, bag)) (List.empty, newBag)
        let board = Board.create boardSize

        { turnsElapsed = 0; players = playerSet; tileBag = bagState; board = board }

    let private placeWord (word: Tile seq) (location: int * int) direction board : ScoreValue * Board =
        let boardOriented, position, height = Board.normalize location direction board
        let wordSpaces = Board.spacesSection position height (Seq.length word) boardOriented
        let wordScore, wordMult, newSpacesSegment = 
            Seq.fold2 (fun state lTile space -> 
                            let curScore, curWordMult, aggTiles = state
                            let curTileChar, curTileScore = match lTile with 
                                                            | LetterTile (tc, ts) -> tc, ts 
                                                            | BlankTile (Some bc) -> bc, 0
                                                            | BlankTile None -> failwith "Blank tiles must be declared as a letter before they can be placed."
                            let sc, multiplier, tile =                                 
                                match space.tile with
                                | Some t ->
                                    match t with
                                    | LetterTile (tc, tsc) when tc = curTileChar ->
                                        // If a letter tile already exists on the space, 
                                        // we should not count any multiplier on the space
                                        tsc, 1, { tile=Some t; multiplier=space.multiplier }
                                    | BlankTile (Some bc) when bc = curTileChar ->
                                        0, 1, { tile=Some t; multiplier=space.multiplier }
                                    | _ ->
                                        let wordString = word |> Seq.fold (fun w t -> w + Tile.string t ) ""
                                        failwith <| sprintf "Placing '%s' at %A is not valid." wordString location
                                | None -> 
                                    // If there's no tile here, we should multiply the
                                    // tile score by the space's Letter multiplier (if any)
                                    // and pass on any Word multiplier.
                                    match space.multiplier.scope with
                                    | Letter -> 
                                        space.multiplier.coefficient * curTileScore, 1, { tile=Some lTile; multiplier=space.multiplier }
                                    | Word -> 
                                        curTileScore, space.multiplier.coefficient, { tile=Some lTile; multiplier=space.multiplier }                                
                            (curScore + sc, curWordMult * multiplier, tile :: aggTiles)) (0, 1, List.empty) word wordSpaces                            
        
        wordScore * wordMult, Board.replaceSpaces position height newSpacesSegment board |> Board.standardize direction
        
    /// Convert any characters missing in word into tiles, and make sure that any missing tiles are already present on the board.
    /// Then, call placeWord to apply the tiles to the board and modify the player's score. Return a new gamestate.
    let playWord (word:string) (tiles: Tile option list) location direction gamestate =
        let player = Player.current gamestate
        let normalizedBoard, position, height = Board.normalize location direction gamestate.board
        let spaces = Board.spacesSection position height (word.Length) normalizedBoard
        let toPlaceTiles = 
            Seq.map3 (fun ch t space -> 
                        match t with
                        | None ->
                            match space.tile with
                            | Some st ->
                                    match st with
                                    | LetterTile (tc, tsc) when tc = ch ->
                                        LetterTile (tc, tsc)
                                    | BlankTile (Some bc) when bc = ch ->
                                        BlankTile (Some bc)
                                    | _ ->
                                        failwith <| sprintf "Placing '%s' at %A is not valid. '%c' conflicts with existing tile." word location ch
                            | None ->
                                failwith <| sprintf "Placing '%s' at %A is not valid. No tile was provided for '%c' and no tile exists on the board." word location ch
                        | Some providedTile ->
                            match space.tile with
                            | Some st ->
                                failwith <| sprintf "Placing '%s' at %A is not valid. A tile was provided for a space that is already occupied." word location
                            | None ->
                                match providedTile with
                                | LetterTile (pch, psc) when pch = ch -> providedTile
                                | BlankTile (Some bc) when bc = ch -> providedTile
                                | _ -> failwith <| sprintf "Placing '%s' at %A is not valid. Provided tiles do not match the word." word location
                        ) (word.ToUpper()) tiles spaces
        let playerTiles = tiles |> List.filter Option.isSome |> List.map Option.get
        let score, newBoard = placeWord toPlaceTiles location direction gamestate.board
        let newTiles, newBag = TileBag.drawMany (List.length playerTiles) gamestate.tileBag 
        let newHand = player.hand |> List.except playerTiles |> List.append newTiles
        let newPlayerState = { hand = newHand; score = player.score + score }
        { 
            turnsElapsed = gamestate.turnsElapsed; 
            players = Player.update gamestate newPlayerState;
            board = newBoard;
            tileBag = newBag;
        }

    let exchangeTiles tilesToExchange gamestate =
        let player = Player.current gamestate
        let isValid = tilesToExchange |> List.forall (fun t -> List.contains t player.hand)
        if not isValid then failwith "Player does not possess the tiles to exchange."
        let drawn, newBag = TileBag.exchange gamestate.tileBag tilesToExchange
        let newPlayerState = { hand = player.hand |> List.except tilesToExchange |> List.append drawn; score = player.score; }
        {
            turnsElapsed = gamestate.turnsElapsed;
            players = Player.update gamestate newPlayerState;
            board = gamestate.board;
            tileBag = newBag;
        }

    let passTurn gamestate = gamestate
        
    let performTurn state action = 
        let newGamestate = (match action with
                            | PlayWord (word, tiles, location) -> 
                                playWord word tiles location.coordinates location.direction
                            | ExchangeTiles tiles -> 
                                exchangeTiles tiles
                            | Pass -> 
                                passTurn) state
        { turnsElapsed = newGamestate.turnsElapsed + 1; players = newGamestate.players; board = newGamestate.board; tileBag = newGamestate.tileBag }

    let string gamestate = 
        Board.string gamestate.board 
            :: (gamestate.players |> List.map Player.string |> List.mapi (sprintf "\t Player %d: \t%s"))
            |> String.concat "\n\n"
    