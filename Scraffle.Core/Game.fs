module Scraffle.Core.Game

open System.Linq
open Scraffle.Core.Tile
open Scraffle.Core.Board

type Player = {
    hand: Tile List;
    score: ScoreValue;
}

type GameState = {
    turnsElapsed: int;
    players: Player list;
    board: Space seq seq;
    tileBag: TileBag;
}
type GameTurn = GameState -> GameState
type Game = GameTurn seq

module Game =
    let handSize = 7
    let boardSize = 15, 15

    let private createPlayer bag =
        let (hand, bagState) = { 1 .. handSize }
                                    |> Seq.fold (fun (h: Tile list, b) _ -> 
                                                    let (drawn, newBag) = TileBag.draw b
                                                    (drawn :: h, newBag)) (List.empty, bag)
        {hand = hand; score = 0}, bagState

    let private createBoard () =
        0
    
    let create players =
        let newBag = TileBag.create()
        let (playerSet, bagState) = { 0 .. players }
                                    |> Seq.fold (fun (p: Player list, b) _ ->
                                                    let (player, bag) = createPlayer b
                                                    (player :: p, bag)) (List.empty, newBag)

        { turnsElapsed = 0; players = playerSet; tileBag = bagState; board = Seq.empty }
