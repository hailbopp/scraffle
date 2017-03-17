module Tile = 

    open NUnit.Framework
    open FsUnit

    open Scraffle.Core.Tile

    [<TestFixture>]
    type ``Given a newly created TileBag`` ()=
        let bag = TileBag.create()

        let letters = Seq.map (fun l -> match l with | LetterTile (c, s) -> c | BlankTile o -> '*')
        let counts = Seq.groupBy id >> Seq.map (fun (k, vl) -> k, Seq.length vl) >> Map.ofSeq

        [<Test>]
        member __.
            ``when it is randomized, it should contain the same tiles in a different order`` ()=
               let newBag = TileBag.randomize bag
               let originalOrder = letters bag
               let newOrder = letters newBag
            
               originalOrder |> should not' (equal newOrder)
               counts originalOrder |> should equal (counts newOrder)
               
        [<Test>]
        member __.
            ``when a tile is drawn, the bag should contain one less of the drawn tile`` ()=
               let (drawn, newBag) = TileBag.draw bag
               
               let drawnCharacters = letters [drawn]
               Seq.length drawnCharacters |> should equal 1

               let originalChars = letters bag
               let originalCounts = counts originalChars
               let newChars = letters newBag 
               let newCounts = counts newChars
                
               let character = Seq.head drawnCharacters

               Seq.length originalChars |> should equal ((Seq.length newChars) + 1)
               Map.find character originalCounts |> should equal ((Map.find character newCounts) + 1)

