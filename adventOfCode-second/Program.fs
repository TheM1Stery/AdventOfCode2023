open System
open FSharpPlus

let readFromConsoleIn () =
    Seq.initInfinite (fun _ -> Console.ReadLine())
    |> Seq.takeWhile ((<>) null)
    |> Seq.toList



let input =
    // "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    // |> String.split [ "\n" ]
    // |> Seq.toList
    readFromConsoleIn()

type Color =
    | Red
    | Green
    | Blue

type Dice = { Count: int; Color: Color }

type Game = { Number: int; Dices: Dice list }

let folder (acc, prev, num) ch =
    match ch with
    | _ when ch = 'G' ->
        let acc = { Number = 0; Dices = [] }
        (acc, ch, "")
    | _ when ch = ':' && Char.IsDigit prev-> 
        let acc = {Number = int num; Dices = []}
        (acc, ch, "")
    | _ when [ 'r'; 'g'; 'b' ] |> Seq.contains ch && Char.IsDigit prev ->
        let acc =
            { Number = acc.Number
              Dices =
                { Count = int num
                  Color =
                    match ch with
                    | _ when ch = 'r' -> Red
                    | _ when ch = 'g' -> Green
                    | _ when ch = 'b' -> Blue
                    | _ -> Red }
                :: acc.Dices }
        (acc, ch, "")
    | _ when Char.IsDigit ch ->
        (acc, ch, num + string ch)
    | _ -> (acc, ch, "")



let getValues input =
    input
    |> String.filter (fun x -> x <> ' ')
    |> Seq.fold folder ({ Number = 0; Dices = [] }, ' ', "")


let isNotIllegal (numberOfRed ,numberOfGreen ,numberOfBlue) game =
    let {Number = _; Dices = dices} = game
    let illegalExists dice =
        match dice with
        | {Color = Red; Count = count} when count > numberOfRed ->
            true
        | {Color = Blue; Count = count} when count > numberOfBlue ->
            true
        | {Color = Green; Count = count} when count > numberOfGreen ->
           true
        | _ -> false
        
        
      
    not (List.exists illegalExists dices)
    
   
let getAllMaxDices game =
    let {Number = _; Dices=dices} = game
    let filterAndMaxBy color = List.filter (fun x -> x.Color = color) >> List.maxBy (_.Count)
    let filter color = dices |> filterAndMaxBy color
    let maxRedDice = filter Red
    let maxBlueDice = filter Blue
    let maxGreenDice = filter Green
    (maxRedDice, maxGreenDice, maxBlueDice)
    
    
    
    



input
|> List.map getValues
|> List.map (fun (f, _, _) -> f)
|> List.map getAllMaxDices
|> List.map (fun (f,s,t) -> f.Count * s.Count * t.Count)
|> List.sum
// |> List.filter (isNotIllegal (12, 13,14))
// |> List.sumBy _.Number
|> printfn "%A"
 
