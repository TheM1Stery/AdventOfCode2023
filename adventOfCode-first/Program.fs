open System
open FSharpPlus


// First Part

// let list = Seq.initInfinite (fun _ -> Console.ReadLine()) |> Seq.takeWhile ((<>) null) |> Seq.toList
// let isDigit x = Char.IsDigit x
// let filterString str = String.filter isDigit str

// now that i think about it, this can be replaced by match
// let getNumber str = (if String.length str > 1 then $"{Seq.head str}{Seq.last str}" else $"{Seq.head str}{Seq.head str}") |> int


// let strNums = ["one"; "two"; "three"; "four"; "five";"six";"seven";"eight";"nine"]

// let getNumberTwo = (if String.length str > 1 then $"{Seq.head str}{Seq.last str}" else $"{Seq.head str}{Seq.head str}") |> int


// list
// |> List.map filterString
// |> List.map getNumber
// |> List.sum
// |> printfn "%d"

// let list =
//     "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen"
//         .Split
//         '\n'
//     |> Seq.toList
//
// printfn "%A" list


let list =
    Seq.initInfinite (fun _ -> Console.ReadLine())
    |> Seq.takeWhile ((<>) null)
    |> Seq.toList

let isDigit x = Char.IsDigit x

let filterString str = String.filter isDigit str

// now that i think about it, this can be replaced by match
let getNumber str =
    (if String.length str > 1 then
         $"{Seq.head str}{Seq.last str}"
     else
         $"{Seq.head str}{Seq.head str}")
    |> int


let digits =
    [ ("one", "1")
      ("two", "2")
      ("three", "3")
      ("four", "4")
      ("five", "5")
      ("six", "6")
      ("seven", "7")
      ("eight", "8")
      ("nine", "9") ]

let getDigits (digitsToCheck: (string * string) list) inputString =
    let folder acc digit =
        let checkRev = String.rev >> String.isSubString (fst digit |> String.rev)

        let acc =
            match inputString with
            | _ when inputString |> String.isSubString (fst digit) ->
                (digit, inputString |> String.findSliceIndex (fst digit)) :: acc
            | _ -> acc

        let acc =
            match inputString with
            | _ when inputString |> checkRev ->
                let reversedIndex =
                    inputString |> String.rev |> String.findSliceIndex (fst digit |> String.rev)

                (digit, String.length inputString - reversedIndex - (digit |> fst |> String.length))
                :: acc
            | _ -> acc

        acc

    let insert (acc: string, offset: int) (x: (string * string) * int) =
        (acc.Insert(snd x + offset, x |> fst |> snd), offset + 1)


    digitsToCheck
    |> List.fold folder []
    |> List.sortBy (fun x -> snd x)
    |> List.distinct
    |> List.fold insert (inputString, 0)




// "twonetwo" |> getDigits digits |> printf "%A"





list
|> List.map (getDigits digits)
|> List.map (fun x -> filterString (fst x))
|> List.map getNumber
|> List.sum
|> printfn "%A"
