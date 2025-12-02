module Day02

open Common
open System
open System.Collections.Generic
open Microsoft.FSharp.Core.Operators.Checked
open Common.Operators
open System.IO
open TextCopy

let dir = __SOURCE_DIRECTORY__

//type Present = { X: int; Y: int; Z: int; }

//let parseLn (ln:string) =
//    match ln with
//    | Regex @"^(\w+) blabla (\d+)$" [a; Int b] ->
//        (a,b)
        


let solve1 (inputPath: string) =
    let lns = File.ReadAllLines(dir + "/" + inputPath) |> Seq.filter((<>)"") |> Seq.toList;
    
    let splits = 
        lns[0].Replace("-","|")
        |> fun x -> x.Split(",") |> List.ofSeq |> List.except [""] 
        |> List.map (grabNumsGeneric int64) |> List.map (fun (x::y::[]) -> [x..1L..y])

    let justNums = splits |> List.concat
    let invalids = justNums |> List.filter(
        fun num -> 
            let str = num.ToString()
            let str1 = str.Substring(0, str.Length/2)
            let str2 = str.Substring(str.Length/2)
            str1=str2
    )
    let invalidSum = invalids |> List.sum



    let res = invalidSum
    ClipboardService.SetText(res.ToString())
    res
 

let solve2 (inputPath: string) =
    let lns = File.ReadAllLines(dir + "/" + inputPath) |> Seq.filter((<>)"") |> Seq.toList;

    let splits = 
        lns[0].Replace("-","|")
        |> fun x -> x.Split(",") |> List.ofSeq |> List.except [""] 
        |> List.map (grabNumsGeneric int64) |> List.map (fun (x::y::[]) -> [x..1L..y])

    let justNums = splits |> List.concat
    let invalids = justNums |> List.filter(
        fun num -> 
            let str = num.ToString()
            seq { 1..str.Length/2 }
            |> Seq.exists (fun i -> 
                if str.Length%i <> 0 then false else
                let strs = [0..i..str.Length-1] |> List.map (fun idx -> str.Substring(idx,i)) |> List.distinct
                strs.Length = 1
            )
    )
    let invalidSum = invalids |> List.sum



    let res = invalidSum
    ClipboardService.SetText(res.ToString())
    res
