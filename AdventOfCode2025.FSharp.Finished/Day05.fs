module Day05

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
    let lns = File.ReadAllLines(dir + "/" + inputPath)|> Seq.toList;
    
    let lns0 = lns |> List.takeWhile ((<>)"")
    let lns1 = lns |> List.skipWhile ((<>)"") |> List.skip 1 |> List.takeWhile((<>)"") |> List.map int64

    let rngs = lns0 |> List.map (fun str -> str.Replace('-','|')) |> List.map (grabNumsGeneric int64) |> List.map (fun (x::y::[]) -> x,y)

    let goodNums = lns1 |> List.filter (fun x -> rngs |> List.exists (fun (a,b)-> x >= a && x <= b))




    let res = goodNums |> List.length
    ClipboardService.SetText(res.ToString())
    res
 

let solve2 (inputPath: string) =
    let lns = File.ReadAllLines(dir + "/" + inputPath)  |> Seq.toList;

    let lns0 = lns |> List.takeWhile ((<>)"")
    let rngs = lns0 |> List.map (fun str -> str.Replace('-','|')) |> List.map (grabNumsGeneric int64) |> List.map (fun (x::y::[]) -> x,y)

    let sorted = rngs |> List.sortBy fst
    let merged = 
        sorted |> List.skip 1
        |> List.fold (
            fun ((ha,hb)::others) (a,b)-> 
                if a <= hb
                then (ha,List.max [b;hb])::others
                else (a,b)::(ha,hb)::others

        ) [sorted[0]]

    let total = merged |> List.map (fun (a,b) -> b-a+1L) |> List.sum




    let res = total
    ClipboardService.SetText(res.ToString())
    res
