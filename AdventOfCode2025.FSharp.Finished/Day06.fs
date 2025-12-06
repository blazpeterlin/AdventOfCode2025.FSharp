module Day06

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
    
    let nums = lns |> List.take (lns.Length-1) |> List.map (grabNumsGeneric int64) |> List.transpose
    let ops = lns|> List.last |> fun x -> x.Split(" ", StringSplitOptions.RemoveEmptyEntries) |> List.ofSeq

    let allRes = 
        nums 
        |> List.zip ops 
        |> List.map (fun (op, nums) ->
            match op with
            | "+" -> nums |> List.fold (+) 0L
            | "*" -> nums |> List.fold (fun a b -> a*b) 1L
        )



    let res = allRes |> List.sum
    ClipboardService.SetText(res.ToString())
    res
 

let solve2 (inputPath: string) =
    let lns = File.ReadAllLines(dir + "/" + inputPath) |> Seq.filter((<>)"") |> Seq.toList;

    //let nums = lns |> List.take (lns.Length-1) |> List.map (grabNumsGeneric id) |> List.transpose
    let ops = lns|> List.last |> fun x -> x.Split(" ", StringSplitOptions.RemoveEmptyEntries) |> List.ofSeq
    let indexes0 = 
        lns |> List.last |> fun s -> s.ToCharArray() |> Seq.indexed |> Seq.filter (fun (idx,ch) -> ch <> ' ') |> Seq.map fst
    let indexes = 
        Seq.append 
            indexes0
            (lns |> List.last |> fun x -> [x.Length])
    let lens = indexes |> Seq.pairwise |> Seq.map (fun (a,b) -> a,(b-a)) |> List.ofSeq
    let strs = lns |>List.take (lns.Length-1)
    let numsOfStrs = 
        List.allPairs lens strs
        |> List.map (fun ((idx,len),s)  ->
            s.Substring(idx,len)
        )
        |> List.chunkBySize (strs.Length)

    let allRes = 
        numsOfStrs 
        |> List.zip ops 
        |> List.map (fun (op, numsOfStrs) ->

            let chars = numsOfStrs |> Seq.map (fun str -> str.ToCharArray() |> Seq.toList)
            //let maxLen = chars |> Seq.map List.length |> Seq.max
            //let charsModified = 
            //    chars 
            //    |> Seq.map (fun chs -> 
            //        [1..(maxLen - chs.Length)]
            //        |> List.fold (fun chsi _ -> chsi@[' ']) chs
            //    )
            let inverted = chars |>Seq.transpose |> Seq.map (fun chs -> String(chs |>Array.ofSeq))
            let nums = inverted |> Seq.filter (fun str -> str.Trim() <>"") |>Seq.map int64 |>List.ofSeq

            let r =
                match op with
                | "+" -> nums |> List.fold (+) 0L
                | "*" -> nums |> List.fold (fun a b -> a*b) 1L
            r
        )



    let res = allRes |> List.sum
    ClipboardService.SetText(res.ToString())
    res
 
