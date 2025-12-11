module Day11_countDfs

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
    
    let moves = 
        lns
        |> List.map (fun ln -> 
            match (ln.Split([|':';' '|],StringSplitOptions.RemoveEmptyEntries) |> List.ofArray) with
            | head::lst -> head,lst
           )

        |> Map.ofList

    let nextS (s: string list) =
        let h = s.Head
        if not(moves.ContainsKey(h)) then Seq.empty else
        seq {
            for next in moves[h] do
                yield (next::s),1
        }

        

    let st0 = ["you"]

    let allPaths =
        CommonGraph.AStarSeq st0 (fun x -> 0) (fun x -> x.Head="out") nextS


    let res = allPaths |> Seq.length
    ClipboardService.SetText(res.ToString())
    res
 

let solve2 (inputPath: string) =
    let lns = File.ReadAllLines(dir + "/" + inputPath) |> Seq.filter((<>)"") |> Seq.toList;
 
    let moves = 
        lns
        |> List.map (fun ln -> 
            match (ln.Split([|':';' '|],StringSplitOptions.RemoveEmptyEntries) |> List.ofArray) with
            | head::lst -> head,lst
           )

        |> Map.ofList

    let nextS (filteredTgt:HashSet<string>) (node: string)  =
        if filteredTgt.Contains(node) then Seq.empty else
        if not(moves.ContainsKey(node)) then Seq.empty else
        moves.TryFind node |> Option.defaultValue List.empty |> Seq.ofList

    let memoized = Dictionary<string*string,int64>()
    let rec countDfs getNext target node  = 
        if memoized.ContainsKey(node,target) then memoized[node,target] else
        if node=target then 1L else
        let r = getNext node |> Seq.map (countDfs getNext target) |> Seq.sum
        memoized[(node,target)] <- r
        r


    let countPaths (a:string) (b:string) (skipAfter:HashSet<string>) =
        countDfs (nextS skipAfter) b a
   
    let combinations = [
        ["svr";"fft";"dac";"out"]
        ["svr";"dac";"fft";"out"]
    ]

    let nums = 
        combinations
        |> List.map (fun combo ->
            combo
            |> Seq.pairwise
            |> Seq.map (fun (a,b) -> 
                let hs = HashSet([b;"out"] )
                let counted = countPaths a b hs
                counted
            )
            |> Seq.reduce (*)
        )

    let res = nums |> List.sum
    ClipboardService.SetText(res.ToString())
    res
