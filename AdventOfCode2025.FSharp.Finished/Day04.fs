module Day04

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
        

let adjacents (x,y) =
    [
        x+1,y;
        x+1,y-1;
        x,y-1;
        x-1,y-1;
        x-1,y;
        x-1,y+1;
        x,y+1;
        x+1,y+1;
    ]
        


let solve1 (inputPath: string) =
    let lns = File.ReadAllLines(dir + "/" + inputPath) |> Seq.filter((<>)"") |> Seq.toList;
    
    let mp = 
        lns
        |> List.mapi (fun row line -> 
            line |> Seq.mapi (fun col ch -> ((row, col), ch)))
        |> Seq.collect id
        |> Map.ofSeq

    let rolls = mp.Keys |> Seq.filter (fun k -> mp[k]='@') |> List.ofSeq
    let goodRolls = 
        rolls 
        |> Seq.filter (
            fun p ->
                let adjs = (adjacents p) |> List.filter (fun p2 -> mp.ContainsKey(p2) && mp[p2]='@')
                adjs.Length < 4
        )   
        |> List.ofSeq



    let res = goodRolls.Length
    ClipboardService.SetText(res.ToString())
    res
 

let step (mp:Map<int*int,char>) =

    let rolls = mp.Keys |> Seq.filter (fun k -> mp[k]='@') |> List.ofSeq
    let goodRolls = 
        rolls 
        |> Seq.filter (
            fun p ->
                let adjs = (adjacents p) |> List.filter (fun p2 -> mp.ContainsKey(p2) && mp[p2]='@')
                adjs.Length < 4
        )   
        |> List.ofSeq

    let nextMp = mp |> Map.map (
        fun k _ -> 
            let contains = goodRolls |> List.contains k
            if contains then '.' else mp[k]
    )

    if nextMp = mp then None else Some(nextMp, nextMp)


let solve2 (inputPath: string) =
    let lns = File.ReadAllLines(dir + "/" + inputPath) |> Seq.filter((<>)"") |> Seq.toList;

    let mp = 
        lns
        |> List.mapi (fun row line -> 
            line |> Seq.mapi (fun col ch -> ((row, col), ch)))
        |> Seq.collect id
        |> Map.ofSeq


    let lastMp = mp |> Seq.unfold step |> Seq.last

    let countRolls (m:Map<int*int,char>) = m.Values |> Seq.filter ((=)'@') |>Seq.length

    let diffPaper = (countRolls mp) - (countRolls lastMp)



    let res = diffPaper
    ClipboardService.SetText(res.ToString())
    res
