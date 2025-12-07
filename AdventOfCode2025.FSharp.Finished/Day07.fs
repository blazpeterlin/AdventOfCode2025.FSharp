module Day07

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
    
    let grid = 
        lns 
        |> List.mapi (fun y line -> 
            line.ToCharArray() 
            |> Array.mapi (fun x ch -> ((x, y), ch)))
        |> List.collect Array.toList
        |> Map.ofList

    let start = grid |> Map.findKey (fun k v -> v = 'S')
    let dir = (0,1)

    let mutable pos = ([start],[])
    let maxY = grid.Keys |> Seq.map snd |> Seq.max

    while ((snd (fst pos).Head) <= maxY) do
        pos <- (fst pos |> List.map ((+..) dir), snd pos)
        pos <- 
            (fst pos)
            |> List.map (fun (x,y) -> if grid.ContainsKey(x,y) && grid[x,y]='^' then [x-1,y;x+1,y] else [x,y])
            |> List.concat
            |> List.distinct
            , snd pos |> List.append (fst pos |> List.filter (fun (x,y) -> grid.ContainsKey(x,y) && grid[x,y]='^'))


    let res = (snd pos).Length
    ClipboardService.SetText(res.ToString())
    res
 

let solve2 (inputPath: string) =
    let lns = File.ReadAllLines(dir + "/" + inputPath) |> Seq.filter((<>)"") |> Seq.toList;

    let grid = 
        lns 
        |> List.mapi (fun y line -> 
            line.ToCharArray() 
            |> Array.mapi (fun x ch -> ((x, y), ch)))
        |> List.collect Array.toList
        |> Map.ofList

    let start = grid |> Map.findKey (fun k v -> v = 'S')
    let dir = (0,1)

    let mutable pos = ([start,1L],[])
    let maxY = grid.Keys |> Seq.map snd |> Seq.max

    while (pos |> fst |> fun p -> p.Head |> fst |>snd) <= maxY do
        pos <- (fst pos |> List.map (fun (p,c) -> p+..dir,c), snd pos)
        pos <- 
            (fst pos)
            |> List.map (fun ((x,y),c) -> if grid.ContainsKey(x,y) && grid[x,y]='^' then [(x-1,y),c;(x+1,y),c] else [(x,y),c])
            |> List.concat
            |> List.groupBy (fun (p,c) -> p)
            |> List.map(fun (k,lst) -> k, (lst |> List.map snd |>List.sum))
            , snd pos |> List.append (fst pos |> List.filter (fun ((x,y),c) -> grid.ContainsKey(x,y) && grid[x,y]='^') |> List.map snd)


    let res = (snd pos) |> List.sum |> (+)1L
    ClipboardService.SetText(res.ToString())
    res
