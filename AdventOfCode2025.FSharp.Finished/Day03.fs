module Day03

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
        

let largest (nums: int list) =
    
    let possibilities = 
        seq {
            for i in [0 .. nums.Length - 2] do
                for j in [i+1 .. nums.Length - 1 ] do
                    yield nums[i]*10+nums[j]
        }

    possibilities |> Seq.max

let solve1 (inputPath: string) =
    let lns = File.ReadAllLines(dir + "/" + inputPath) |> Seq.filter((<>)"") |> Seq.toList;
    
    let nums = lns |> List.map (fun ln -> ln.ToCharArray() |> List.ofSeq |> fun chars -> chars |> List.map (fun ch -> ch.ToString() |> int))

    let largests = nums |>List.map largest



    let res = largests |> List.sum
    ClipboardService.SetText(res.ToString())
    res
 

let dict = Dictionary<int64 list * int,int64>()
//let dict = System.Collections.Dictionary<(int * int64 list), int64>()

let rec largestN N (nums: int64 list) =
    if dict.ContainsKey(nums,N) then dict[nums,N] else

    if N = 1 then nums |> Seq.max else

    let multiplier = seq { 1L .. int64(N-1) } |> Seq.map (fun x -> 10L) |> Seq.fold (*) 1L
    
    let possibilities = 
        seq {
            for i in [0 .. nums.Length - N] do
                let sub = largestN (N-1) (nums |>List.skip (i+1))
                yield nums[i]*(multiplier)+sub
        }

    let res = possibilities |> Seq.max
    dict[(nums,N)] <- res
    res

let solve2 (inputPath: string) =
    let lns = File.ReadAllLines(dir + "/" + inputPath) |> Seq.filter((<>)"") |> Seq.toList;

    let nums = lns |> List.map (fun ln -> ln.ToCharArray() |> List.ofSeq |> fun chars -> chars |> List.map (fun ch -> ch.ToString() |> int64))

    let largests = nums |>List.map (largestN 12)



    let res = largests |> List.sum
    ClipboardService.SetText(res.ToString())
    res
