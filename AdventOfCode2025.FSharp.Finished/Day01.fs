module Day01

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

    let inps = lns |> List.map (fun ln -> ln[0], Int32.Parse(ln.Substring(1)))

    let d0 = 50

    let dF = inps |> List.scan (fun d (ch,delta) -> (d + (if ch='L' then -delta else delta) + 100)%100) d0

    let allZeros = dF |> List.filter (fun x -> x=0) |> List.length





    let res = allZeros
    ClipboardService.SetText(res.ToString())
    res
 

let solve2 (inputPath: string) =
    let lns = File.ReadAllLines(dir + "/" + inputPath) |> Seq.filter((<>)"") |> Seq.toList;

    let inps = lns |> List.map (fun ln -> ln[0], Int32.Parse(ln.Substring(1)))
    let fixedInps = 
        inps 
        |> List.map (fun (ch,num) -> 
            let sq = 
                seq {
                    let mutable n = num
                    while n >= 98 do
                        yield ch,98
                        n <- n-98
                    if n>0 then yield ch,n
                }
            sq |> List.ofSeq
        )
        |> List.concat

    let d0 = 50

    let dF = 
        fixedInps 
        |> List.scan (fun (d,passed) (ch,delta) -> 
                let resultNum = (d + (if ch='L' then -delta else delta) + 100)%100
                let passed = 
                    if d=0 then false else
                    if ch='L' && delta>=d then true else
                    if ch='R' && d+delta>=100 then true else
                        false
                resultNum, passed
                )
                (d0, false)

    let allZerosPassed = dF |> List.filter (fun (_,passed) -> passed) |> List.length




    // not 5100

    let res = allZerosPassed
    ClipboardService.SetText(res.ToString())
    res
