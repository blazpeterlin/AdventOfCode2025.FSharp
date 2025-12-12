module Day12

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
    let lns = File.ReadAllLines(dir + "/" + inputPath) |> Seq.toList;
    
    let presentLns = lns |> List.takeWhile (fun ln -> ln.IndexOf("x")=(-1))
    let reqLns = lns |> List.skip (presentLns.Length) |> List.except([""])

    let presents = 
        presentLns
        |> List.chunkBySize 5
        |> List.mapi (fun i chnk ->

            let img = 
                chnk |> List.skip 1 |> List.filter ((<>)"")
                |> List.mapi (fun j ln -> j,ln.ToCharArray()|>List.ofArray|>List.map(fun ch -> ch='#'))
            i,img
        )
        |> Map.ofList

    let reqs = 
        reqLns 
        |> List.map (fun ln ->
            let (lnSize::lnNums::[])= (ln.Split(":")|> List.ofArray)
            let w::h::[]=lnSize |> grabNums
            let reqCount = lnNums |> grabNums
            (w,h),reqCount
        )

    let possibles = 
        reqs
        |> List.filter (fun ((w,h),req)->
            let size = w*h
            let blocks = 
                req
                |> List.mapi (fun idx r ->
                    let p = presents[idx]
                    let fillings = p |> List.map snd |> List.concat |> List.filter id |> List.length
                    fillings * r
                )
            let blockSize = blocks |> List.sum

            if blockSize <= size
            then true
            else false

        )


    let res = 0
    ClipboardService.SetText(res.ToString())
    res
 

let solve2 (inputPath: string) =
    let lns = File.ReadAllLines(dir + "/" + inputPath) |> Seq.filter((<>)"") |> Seq.toList;




    let res = 0
    ClipboardService.SetText(res.ToString())
    res
