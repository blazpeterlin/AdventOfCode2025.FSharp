module Day08

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
    
    let coords = lns |>List.map (grabNumsGeneric int64) |> List.map (fun (x::y::z::[]) -> x,y,z)

    let dists0 = 
        List.allPairs coords coords
        |> List.filter (fun (a,b) -> a<>b)
        |> List.map (fun (a,b) -> (List.findIndex ((=)a) coords), List.findIndex ((=)b) coords, euclidean3sq a b)
        |> List.filter (fun (a,b,_)-> a<b)
        |> List.sortBy (fun (a,b,d) -> d)

    let circuits0 : int list list = seq { for n in 0..(coords.Length-1) do yield [n] } |> List.ofSeq

    let (circuitsN, distsN) = 
        [1..1000]
        |> Seq.fold (
            fun (circuitsI, distsI) _ -> 
                let (nextA, nextB,_) = List.head distsI
                let prevCircuitA = circuitsI |> List.find (fun lst -> lst |> List.contains nextA)
                let prevCircuitB = circuitsI |> List.find (fun lst -> lst |> List.contains nextB)
                let totalNewCircuit = 
                    prevCircuitA @ prevCircuitB
                    |> List.distinct

                let nextDists = 
                    distsI 
                    |> List.skip 1
                    //|> List.filter (
                    //    fun (da, db, _) -> 
                    //        not (List.contains da totalNewCircuit
                    //            &&
                    //            List.contains db totalNewCircuit
                    //        )
                    //)
                let nextCircuits = 
                    circuitsI 
                    |> List.except ([prevCircuitA;prevCircuitB])
                    |> List.append [totalNewCircuit]


                nextCircuits,nextDists
            
        ) (circuits0, dists0)

    let largestCircuits = circuitsN |> List.sortByDescending (List.length) |> List.take 3




    let res = largestCircuits |> List.map List.length |> List.fold (*) 1
    ClipboardService.SetText(res.ToString())
    res
 

let solve2 (inputPath: string) =
    let lns = File.ReadAllLines(dir + "/" + inputPath) |> Seq.filter((<>)"") |> Seq.toList;

    let coords = lns |>List.map (grabNumsGeneric int64) |> List.map (fun (x::y::z::[]) -> x,y,z)

    let dists0 = 
        List.allPairs coords coords
        |> List.filter (fun (a,b) -> a<>b)
        |> List.map (fun (a,b) -> (List.findIndex ((=)a) coords), List.findIndex ((=)b) coords, euclidean3sq a b)
        |> List.filter (fun (a,b,_)-> a<b)
        |> List.sortBy (fun (a,b,d) -> d)

    let circuits0 : int list list = seq { for n in 0..(coords.Length-1) do yield [n] } |> List.ofSeq

    let (circuitsN, distsN) = 
        [1..(circuits0.Length-2)]
        |> Seq.fold (
            fun (circuitsI, distsI) _ -> 
                let (nextA, nextB,_) = List.head distsI
                let prevCircuitA = circuitsI |> List.find (fun lst -> lst |> List.contains nextA)
                let prevCircuitB = circuitsI |> List.find (fun lst -> lst |> List.contains nextB)
                let totalNewCircuit = 
                    prevCircuitA @ prevCircuitB
                    |> List.distinct

                let nextDists = 
                    distsI 
                    //|> List.skip 1
                    |> List.filter (
                        fun (da, db, _) -> 
                            not (List.contains da totalNewCircuit
                                &&
                                List.contains db totalNewCircuit
                            )
                    )
                let nextCircuits = 
                    circuitsI 
                    |> List.except ([prevCircuitA;prevCircuitB])
                    |> List.append [totalNewCircuit]


                nextCircuits,nextDists
            
        ) (circuits0, dists0)


    let (idxA,idxB,_) = distsN |> List.head
    let (ax,ay,az)= coords[idxA]
    let (bx,by,bz) = coords[idxB]



    let res = ax*bx
    ClipboardService.SetText(res.ToString())
    res
