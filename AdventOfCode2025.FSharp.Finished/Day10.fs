module Day10

open Common
open System
open System.Collections.Generic
open Microsoft.FSharp.Core.Operators.Checked
open Common.Operators
open System.IO
open TextCopy
open Microsoft.Z3
open Microsoft.Z3.Int
open Microsoft.Z3.Addons

let dir = __SOURCE_DIRECTORY__

type Machine = { NumStates: int; FinalState: bool list; Togglables: int list list; FinalStatePart2: int list; }
type StatePart1 = { LightsPart1: bool list; }
type StatePart2 = { LightsPart2: int list; }

let getMovesPart1 (m:Machine) (s:StatePart1) =
    seq {
        for t in m.Togglables do
            let nextS = { s with LightsPart1 = s.LightsPart1 |> List.mapi(fun idx b -> if (t |> List.contains idx) then not(b) else b)}
            yield nextS,1
    }
let getMovesPart2 (m:Machine) (s:StatePart2) =
    //let isInfeasible = 
    //    List.zip s.LightsPart2 m.FinalStatePart2
    //    |> List.exists (fun (a , b) -> a>b)
    //if isInfeasible then Seq.empty else

    seq {
        for t in m.Togglables do
            let nextS = { s with LightsPart2 = s.LightsPart2 |> List.mapi(fun idx voltage -> if (t |> List.contains idx) then voltage+1 else voltage)}
            yield nextS,1
    }


let solve1 (inputPath: string) =
    let lns = File.ReadAllLines(dir + "/" + inputPath) |> Seq.filter((<>)"") |> Seq.toList;
    
    let ms: Machine list = 
        lns
        |> List.map (fun ln ->
            let finalState = (ln.Split([|'[';']'|])[1]).ToCharArray() |> Seq.map(fun ch -> ch='#') |> List.ofSeq
            let numStates = finalState.Length
            let numStr = ln.Split([|']';'{';'}'|])[1]
            let blockStr = ln.Split([|'{';'}'|])[1]

            let togglables = 
                numStr.Split(")")
                |> Seq.filter (fun x -> x.Trim()<>"")
                |> Seq.map (fun x -> (x.Split("(")[1]).Split(",") |> Seq.map int |> List.ofSeq)
                |> List.ofSeq

            { FinalState = finalState; NumStates = numStates; Togglables = togglables; FinalStatePart2=[] }
        )
    let numPresses = 
        ms
        |> List.map (fun m ->
            let ls = seq  { for x in [1..m.NumStates] do yield false } |> List.ofSeq
            let st0 = { LightsPart1 = ls }
            let (leastPath, leastCost) = 
                CommonGraph.AStarSeq st0 (fun x -> 0) (fun x -> x.LightsPart1=m.FinalState) (getMovesPart1 m)
                |> Seq.head
            leastCost
        )





    let res = numPresses |> List.sum
    ClipboardService.SetText(res.ToString())
    res
 

let solve2 (inputPath: string) =
    let lns = File.ReadAllLines(dir + "/" + inputPath) |> Seq.filter((<>)"") |> Seq.toList;

    let ms: Machine list = 
        lns
        |> List.map (fun ln ->
            let finalState = (ln.Split([|'[';']'|])[1]).ToCharArray() |> Seq.map(fun ch -> ch='#') |> List.ofSeq
            let numStates = finalState.Length
            let numStr = ln.Split([|']';'{';'}'|])[1]
            let blockStr = ln.Split([|'{';'}'|])[1]
            let finalStatePart2 = blockStr |> grabNums

            let togglables = 
                numStr.Split(")")
                |> Seq.filter (fun x -> x.Trim()<>"")
                |> Seq.map (fun x -> (x.Split("(")[1]).Split(",") |> Seq.map int |> List.ofSeq)
                |> List.ofSeq

            { FinalState = finalState; NumStates = numStates; Togglables = togglables; FinalStatePart2=finalStatePart2 }
        )
    let numPresses = 
        ms
        |> List.map (fun m ->
            let ls = seq  { for x in [1..m.NumStates] do yield 0 } |> List.ofSeq
            //let h (sp: StatePart2) =
            //    List.zip m.FinalStatePart2 sp.LightsPart2
            //    |> List.map (fun (final ,soFar) -> if final<soFar then 9999999 else final-soFar) |> List.sum
            //let st0 = { LightsPart2 = ls }
            //let (leastPath, leastCost) = 
            //    CommonGraph.AStarSeq st0 h (fun x -> x.LightsPart2=m.FinalStatePart2) (getMovesPart2 m)
            //    |> Seq.head

            let opt = Opt()
            let ZERO = IntVal 0
            let objectives = ls |> List.mapi (fun i x -> Int ("obj"+i.ToString()))
            let ts = m.Togglables |> List.indexed |> List.map (fun (idx,tgl) -> Int ("tgl"+idx.ToString()))
            for (idx, tgl) in m.Togglables |> List.indexed do
                let T = ts[idx]
                for (numIdx, t) in tgl |> List.indexed do
                    opt.Add(T >=. ZERO)
            for (objIdx, obj) in objectives|>List.indexed do
                let objToggles = 
                    List.zip m.Togglables ts
                    |> List.filter (fun (tgl,T) -> tgl |> List.contains objIdx)
                    |> List.map snd
                let summedT = objToggles |> List.reduce (+)
                opt.Add(obj =. summedT)

            for (v,finalState) in (List.zip objectives m.FinalStatePart2) do opt.Add(v =. (IntVal finalState))

            let summedAllT = ts |> List.reduce (+)
            opt.Minimize(summedAllT)
            opt.CheckOrFail()
            let resOpt = opt.Eval summedAllT
            let cost = resOpt.ToString() |> int64


            //leastCost
            cost
        )





    let res = numPresses |> List.sum
    ClipboardService.SetText(res.ToString())
    res
