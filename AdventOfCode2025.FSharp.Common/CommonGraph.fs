module CommonGraph

open System.Collections.Generic




let parseGrid lns char2val = 
    lns
    |> List.mapi (fun y line ->
        line
        |> Seq.mapi (fun x char -> (x, y), char2val char)
        |> Seq.toList)
    |> List.concat
    |> Map.ofList

let (|Default|) defaultValue input =
    defaultArg input defaultValue

let AStarSeqInner (pos0:'a) (heuristic: 'a->int) (isFinish:'a->bool) (getNextPos:'a->('a*int) seq) (haveHistory:bool) =
    let pq = PriorityQueue() // mutable
    let h0 = heuristic pos0
    pq.Enqueue((pos0,[pos0],0), 0+h0)
    let v = [] |> HashSet // mutable
    
    let r =
        seq {
            while pq.Count>0 do
                let currPos,history,costSoFar = pq.Dequeue()
                if v.Add currPos then
                    if isFinish currPos then yield (history,costSoFar)
                    let allNextPos = currPos |> getNextPos// |> Seq.filter (fun (pos,_) -> v.Contains pos |> not)
                    pq.EnqueueRange (allNextPos 
                                        |> Seq.map (fun (pos,cost) -> 
                                                        let h = heuristic pos
                                                        let newHistory = if haveHistory then pos::history else []
                                                        (pos,newHistory,costSoFar+cost),costSoFar+cost+h))
        }
        |> seq
    r


let AStarSeq (pos0:'a) (heuristic: 'a->int) (isFinish:'a->bool) (getNextPos:'a->('a*int) seq) =
    AStarSeqInner pos0 heuristic isFinish getNextPos true
let AStarSeqNoHistory (pos0:'a) (heuristic: 'a->int) (isFinish:'a->bool) (getNextPos:'a->('a*int) seq) =
    AStarSeqInner pos0 heuristic isFinish getNextPos false
