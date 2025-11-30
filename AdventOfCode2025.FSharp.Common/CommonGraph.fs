module CommonGraph

open System.Collections.Generic

 let AStarSeq (pos0:'a) (heuristic: 'a->int) (isFinish:'a->bool) (getNextPos:'a->('a*int) seq) =
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
                                                            (pos,pos::history,costSoFar+cost),costSoFar+cost+h))
            }
            |> seq
        r
