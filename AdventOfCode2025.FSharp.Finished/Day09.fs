module Day09

open Common
open System
open System.Collections.Generic
open Microsoft.FSharp.Core.Operators.Checked
open Common.Operators
open System.IO
open TextCopy
open NetTopologySuite.Geometries
open NetTopologySuite.Operation.Overlay

let dir = __SOURCE_DIRECTORY__

//type Present = { X: int; Y: int; Z: int; }

//let parseLn (ln:string) =
//    match ln with
//    | Regex @"^(\w+) blabla (\d+)$" [a; Int b] ->
//        (a,b)
        


let solve1 (inputPath: string) =
    let lns = File.ReadAllLines(dir + "/" + inputPath) |> Seq.filter((<>)"") |> Seq.toList;
    let nums = lns |> List.map (grabNumsGeneric int64) |> List.map (fun (x::y::[]) -> x,y)
    
    let grd = 
        lns
        |> List.map (grabNumsGeneric int64)
        |> List.map (fun (x::y::[]) -> (x,y),true)
        |> Map.ofList

    let rects = 
        List.allPairs nums nums
        |> List.map (fun ((x1,y1),(x2,y2)) -> abs(y2-y1+1L)*abs(x2-x1+1L))

    let largest = rects |> List.sortByDescending id |> List.head


    let res = largest
    ClipboardService.SetText(res.ToString())
    res
 

let allPosBetween (x1:int64,y1:int64) (x2,y2) =

    let d = if x2>x1 then (1L,0L) elif x2 < x1 then (-1,0) elif y2>y1 then (0,1) else (0,-1)

    seq {
        let mutable p = (x1,y1)
        yield p
        while p <> (x2,y2) do
            p <- p ++.. d
            yield p
    }
    |> List.ofSeq


let deltaStep  (x1:int64,y1:int64) (x2,y2) =
    if x2>x1 then (1L,0L) elif x2 < x1 then (-1,0) elif y2>y1 then (0,1) else (0,-1)

let getVerticalRange (x1:int64,y1:int64) (x2,y2) =

    if x1<>x2 then failwith "huh" else
    if y1 < y2 
    then (y1,y2)
    else (y2,y1)


let upwards prev2 prev1 (x1:int64,y1:int64) (x2,y2) =

    let d = deltaStep (x1,y1) (x2,y2)
    let dPrev = deltaStep prev2 prev1

    if (snd d)<>0 then [] else


    let r = 
        seq {
            let mutable p = (x1,y1)
            if (snd dPrev)<>0 then failwith "huh"
            if d<>dPrev then yield p
                //yield (if (snd prev1 < y1 then (x2,y2) else (x1,y1))
            
            while p <> ((x2,y2)--..d) do
                p <- p ++.. d
                yield p

            yield (x2,y2)
        }
        |> List.ofSeq 
    r

let constructPolygon pts =
    let ps = pts |> List.map (fun (x,y) -> Coordinate(float x, float y))
    new NetTopologySuite.Geometries.Polygon(
        shell=LinearRing(points = (ps@[ps.Head]|>Array.ofList)),
        holes=null
    )

let solve2 (inputPath: string) =
    let lns = File.ReadAllLines(dir + "/" + inputPath) |> Seq.filter((<>)"") |> Seq.toList;

    let listPos0 = lns |> List.map (grabNumsGeneric int64) |> List.map (fun (x::y::[]) -> x,y)
    let listPos = listPos0@[listPos0.Head]
    let circumference = 
        listPos 
        |> List.pairwise
        |> List.map (fun (p1,p2) -> allPosBetween p1 p2)
        |> List.concat
        |> List.distinct
        |> HashSet

    let mainPolygon = constructPolygon listPos0

    let listAllAbove = 
        ((listPos0 |> List.rev |> List.take 2 |> List.rev)@listPos0@[listPos0[0]])
        |> List.windowed 4
        |> List.map (fun (p0::p1::p2::p3::[]) -> upwards p0 p1 p2 p3)
        |> Seq.concat
        |> List.ofSeq


    let areaVerticalRangesByX = 
        listAllAbove
        |> Seq.groupBy fst
        |> Seq.map (fun (x,pos) -> 
            let vRanges = 
                pos 
                |> Seq.sortByDescending snd
                |> Seq.chunkBySize 2
                |> Seq.map (fun chnk -> 
                    if chnk.Length < 2 then failwith "huh"
                    let p1::p2::[] = chnk |> List.ofArray
                    getVerticalRange p1 p2
                )
                |> List.ofSeq
            (x, vRanges)
        )
        |> Map.ofSeq

    //CommonPrint.print2d (greenRed |> List.map (fun (a,b) -> int a, int b ))

    let mutable memory = [(-1L,-1L),false] |> Map.ofList
    let isAccepted (x,y) = 
        if memory.ContainsKey(x,y) then memory[x,y] else
        if circumference.Contains(x,y) 
        then
            memory <- memory |> Map.add (x,y) true
            true 
        else
        if not(areaVerticalRangesByX.ContainsKey(x)) 
        then 
            memory <- memory |> Map.add (x,y) false
            false 
        else
        let rngs = areaVerticalRangesByX[x]
        let res = rngs |> List.exists (fun (a,b) -> y >= a && y <= b)
        memory <- memory |> Map.add (x,y) res
        res
    
    let yEvents = listPos0 |> List.map snd |> List.map (fun y -> y::y+1L::[]) |> List.concat |> List.distinct |> HashSet
    let xEvents = listPos0 |> List.map fst |> List.map (fun y -> y::y+1L::[]) |> List.concat |> List.distinct |> HashSet

    let mutable numsSoFar = 0
    let total = listPos0.Length * listPos0.Length / 2



    let goodRects = 

        listPos0
        |> List.map (fun (x1,y1) ->
            let mutable blockedPos : (int64*int64) list = []

            listPos0 
            |> List.filter (fun (x2,y2) -> 
                if x2 < x1 then false else
                if x2=x1 && y2<y1 then false else
                numsSoFar <- numsSoFar + 1

                if 
                    blockedPos 
                    |> List.exists (fun (bx,by) -> 
                        by > y1 && bx <= x2 && by <= y2
                        ||
                        by < y1 && bx <= x2 && by >= y2
                    )
                then false
                else


                ////// before (by hand)

                let xsmol,xbig = if x1<x2 then x1,x2 else x2,x1
                let ysmol,ybig = if y1<y2 then y1,y2 else y2,y1
                let rectPos = 
                    seq { 
                        for x in xEvents do
                            if x < xsmol ||x > xbig then ignore else
                            for y in yEvents do
                                if y < ysmol || y >ybig then ignore else
                                yield x,y
                    }
                let goodRect = rectPos |> Seq.forall isAccepted
                if not (goodRect) then blockedPos <- (x2,y2)::blockedPos
                goodRect



                //////// after (polygon suite)
                //let rectPolygon = constructPolygon [x1,y1;x1,y2;x2,y2;x2,y1]
                
                //let intersected = rectPolygon.Intersection(mainPolygon)
                //intersected.Area = rectPolygon.Area

            )
            |> List.map (fun p2 -> (x1,y1),p2)
        )
        |> List.concat

    let rectScores =
        goodRects
        |> List.map (fun ((x1,y1),(x2,y2)) -> (x1,y1),(x2,y2),(abs(y2-y1)+1L)*(abs(x2-x1)+1L))

    let a,b,cost= rectScores |> List.sortByDescending (fun (a,b,c) -> c) |> List.head

    let res = cost

    ClipboardService.SetText(res.ToString())
    res
    
