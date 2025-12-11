module Day11_Specific

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
    
    let moves = 
        lns
        |> List.map (fun ln -> 
            match (ln.Split([|':';' '|],StringSplitOptions.RemoveEmptyEntries) |> List.ofArray) with
            | head::lst -> head,lst
           )

        |> Map.ofList

    let nextS (s: string list) =
        let h = s.Head
        if not(moves.ContainsKey(h)) then Seq.empty else
        seq {
            for next in moves[h] do
                yield (next::s),1
        }

        

    let st0 = ["you"]

    let allPaths =
        CommonGraph.AStarSeq st0 (fun x -> 0) (fun x -> x.Head="out") nextS


    let res = allPaths |> Seq.length
    ClipboardService.SetText(res.ToString())
    res
 

let solve2 (inputPath: string) =
    let lns = File.ReadAllLines(dir + "/" + inputPath) |> Seq.filter((<>)"") |> Seq.toList;


    let movesLst = 
        lns
        |> List.map (fun ln -> 
            match (ln.Split([|':';' '|],StringSplitOptions.RemoveEmptyEntries) |> List.ofArray) with
            | head::lst -> head,lst
           )


    let sameMoves = 
        movesLst 
        |> List.groupBy snd 
        |> List.map (fun (movesTo, lst) ->
            lst |> List.map fst
        )
        |> List.append [["out"]]

    let moves = 
        movesLst 
        |> List.map (fun (a,bs) ->
            let countedMoves = 
                sameMoves
                |> List.map (fun sm -> 
                    let sameBs = sm |> List.filter (fun smElt -> bs |> List.contains smElt)
                    if sameBs.Length=0 then [] else
                    [sameBs.Head, int64 sameBs.Length]
                )
                |> List.concat

            a, countedMoves
        )
        |> Map.ofList

    let forGraphViz = 
        movesLst
        |> List.map (fun (a,bs) -> bs |> List.map (fun b -> a,b))
        |> List.concat
        |> List.fold (fun str (a,b) -> str+Environment.NewLine+a+ " -> " + b) ""


    let nextS (filteredTgt:HashSet<string>) (s: (string*int64) list)  =
        let h,multiplierSoFar = s.Head
        if filteredTgt.Contains(h) then Seq.empty else
        if not(moves.ContainsKey(h)) then Seq.empty else
        seq {
            for (nextS,multiplier) in moves[h] do
                yield ((nextS,multiplierSoFar*multiplier)::s),1
        }


    let allPathsGeneric a b skipAfter =
        let r = CommonGraph.AStarSeq [(a,1L)] (fun x -> 0) (fun x -> fst(x.Head)=b) (nextS skipAfter)
        //let huh = r |> List.ofSeq
        r |> Seq.map (fun ((h)::_,c) -> (h.Head |> snd)) |> Seq.sum

    // stage 1: svr
    // stage 2 SKIP: bbm fjk jus qiv izh
    // stage 3: huq vcp eru kin urx
    // -------  fft here
    // stage 4: tio jyv mqi
    // stage 5: jgb ndb tyj gaj uis
    // stage 6: dzv eyw mxy
    // ------- dac here
    // stage 7: you qlu icu fji ako
    // stage 8: OUT

    let stages = [
        ["svr"];
        [(*"huq";*)"vcp";"eru";"kin";"urx"];
        ["fft"];
        ["tio";"jyv";"mqi"];
        ["jgb";"ndb";"tyj";"gaj";"uis"];
        ["dzv";"eyw";"mxy"];
        ["dac"];
        ["you";"qlu";"icu";"fji";"ako"];
        ["out"];
    ]
    

    let allPerm_svr_fft = 
        let skip1 = stages[2]@stages[3] |> HashSet
        let skip2 = stages[3]@stages[4] |>HashSet
        seq {
        for x1 in stages[1] do
            let sf_0_1 = allPathsGeneric "svr" x1 skip1
            let sf_1_2 = allPathsGeneric x1 "fft" skip2
            yield sf_0_1*sf_1_2
        } |>Seq.sum

    let allPerm_fft_dac = 
        seq {
            let skip_2_3 = stages[3] |> HashSet
            let skip_3_4 = stages[4] |> HashSet
            let skip_4_5 = stages[5] |> HashSet
            let skip_5_6 = stages[6]@stages[7] |> HashSet
            for s3 in stages[3] do
                let sf_2_3 = allPathsGeneric "fft" s3 skip_2_3
                for s4 in stages[4] do
                    let sf_3_4 = allPathsGeneric s3 s4 skip_3_4
                    for s5 in stages[5] do
                        let sf_4_5 = allPathsGeneric s4 s5 skip_4_5
                        let sf_5_6 = allPathsGeneric s5 "dac" skip_5_6
                        yield sf_2_3 * sf_3_4 * sf_4_5 * sf_5_6
        }
        |> Seq.sum

    let allPerm_dac_out =
        seq {
            let skip_6_7 = stages[7] |> HashSet
            let skip_7_8 = stages[8] |> HashSet
            for s7 in stages[7] do
                let sf_6_7 =  allPathsGeneric "dac" s7 skip_6_7
                let sf_7_8 =  allPathsGeneric s7 "out" skip_7_8
                yield sf_6_7 * sf_7_8
        }
        |> Seq.sum

    let res = allPerm_svr_fft * allPerm_fft_dac * allPerm_dac_out
    ClipboardService.SetText(res.ToString())
    res
