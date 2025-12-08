module Common

open System
open System.Collections.Generic
open Microsoft.FSharp.Core.Operators.Checked
open System.IO
open System.Security.Cryptography
open System.Text
open System.Text.RegularExpressions

let grabNumsGeneric (modifier:string->'a) (str:string) : 'a list =
    let nonNums = str.ToCharArray() |>Array.filter (fun ch -> not(Char.IsDigit(ch)) && ch <> '-') |>Array.distinct
    str.Split(nonNums, StringSplitOptions.RemoveEmptyEntries) |> Seq.map modifier |> List.ofSeq
    
let grabNums (str:string) : int list =
    let nonNums = str.ToCharArray() |>Array.filter (fun ch -> not(Char.IsDigit(ch)) && ch <> '-') |>Array.distinct
    str.Split(nonNums, StringSplitOptions.RemoveEmptyEntries) |> Seq.map int |> List.ofSeq
    
    
let manhattan (x0,y0) (x1,y1) = abs(x1-x0)+abs(y1-y0)
let manhattan3 (x0,y0,z0) (x1,y1,z1) = abs(x1-x0)+abs(y1-y0)+abs(z1-z0)
let euclidean3sq (x0:int64,y0:int64,z0:int64) (x1,y1,z1) = (x1-x0)*(x1-x0)+(y1-y0)*(y1-y0)+(z1-z0)*(z1-z0)

let asTuple2<'a> (lst: 'a list) : 'a*'a =
    match lst with
    | a::b::_ -> (a,b)
    | _ -> failwith ("not tuple2: list is len " + lst.Length.ToString())

let asTuple3<'a> (lst: 'a list) : 'a*'a*'a  =
    match lst with
    | a::b::c::_ -> (a,b,c)
    | _ -> failwith ("not tuple2: list is len " + lst.Length.ToString())
    
let asTuple4<'a> (lst: 'a list) : 'a*'a*'a*'a  =
    match lst with
    | a::b::c::d::_ -> (a,b,c,d)
    | _ -> failwith ("not tuple2: list is len " + lst.Length.ToString())
    
let asTuple5<'a> (lst: 'a list) : 'a*'a*'a*'a*'a  =
    match lst with
    | a::b::c::d::e::_ -> (a,b,c,d,e)
    | _ -> failwith ("not tuple2: list is len " + lst.Length.ToString())
    
let seqGroupBy (projKey:'T->'Key) (projVal:'T->'Val) (lst:'T seq) : ('Key * 'Val seq) seq =
       lst |> Seq.groupBy projKey |> Seq.map (fun (x,lst) -> x,lst |> Seq.map projVal)

let listGroupBy (projKey:'T->'Key) (projVal:'T->'Val) (lst:'T list) : ('Key * 'Val list) list =
        lst |> List.groupBy projKey |> List.map (fun (x,lst) -> x,lst |> List.map projVal)


// MD5 functionality
let computeMd5Hash (input: string) : string =
    use md5 = MD5.Create()
    let inputBytes = Encoding.UTF8.GetBytes(input)
    let hashBytes = md5.ComputeHash(inputBytes)
    
    hashBytes
    |> Array.map (fun b -> b.ToString("x2"))
    |> String.concat ""

let computeMd5HashBytes (input: string) : byte[] =
    use md5 = MD5.Create()
    let inputBytes = Encoding.UTF8.GetBytes(input)
    md5.ComputeHash(inputBytes)


let (|Regex|_|) pattern input =
   let m = Regex.Match(input, pattern)

   if m.Success then
       Some(List.tail [ for g in m.Groups -> g.Value ])
   else
       None


let RegexMulti pattern input =
   let ms = Regex.Matches(input, $"(?=({pattern}))") |> seq
   ms
   |> Seq.filter _.Success
   |> Seq.map (fun m -> List.tail [ for g in m.Groups -> g.Value ])

let RegexMultiIdx pattern input =
   let ms = Regex.Matches(input, $"(?=({pattern}))") |> seq
   ms
   |> Seq.filter _.Success
   |> Seq.map (fun m -> m.Index, List.tail [ for g in m.Groups -> g.Value ])


let (|Int|_|) (str: string) =
    let mutable intvalue = 0

    if System.Int32.TryParse(str, &intvalue) then
        Some(intvalue)
    else
        None

let (|Int64|_|) (str: string) =
    let mutable intvalue: int64 = 0

    if System.Int64.TryParse(str, &intvalue) then
        Some(intvalue)
    else
        None

        
let rec permutations list =
    match list with
    | [] -> [[]]
    | head :: tail ->
        permutations tail
        |> List.collect (fun perm ->
            [0..List.length perm]
            |> List.map (fun i ->
                let (before, after) = List.splitAt i perm
                before @ [head] @ after))

module Operators =
    let (+..) (x0,y0) (x1,y1) = (x0+x1,y0+y1)
    let (+...) (x0,y0,z0) (x1,y1,z1) = (x0+x1,y0+y1,z0+z1)
    let (-..) (x0,y0) (x1,y1) = (x0-x1,y0-y1)
    let (-...) (x0,y0,z0) (x1,y1,z1) = (x0-x1,y0-y1,z0-z1)
    let ( *..) (x,y) factor = (x*factor, y*factor)
    

    let inline map mapping list = List.map mapping list
    let inline filter predicate list = List.filter predicate list
    let inline sum list = List.sum list
    let inline ofArray arr = List.ofArray arr
    let inline ofSeq sq = List.ofSeq sq
    let inline distinct list = List.distinct list
    let inline length list = List.length list
    let inline windowed windowSize list = List.windowed windowSize list
    let inline fold (folder:'State -> 'T -> 'State) (state: 'State) (list: 'T list) = List.fold folder state list
    let inline head list = List.head list

