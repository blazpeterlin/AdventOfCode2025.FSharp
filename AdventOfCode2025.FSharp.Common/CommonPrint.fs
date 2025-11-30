module CommonPrint

open System

let print2d (fullCoords: (int*int) seq) =
     let printed = fullCoords |> Seq.sortBy (fun pos -> snd pos,fst pos) |> List.ofSeq
     let fstX,fstY = printed |> List.head
     let minX = printed |> List.map fst |>List.min
     let mutable prevY = fstY
     let mutable prevX = minX-1
     for (x,y) in printed do
         while prevY <> y do
             Console.WriteLine(); 
             prevX <- minX-1;
             prevY <- prevY + 1
             0 |> ignore
         
         for ix in [prevX+2 .. x] do Console.Write(' ')
         Console.Write("█")
         prevY <- y
         prevX <- x
