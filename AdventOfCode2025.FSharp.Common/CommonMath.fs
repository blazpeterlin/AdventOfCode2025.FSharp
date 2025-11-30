module CommonMath
    
    let isPrime a =
        match a with
        | a when a < 2 -> false
        | a ->
            let divisors = seq { 2 .. int (sqrt (float a)) }
            not <| Seq.exists (fun d -> a % d = 0) divisors

    let rec gcd x y =
        if y = 0 then x
        else gcd y (x % y)

    let lcm a b = a*b/(gcd a b)

    let modularInverse modulo num = 
        let m = modulo
        let a = num
        let rec eea t t' r r' =
            match r' with
            | 0 -> t
            | _ -> 
                let div = r/r'
                eea t' (t - div * t') r' (r - div * r')
        (m + eea 0 1 m a) % m
    
    let chineseRemainderTheorem remainders modulos =
        let n = remainders
        let g = modulos
        match Seq.fold(fun n g->if (gcd n g)=1 then n*g else 0) 1 g with
        |0 -> None
        |fN-> Some ((Seq.fold2(fun n i g -> n+i*(fN/g)*(modularInverse g ((fN/g)%g))) 0 n g)%fN)


    
    let rec generateCombinationsNonRepeating (eltList : 'T list) : 'T list seq =
        match eltList with
        | [x] -> [[x]]
        | x -> 
            [0..x.Length-1]
            |> Seq.map (fun idx ->
                let elt = eltList[idx]
                let innerChList = eltList |> List.except [elt]
                let r = 
                    generateCombinationsNonRepeating innerChList
                    |> Seq.map (fun arr -> elt::arr)
                r
            )
            |> Seq.concat

    let generateCombinationsRepeating (eltList : 'T list) : 'T list seq =
        let rec generateCombinationsRepeatingInner (eltsRemaining:int) : 'T list seq =
            match eltsRemaining with
            | 1 -> eltList |> Seq.map (fun elt -> [elt])
            | _ -> 
                [0..eltList.Length-1]
                |> Seq.map (fun idx ->
                    let elt = eltList[idx]
                    let r = 
                        generateCombinationsRepeatingInner (eltsRemaining-1)
                        |> Seq.map (fun arr -> elt::arr)
                    r
                )
                |> Seq.concat
        generateCombinationsRepeatingInner (eltList.Length)