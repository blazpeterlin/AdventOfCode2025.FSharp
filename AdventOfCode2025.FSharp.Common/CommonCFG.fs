module CommonCFG
#nowarn 3391

type CFGVar = CFGVar of string
type CFGTerminal = CFGTerminal of string
type CFGSymbol = 
    | Var of CFGVar 
    | Terminal of CFGTerminal
    // Add implicit conversion operators
    static member op_Implicit(var: CFGVar) = Var(var)
    static member op_Implicit(terminal: CFGTerminal) = Terminal(terminal)

type CFGRule = { Origin: CFGVar; Productions: CFGSymbol list }



let toChomskyForm (rules: CFGRule list) =
    let vars = rules |> List.map _.Origin |> List.distinct

    let nextRules =
        seq {
            for var in vars do
                let (CFGVar varName) = var

                let mutable appendI = 0
                let mutable parentSymbol = var

                for rule in rules |> List.filter (fun r -> r.Origin = var) do
                    let mutable remainingProds = rule.Productions
                    while remainingProds.Length > 2 do
                        appendI <- appendI + 1

                        let prHead::prTail = remainingProds       
                        let nextSymbol = CFGVar(varName + "¤" + appendI.ToString())

                        yield { Origin= parentSymbol; Productions = prHead::nextSymbol::[]}
                        parentSymbol <- nextSymbol
                        remainingProds <- prTail
                    yield { Origin = parentSymbol; Productions = remainingProds }
        }

    nextRules |> List.ofSeq


