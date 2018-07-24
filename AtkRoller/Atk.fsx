open System

type Result =
| Miss
| Hit
| ThreatHit
| Crit

let firstAtk = 12
let secondAtk = 7
let mhDmgMod = 7
let ohDmgMod = 4
let critLowerBound = 15
let critMultiplier = 4

let rand = new System.Random()

let twenty () = rand.Next(1, 21)
let four () = rand.Next(1, 5)

let confirm ac modifier =
    (twenty() + modifier) >= ac

let singleAtk ac modifier = 
    let res = twenty ()
    let mRes = res + modifier
    let iRes = 
        match (mRes >= ac),(res >= critLowerBound) with
        | true, true -> if confirm ac modifier then Crit else ThreatHit
        | true, false -> Hit
        | _ -> Miss
    iRes,res,mRes

// int -> Result -> (Result * int * int) == Result * nat damage * modified damage
let calcDamageOnHit dmgMod h =
    let dmgRoll = four()
    match h with
    | Crit -> Crit,dmgRoll,(dmgRoll |> (+) dmgMod |> (*) critMultiplier)
    | Miss -> Miss,0,0
    | x -> x,dmgRoll,(dmgRoll |> (+) dmgMod)

let addInfo (n,m) (i,nd,md) =
    i,n,m,nd,md

let lMaceAtk ac modifier bStackInit dmgMod =
    let rec loop acc bStack =
        let result = singleAtk ac (modifier + bStack)
        match result with
        | Crit,n,m -> loop ((Crit |> calcDamageOnHit (dmgMod + bStack + 1) |> addInfo (n,m))::acc) (bStack + 1)
        | ThreatHit,n,m -> loop ((ThreatHit |> calcDamageOnHit (dmgMod + bStack) |> addInfo (n,m))::acc) bStack
        | Hit,n,m -> ((Hit |> calcDamageOnHit (dmgMod + bStack) |> addInfo (n,m))::acc),bStack
        | Miss,n,m -> (((Miss,0,0) |> addInfo (n,m))::acc),bStack
    loop [] bStackInit

let frenzyAtks = 
    [ (firstAtk, mhDmgMod);
      (firstAtk, mhDmgMod);
      (secondAtk, mhDmgMod);
      (firstAtk, ohDmgMod);
      (secondAtk, ohDmgMod); ]

let regAtks = frenzyAtks |> List.skip 1

//let toPrettyPrint ac (im,n,m,nd,md) =
//    sprintf "Rolled %i (nat %i) vs AC 

let toPrettyPrint rStacks ac initialStacks results =
    let resultsStrings =
        results 
        |> List.rev
        |> List.map (fun (im,n,m,nd,md) -> 
            sprintf "Rolled %i (nat %i) vs AC %i - a %A dealing %i (unmodified d4: %i) damage)" m n ac im md nd)
    let final = 
        (sprintf "Resulting in %i BitW stacks (up from %i)!" rStacks initialStacks)::(List.rev resultsStrings)
        |> List.rev
        |> List.map (fun s -> sprintf "%s%s" s System.Environment.NewLine)
    System.String.Concat(final)
    
let fullAtk ac modifier bStack isFrenzy =
    let atks = if isFrenzy then frenzyAtks else regAtks
    let (atkRes, endStacks) = 
        atks 
        |> List.fold (fun (hs,bs) (atkMod,dmgMod) ->
            let hits,stack = lMaceAtk ac (atkMod + modifier) bs dmgMod
            ([hs; hits] |> List.concat),(bs + stack)) ([], bStack)
    toPrettyPrint endStacks ac bStack atkRes
    
//    |> List.map (fun x -> x |> toPrettyPrint ac
