type Result =
| Miss of int * int // natural atk, modified atk
| Hit of int * int * int * int // natural atk, modified atk, natural dmg, modified dmg
| ThreatHit of int * int * int * int 
| Crit of int * int * int *int

let resultToString r = 
    match r with
    | Miss _ -> "Miss"
    | Hit _ -> "Hit"
    | ThreatHit _ -> "Critical Threat (No Confirm)"
    | Crit _ -> "Critical Hit"

type Attack = {
    Descriptor : string
    Natural : int
    ModifiedRoll : int
    DamageRoll : int
    ModifiedDamage : int }

type FullResult = {
    Results : Attack []
    FinalStacks : int
    TotalDamage : int }

let firstAtk = 12
let secondAtk = 7
let mhDmgMod = 7
let ohDmgMod = 4
let critLowerBound = 15
let critMultiplier = 4

let rand = new System.Random()
let twenty () = rand.Next(1, 21)
let four () = rand.Next(1, 5)

let fAttacks = [
    (firstAtk, mhDmgMod);
    (firstAtk, mhDmgMod);
    (secondAtk, mhDmgMod);
    (firstAtk, ohDmgMod);
    (secondAtk, ohDmgMod); ]

let nAttacks = fAttacks |> List.skip 1

let calcDamage dmgBonus =
    let nRoll = four()
    let mDmg = nRoll + dmgBonus
    nRoll,mDmg

let confirm ac atkBonus =
    if (twenty() + atkBonus) >= ac then true else false

let doAttack ac atkBonus dmgBonus =
    let natRoll = twenty()
    let mRoll = natRoll + atkBonus
    match (mRoll >= ac),(natRoll >= critLowerBound) with
    | true, true -> 
        let natDmg,mDmg = calcDamage (dmgBonus+1) // plus one for b stack
        if confirm ac atkBonus then             
            Crit (natRoll, mRoll,natDmg,(mDmg*critMultiplier))
        else ThreatHit(natRoll, mRoll, natDmg, mDmg)
    | true, false ->
        let natDmg,mDmg = calcDamage dmgBonus
        Hit (natRoll, mRoll, natDmg, mDmg)
    | _ -> Miss (natRoll, mRoll)

let reverseResults (rs, s) = 
    (rs |> List.rev), s

let toReadableDto (rs, s) = 
    let atkRes = 
        rs
        |> List.map (fun r ->
            let (a,b,c,d) =
                match r with 
                | Miss (a,b) -> a,b,0,0
                | Hit (a,b,c,d) -> a,b,c,d
                | ThreatHit (a,b,c,d) -> a,b,c,d
                | Crit (a,b,c,d) -> a,b,c,d
            { Descriptor = r |> resultToString
              Natural  = a
              ModifiedRoll = b
              DamageRoll = c
              ModifiedDamage = d })
        |> List.toArray
    { Results = atkRes
      FinalStacks = s
      TotalDamage = atkRes |> Array.sumBy (fun x -> x.ModifiedDamage) }

let fullAtk ac modifier initStack frenzy =
    let attacks = if frenzy then fAttacks else nAttacks
    let rec loop atkLeft results bStacks =
        match atkLeft with
        | [] -> results, bStacks
        | (atkMod,dmgMod)::xs ->
            let res = doAttack ac (modifier + atkMod + bStacks) (dmgMod + bStacks)
            match res with
            | Crit _ -> loop ((atkMod,dmgMod)::xs) (res::results) (bStacks + 1)
            | ThreatHit _ -> loop ((atkMod,dmgMod)::xs) (res::results) bStacks
            | _ -> loop xs (res::results) bStacks
    loop attacks [] initStack
    |> reverseResults
    |> toReadableDto