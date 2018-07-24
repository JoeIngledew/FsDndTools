// load modifiers from some kind of config?
let firstAtk = 12
let secondAtk = 7
let mhDmgMod = 7
let ohDmgMod = 4
let critLowerBound = 15

type AtkResult = {
    Success : bool
    DamageRoll : int
    IsCritThreat : bool
    IsConfirmedCritical : bool }

let rand = new System.Random()

let crit x = x * 4

let rollDamage dmgMod =
    rand.Next(1, 5) + dmgMod

let rec rollSingleAtk ac modifier dmgMod =
    let roll = rand.Next(1, 21)
    let modifiedRoll = roll + modifier
    if modifiedRoll > ac then
        if roll >= critLowerBound then
            let confirmRoll = rollSingleAtk ac modifier dmgMod
            if confirmRoll.Success then
                { Success = true
                  DamageRoll = rollDamage (dmgMod + 1) |> crit
                  IsCritThreat = true
                  IsConfirmedCritical = true }
            else 
                { Success = true
                  DamageRoll = rollDamage dmgMod
                  IsCritThreat = true
                  IsConfirmedCritical = false }
        else 
            { Success = true
              DamageRoll = rollDamage dmgMod
              IsCritThreat = false
              IsConfirmedCritical = false }
    else { 
        Success = false
        DamageRoll = 0
        IsCritThreat = false
        IsConfirmedCritical = false }

let unpack res = (res.IsCritThreat, res.IsConfirmedCritical, res.Success, res.DamageRoll)

let pack res = 
    match res with
    | a, b, c, d -> { Success = c; DamageRoll = d; IsCritThreat = a; IsConfirmedCritical = b }


// int -> int -> int -> int -> (AtkResult list * int)
let rollLightningMaceAtk ac modifier stacks dmgMod =
    let rec loop results stacks' =
        let result = rollSingleAtk ac (modifier + stacks') (dmgMod + stacks') |> unpack
        match result with
        | true, true, _, _ -> 
            printfn "Got a crit"
            loop (result::results) (stacks' + 1)
        | true, false, _, _ -> 
            printfn "Got a crit threat"
            loop (result::results) stacks'
        | _ -> 
            printfn "Finished Lightning mace atk with AC %i mod %i stacks %i dmgMod %i" ac modifier stacks dmgMod
            printfn "Got the following results: %A" results
            ((result::results) |> List.map pack), stacks'
    loop [] stacks

let rollAtk ac modifier frenzy stacks =
    let unmodifiedSeq = 
        [ (firstAtk, mhDmgMod);
          (firstAtk, mhDmgMod);
          (secondAtk, mhDmgMod);
          (firstAtk, ohDmgMod);
          (secondAtk, ohDmgMod) ]
    let modSequence = 
        if frenzy then unmodifiedSeq else unmodifiedSeq |> List.skip 1
    let results = 
        modSequence 
        |> List.fold (fun x y ->
            let b = rollLightningMaceAtk ac (fst y + modifier) (snd x) (snd y)
            ((fst b) @ (fst x)), ((snd x) + (snd b))) ([], stacks)
    results
            