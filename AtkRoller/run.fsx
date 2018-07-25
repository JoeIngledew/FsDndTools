﻿#r "System.Net.Http"
#r "Newtonsoft.Json"
#r "System.Xml.dll"
#r "System.Runtime.Serialization.dll"

open System
open System.Net
open System.Net.Http
open Newtonsoft.Json
open Microsoft.FSharp.Reflection
open System.IO
open System.Reflection
open System.Runtime.Serialization
open System.Runtime.Serialization.Formatters.Binary
open System.Runtime.Serialization.Json
open System.Text
open System.Xml
open System.Xml.Serialization

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

[<CLIMutable>]
[<JsonObject(MemberSerialization = MemberSerialization.OptIn)>]
type Output = {
    [<JsonProperty>] mutable AttackResult : string []
    [<JsonProperty>] mutable FinalStacks : int
    [<JsonProperty>] mutable TotalDamage : int
}

[<CLIMutable>]
[<JsonObject(MemberSerialization = MemberSerialization.OptIn)>]
type Request = {
    [<JsonProperty>] mutable Ac : int
    [<JsonProperty>] mutable Stacks : System.Nullable<int>
    [<JsonProperty>] mutable Modifier : System.Nullable<int>
    [<JsonProperty>] mutable IsFrenzy : System.Nullable<bool>
}

let attackToString atk =
    sprintf "Rolled a %i (nat %i) resulting in a %s dealing %i damage (d4 rolled %i)" atk.ModifiedRoll atk.Natural atk.Descriptor atk.ModifiedDamage atk.DamageRoll

let fullResultToOutput fr = 
    { AttackResult = fr.Results |> Array.map attackToString 
      FinalStacks = fr.FinalStacks
      TotalDamage = fr.TotalDamage }

let firstAtk = 12
let secondAtk = 7
let mhDmgMod = 7
let ohDmgMod = 4
let critLowerBound = 15
let critMultiplier = 4

let mutable rand = new System.Random()
let twenty () = rand.Next(1, 21)
let four () = rand.Next(1, 5)

let fAttacks () = [
    (firstAtk, mhDmgMod);
    (firstAtk, mhDmgMod);
    (secondAtk, mhDmgMod);
    (firstAtk, ohDmgMod);
    (secondAtk, ohDmgMod); ]

let nAttacks () = [
    (firstAtk, mhDmgMod);
    (secondAtk, mhDmgMod);
    (firstAtk, ohDmgMod);
    (secondAtk, ohDmgMod); ]

let calcDamage dmgBonus =
    let nRoll = four()
    let mDmg = nRoll + dmgBonus
    nRoll,mDmg

let confirm ac atkBonus =
    (twenty() + atkBonus) >= ac

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

let fullAtk ac modifier initStack frenzy (log : TraceWriter) =
    let attacks = if frenzy then fAttacks () else nAttacks () // for some reason only works if they're functions not 'values' 
    // log.Info(sprintf "Using atk pattern: %A" attacks)
    let rec loop atkLeft results bStacks =
        match atkLeft with
        | [] -> 
            // log.Info(sprintf "Finished atks")
            results, bStacks
        | (atkMod,dmgMod)::xs ->
            // log.Info(sprintf "Calc atk with mod %A dmg %A" atkMod dmgMod)
            let res = doAttack ac (modifier + atkMod + bStacks) (dmgMod + bStacks)
            // log.Info(sprintf "Atk res: %A" res)
            match res with
            | Crit _ -> loop ((atkMod,dmgMod)::xs) (res::results) (bStacks + 1)
            | ThreatHit _ -> loop ((atkMod,dmgMod)::xs) (res::results) bStacks
            | _ -> loop xs (res::results) bStacks
    loop attacks [] initStack
    |> reverseResults
    |> toReadableDto

let valueIfNone replacementVal (x : System.Nullable<'a>) =
    if x = System.Nullable() then replacementVal else x.Value

let Run(req: HttpRequestMessage, log: TraceWriter) =
    async {
        let! content =
            req.Content.ReadAsStringAsync()
            |> Async.AwaitTask
        try
            let input = JsonConvert.DeserializeObject<Request>(content)

            rand <- new System.Random()        
            let ac = input.Ac
            // log.Info(sprintf "Got AC: %A" ac)
            let modifier = input.Modifier |> valueIfNone 0 
            // log.Info(sprintf "Got Mod: %A" modifier)
            let stacks = input.Stacks |> valueIfNone 0
            // log.Info(sprintf "Got stacks: %A" stacks)
            let frenzy = input.IsFrenzy |> valueIfNone false
            // log.Info(sprintf "Got frenzy: %A" frenzy)

            let resp = fullAtk ac modifier stacks frenzy log |> fullResultToOutput
            return req.CreateResponse(HttpStatusCode.OK, resp) 
        with ex ->
            log.Info(ex.Message)
            return req.CreateResponse(HttpStatusCode.BadRequest, "Looks like the input was malformed!")
    } |> Async.RunSynchronously

