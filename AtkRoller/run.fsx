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
| Miss of int * int * int// natural atk, modified atk, die
| Hit of int * int * int * int * int // natural atk, modified atk, natural dmg, modified dmg, die
| ThreatHit of int * int * int * int * int
| Crit of int * int * int *int * int

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
    ModifiedDamage : int
    DamageDie : int }

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
type InAtk = {
    [<JsonProperty>] mutable AttackBonus : int
    [<JsonProperty>] mutable DamageBonus : int
}

[<CLIMutable>]
[<JsonObject(MemberSerialization = MemberSerialization.OptIn)>]
type Request = {
    [<JsonProperty>] mutable Ac : int
    [<JsonProperty>] mutable Stacks : System.Nullable<int>
    [<JsonProperty>] mutable Modifier : System.Nullable<int>
    [<JsonProperty>] mutable Attacks : InAtk []
    [<JsonProperty>] mutable CritMinimum : System.Nullable<int>
    [<JsonProperty>] mutable CritMultiplier : System.Nullable<int>
    [<JsonProperty>] mutable DamageDie : System.Nullable<int>
}

let attackToString atk =
    sprintf "Rolled a %i (nat %i) resulting in a %s dealing %i damage (d%i rolled %i)" atk.ModifiedRoll atk.Natural atk.Descriptor atk.ModifiedDamage atk.DamageDie atk.DamageRoll

let fullResultToOutput fr = 
    { AttackResult = fr.Results |> Array.map attackToString 
      FinalStacks = fr.FinalStacks
      TotalDamage = fr.TotalDamage }

let mutable rand = new System.Random()
let twenty () = rand.Next(1, 21)
let dmg die = rand.Next(1, (die + 1))

let calcDamage dmgDie dmgBonus =
    let nRoll = dmg dmgDie
    let mDmg = nRoll + dmgBonus
    nRoll,mDmg

let confirm ac atkBonus =
    (twenty() + atkBonus) >= ac

let doAttack ac atkBonus dmgBonus critLowerBound critMultiplier dmgDie =
    let natRoll = twenty()
    let mRoll = natRoll + atkBonus
    match (natRoll > 1), (mRoll >= ac),(natRoll >= critLowerBound) with
    | true, true, true -> 
        let natDmg,mDmg = calcDamage dmgDie (dmgBonus+1) // plus one for b stack
        if confirm ac atkBonus then             
            Crit (natRoll, mRoll,natDmg,(mDmg*critMultiplier), dmgDie)
        else ThreatHit(natRoll, mRoll, natDmg, mDmg, dmgDie)
    | true, true, false ->
        let natDmg,mDmg = calcDamage dmgDie dmgBonus
        Hit (natRoll, mRoll, natDmg, mDmg, dmgDie)
    | _ -> Miss (natRoll, mRoll, dmgDie)

let reverseResults (rs, s) = 
    (rs |> List.rev), s

let toReadableDto (rs, s) = 
    let atkRes = 
        rs
        |> List.map (fun r ->
            let (a,b,c,d, e) =
                match r with 
                | Miss (a,b,e) -> a,b,0,0, e
                | Hit (a,b,c,d, e) -> a,b,c,d, e
                | ThreatHit (a,b,c,d, e) -> a,b,c,d, e
                | Crit (a,b,c,d, e) -> a,b,c,d, e
            { Descriptor = r |> resultToString
              Natural  = a
              ModifiedRoll = b
              DamageRoll = c
              ModifiedDamage = d
              DamageDie = e })
        |> List.toArray
    { Results = atkRes
      FinalStacks = s
      TotalDamage = atkRes |> Array.sumBy (fun x -> x.ModifiedDamage) }

let fullAtk ac modifier initStack attacks critLowerBound critMultiplier dmgDie (log: TraceWriter) =
    let rec loop atkLeft results bStacks =
        match atkLeft with
        | [] -> 
            results, bStacks
        | (atkMod,dmgMod)::xs ->
            let res = doAttack ac (modifier + atkMod + bStacks) (dmgMod + bStacks) critLowerBound critMultiplier dmgDie
            log.Info(sprintf "Did attack with mod %i dmg %i stacks at %i" atkMod dmgMod bStacks)
            log.Info(sprintf "Result: %A" res)
            match res with
            | Crit _ -> loop ((atkMod,dmgMod)::xs) (res::results) (bStacks + 1)
            | ThreatHit _ -> loop ((atkMod,dmgMod)::xs) (res::results) bStacks
            | _ -> loop xs (res::results) bStacks
    loop attacks [] initStack
    |> reverseResults
    |> toReadableDto

let valueIfNone replacementVal (x : System.Nullable<'a>) =
    if x = System.Nullable() then replacementVal else x.Value

let toAtkTuple atk =
    atk.AttackBonus, atk.DamageBonus

let Run(req: HttpRequestMessage, log: TraceWriter) =
    async {
        let! content =
            req.Content.ReadAsStringAsync()
            |> Async.AwaitTask
        try
            let input = JsonConvert.DeserializeObject<Request>(content)
            //log.Info(sprintf "Processing the following request: %A" input)
            rand <- new System.Random()        
            let ac = input.Ac
            let modifier = input.Modifier |> valueIfNone 0 
            let stacks = input.Stacks |> valueIfNone 0
            let critLowerBound = input.CritMinimum |> valueIfNone 20
            let critMulti = input.CritMultiplier |> valueIfNone 1
            let dmgDie = input.DamageDie |> valueIfNone 4

            log.Info("Got the following numerical vals:")
            log.Info(sprintf "AC: %i" ac)
            log.Info(sprintf "Modifier: %i" modifier)
            log.Info(sprintf "Stacks: %i" stacks)
            log.Info(sprintf "CLB: %i" critLowerBound)
            log.Info(sprintf "Multi: %i" critMulti)

            let attacks = input.Attacks |> Array.map toAtkTuple |> Array.toList

            log.Info(sprintf "Got these attacks: %A" attacks)

            let resp = fullAtk ac modifier stacks attacks critLowerBound critMulti dmgDie log |> fullResultToOutput
            log.Info(sprintf "Produced response: %A" resp)

            return req.CreateResponse(HttpStatusCode.OK, resp)
        with ex ->
            log.Info(ex.Message)
            return req.CreateResponse(HttpStatusCode.BadRequest, "Looks like the input was malformed!")
    } |> Async.RunSynchronously

