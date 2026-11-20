module Evaluateur

open System
open System.Collections.Generic
open Ast

// ---------------------------------------------------------------
// Environnement : où sont stockées les variables et les fonctions
// ---------------------------------------------------------------

// Une portée = les variables d'un "niveau" (le programme, ou un appel de fonction)
// + la portée parente (pour retrouver les variables globales).
type Portee =
    { Vars: Dictionary<string, Valeur>
      Parent: Portee option }

let nouvellePortee (parent: Portee option) : Portee =
    { Vars = Dictionary<string, Valeur>()
      Parent = parent }

let globale : Portee = nouvellePortee None

// Les fonctions déclarées avec defun
let fonctions = Dictionary<string, Fonction>()

let rec chercher (p: Portee) (nom: string) : Valeur option =
    match p.Vars.TryGetValue nom with
    | true, v -> Some v
    | _ ->
        match p.Parent with
        | Some parent -> chercher parent nom
        | None -> None

// (set x 20) : modifie une variable qui existe déjà
let rec modifier (p: Portee) (nom: string) (v: Valeur) : unit =
    if p.Vars.ContainsKey nom then
        p.Vars.[nom] <- v
    else
        match p.Parent with
        | Some parent -> modifier parent nom v
        | None -> failwithf "Impossible de modifier %s : variable inconnue" nom

// ---------------------------------------------------------------
// Petites fonctions utilitaires
// ---------------------------------------------------------------

// Comment afficher une valeur (ex : (1 2 3), nil, t)
let rec texteDe (v: Valeur) : string =
    match v with
    | VEntier n -> string n
    | VChaine s -> s
    | VListe l -> "(" + String.concat " " (List.map texteDe l) + ")"
    | VNil -> "nil"
    | VT -> "t"
    | VFonction nom -> nom

let texteDesArgs (args: Valeur list) : string =
    args |> List.map texteDe |> String.concat ""

// Tout est "vrai" sauf nil
let estVrai (v: Valeur) : bool = v <> VNil

let booleen (b: bool) : Valeur = if b then VT else VNil

let entierDe (v: Valeur) : int =
    match v with
    | VEntier n -> n
    | _ -> failwithf "Un entier est attendu, mais on a reçu : %s" (texteDe v)

let deuxEntiers (nom: string) (args: Valeur list) : int * int =
    match args with
    | [ a; b ] -> entierDe a, entierDe b
    | _ -> failwithf "%s attend exactement 2 arguments" nom

let deuxValeurs (nom: string) (args: Valeur list) : Valeur * Valeur =
    match args with
    | [ a; b ] -> a, b
    | _ -> failwithf "%s attend exactement 2 arguments" nom

let arith (nom: string) (op: int -> int -> int) (args: Valeur list) : Valeur =
    let a, b = deuxEntiers nom args
    VEntier(op a b)

let comparer (nom: string) (op: int -> int -> bool) (args: Valeur list) : Valeur =
    let a, b = deuxEntiers nom args
    booleen (op a b)

let primitives =
    set [ "+"; "-"; "*"; "/"; "<"; ">"; "<="; ">="; "="; "/="
          "list"; "printf"; "printfn"; "map"; "reduce" ]

let estPrimitive (nom: string) : bool = Set.contains nom primitives

let nomDeSymbole (e: Expr) : string =
    match e with
    | Symbole s -> s
    | _ -> failwith "Un nom (symbole) est attendu dans la liste des paramètres"

// Un symbole est soit une variable, soit le nom d'une fonction (ex : le "double" de map)
let evalSymbole (portee: Portee) (nom: string) : Valeur =
    match chercher portee nom with
    | Some v -> v
    | None ->
        if fonctions.ContainsKey nom || estPrimitive nom then
            VFonction nom
        else
            failwithf "Symbole inconnu : %s" nom

// ---------------------------------------------------------------
// Le coeur : l'évaluateur
// ---------------------------------------------------------------

let rec eval (portee: Portee) (expr: Expr) : Valeur =
    match expr with
    | Entier n -> VEntier n
    | Chaine s -> VChaine s
    | Symbole "t" -> VT
    | Symbole "nil" -> VNil
    | Symbole nom -> evalSymbole portee nom
    | Liste [] -> VNil

    // (defun nom (params...) corps...)
    | Liste(Symbole "defun" :: Symbole nom :: Liste parametres :: corps) ->
        let noms = parametres |> List.map nomDeSymbole
        fonctions.[nom] <- { Params = noms; Corps = corps }
        VNil

    // (let x 12) : crée une variable dans la portée courante
    | Liste [ Symbole "let"; Symbole nom; valeurExpr ] ->
        let v = eval portee valeurExpr
        portee.Vars.[nom] <- v
        v

    // (set x 20) : change une variable existante
    | Liste [ Symbole "set"; Symbole nom; valeurExpr ] ->
        let v = eval portee valeurExpr
        modifier portee nom v
        v

    // (if condition alors sinon)
    | Liste [ Symbole "if"; condition; alors; sinon ] ->
        if estVrai (eval portee condition) then eval portee alors else eval portee sinon

    | Liste [ Symbole "if"; condition; alors ] ->
        if estVrai (eval portee condition) then eval portee alors else VNil

    // (when condition corps...)
    | Liste(Symbole "when" :: condition :: corps) ->
        if estVrai (eval portee condition) then evalSequence portee corps else VNil

    // Sinon : appel de fonction. On évalue d'abord les arguments, puis on appelle.
    | Liste(Symbole nom :: argsExpr) ->
        let args = argsExpr |> List.map (eval portee)
        appliquer nom args

    | Liste _ -> failwith "Appel de fonction invalide : une fonction doit commencer par un nom"

// Évalue plusieurs expressions de suite, retourne la valeur de la dernière
and evalSequence (portee: Portee) (exprs: Expr list) : Valeur =
    exprs |> List.fold (fun _ e -> eval portee e) VNil

// TOUS les appels (fonctions de l'utilisateur ET fonctions de base) passent ici.
// C'est ici qu'on enregistrera l'arbre d'appel à l'étape 8.
and appliquer (nom: string) (args: Valeur list) : Valeur =
    match fonctions.TryGetValue nom with
    | true, f -> appelerFonction nom f args
    | _ -> primitive nom args

and appelerFonction (nom: string) (f: Fonction) (args: Valeur list) : Valeur =
    if List.length args <> List.length f.Params then
        failwithf "La fonction %s attend %d argument(s) mais en reçoit %d" nom (List.length f.Params) (List.length args)
    // Chaque appel a sa propre portée : les paramètres et les let locaux y vivent
    let locale = nouvellePortee (Some globale)
    List.iter2 (fun p a -> locale.Vars.[p] <- a) f.Params args
    evalSequence locale f.Corps

and primitive (nom: string) (args: Valeur list) : Valeur =
    match nom with
    | "+" -> arith nom (+) args
    | "-" -> arith nom (-) args
    | "*" -> arith nom ( * ) args
    | "/" -> arith nom (fun a b -> if b = 0 then failwith "Division par zéro" else a / b) args
    | "<" -> comparer nom (<) args
    | ">" -> comparer nom (>) args
    | "<=" -> comparer nom (<=) args
    | ">=" -> comparer nom (>=) args
    | "=" ->
        let a, b = deuxValeurs nom args
        booleen (a = b)
    | "/=" ->
        let a, b = deuxValeurs nom args
        booleen (a <> b)
    | "list" -> VListe args
    | "printf" ->
        Console.Write(texteDesArgs args)
        VNil
    | "printfn" ->
        Console.WriteLine(texteDesArgs args)
        VNil
    | "map" -> primMap args
    | "reduce" -> primReduce args
    | _ -> failwithf "Fonction inconnue : %s" nom

// (map fonction liste) : applique la fonction à chaque élément
and primMap (args: Valeur list) : Valeur =
    match args with
    | [ VFonction f; VListe elements ] ->
        VListe(elements |> List.map (fun x -> appliquer f [ x ]))
    | _ -> failwith "map attend une fonction et une liste"

// (reduce fonction liste) : combine les éléments deux par deux
and primReduce (args: Valeur list) : Valeur =
    match args with
    | [ VFonction f; VListe(premier :: reste) ] ->
        reste |> List.fold (fun acc x -> appliquer f [ acc; x ]) premier
    | [ VFonction _; VListe [] ] -> failwith "reduce ne peut pas réduire une liste vide"
    | _ -> failwith "reduce attend une fonction et une liste"

// Exécute tout le programme, expression par expression
let executer (programme: Expr list) : unit =
    programme |> List.iter (fun e -> eval globale e |> ignore)
