module Parseur

open Ast

// Lit UNE expression au début de la liste de jetons.
// Retourne l'expression lue ET les jetons qui restent à lire.
let rec lireExpr (jetons: Token list) : Expr * Token list =
    match jetons with
    | [] -> failwith "Fin de fichier inattendue"
    | TEntier n :: reste -> Entier n, reste
    | TChaine s :: reste -> Chaine s, reste
    | TSymbole s :: reste -> Symbole s, reste
    | ParenFermante :: _ -> failwith "Parenthèse fermante inattendue"
    | ParenOuvrante :: reste -> lireElements reste []

// Après une "(" : on lit des expressions jusqu'à la ")" correspondante.
// "acc" accumule les éléments lus (à l'envers, d'où le List.rev).
and lireElements (jetons: Token list) (acc: Expr list) : Expr * Token list =
    match jetons with
    | [] -> failwith "Parenthèse fermante manquante"
    | ParenFermante :: reste -> Liste(List.rev acc), reste
    | _ ->
        let e, reste = lireExpr jetons
        lireElements reste (e :: acc)

// Un programme = une suite d'expressions au niveau supérieur
let rec lireProgramme (jetons: Token list) : Expr list =
    match jetons with
    | [] -> []
    | _ ->
        let e, reste = lireExpr jetons
        e :: lireProgramme reste

let analyser (texte: string) : Expr list =
    texte |> Tokenizer.tokeniserTexte |> lireProgramme
