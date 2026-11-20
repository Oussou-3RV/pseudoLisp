module Tokenizer

open System
open Ast

// Un mot (symbole ou nombre) s'arrête à un espace, une parenthèse, un guillemet ou un ;
let estFinDeMot (c: char) : bool =
    Char.IsWhiteSpace c || c = '(' || c = ')' || c = '"' || c = ';'

// On parcourt la liste de caractères : on regarde le premier, on produit un jeton,
// puis on recommence avec le reste (récursion).
let rec tokeniserCaracteres (chars: char list) : Token list =
    match chars with
    | [] -> []
    | c :: reste when Char.IsWhiteSpace c ->
        tokeniserCaracteres reste
    | ';' :: reste ->
        // commentaire : on ignore tout jusqu'à la fin de la ligne
        tokeniserCaracteres (List.skipWhile (fun c -> c <> '\n') reste)
    | '(' :: reste -> ParenOuvrante :: tokeniserCaracteres reste
    | ')' :: reste -> ParenFermante :: tokeniserCaracteres reste
    | '"' :: reste ->
        let contenu = List.takeWhile (fun c -> c <> '"') reste
        if List.length contenu = List.length reste then
            failwith "Chaîne de caractères non terminée (guillemet manquant)"
        else
            let suite = List.skip (List.length contenu + 1) reste
            let texte = contenu |> List.map string |> String.concat ""
            TChaine(texte.Replace("\\n", "\n")) :: tokeniserCaracteres suite
    | _ ->
        // un mot : soit un nombre (12, -5), soit un symbole (double, +, taxable?)
        let mot = List.takeWhile (fun c -> not (estFinDeMot c)) chars
        let suite = List.skip (List.length mot) chars
        let texte = mot |> List.map string |> String.concat ""
        let jeton =
            match Int32.TryParse(texte) with
            | true, n -> TEntier n
            | _ -> TSymbole texte
        jeton :: tokeniserCaracteres suite

let tokeniserTexte (texte: string) : Token list =
    tokeniserCaracteres (List.ofSeq texte)
