module Ast

// ---------------------------------------------------------------
// Les "types de données" du projet.
// Un type union (|) dit : "une valeur de ce type est SOIT ceci, SOIT cela".
// ---------------------------------------------------------------

// Étape 2 : un morceau de texte du programme (sortie du tokenizer)
type Token =
    | ParenOuvrante
    | ParenFermante
    | TEntier of int
    | TChaine of string
    | TSymbole of string

// Étape 3 : le programme sous forme d'arbre (sortie du parseur).
// (+ x 2)  devient  Liste [Symbole "+"; Symbole "x"; Entier 2]
type Expr =
    | Entier of int
    | Chaine of string
    | Symbole of string
    | Liste of Expr list

// Étape 4+ : ce que le programme calcule (sortie de l'évaluateur)
type Valeur =
    | VEntier of int
    | VChaine of string
    | VListe of Valeur list
    | VNil                    // nil : faux / rien
    | VT                      // t : vrai
    | VFonction of string     // une fonction passée en argument, par son nom (ex : map double ...)

// Une fonction déclarée avec defun : ses paramètres et son corps
type Fonction =
    { Params: string list
      Corps: Expr list }
