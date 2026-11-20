module Program

open System.IO

[<EntryPoint>]
let main (argv: string[]) : int =
    if argv.Length = 0 then
        printfn "Usage : pseudoLisp <fichier.lisp>"
        1
    else
        try
            let texte = File.ReadAllText(argv.[0])
            let programme = Parseur.analyser texte
            Evaluateur.executer programme
            0
        with ex ->
            eprintfn "Erreur : %s" ex.Message
            1
