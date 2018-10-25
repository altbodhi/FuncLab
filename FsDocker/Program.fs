module Program

type Options = { ui: bool;  }


let parseArg args =
    let defaultArgs = { ui = false }
    let rec parse args options =
        match args with
        | [] -> options
        | "/ui" :: xs | "-ui" :: xs | "--ui" :: xs -> parse xs {options with ui = true }
        | h :: t -> parse t options
    parse args defaultArgs

[<EntryPoint>]
let main argv =
    let args = argv |> Array.toList |> parseArg 
    if args.ui then
       AvaloniaGame.start()
    else       
        ConsoleGame.start()
    exit 0
