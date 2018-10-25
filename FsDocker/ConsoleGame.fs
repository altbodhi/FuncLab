module ConsoleGame
open System
open Game
open Util

let consoleDraw state =
    Console.Clear()
    for cell in state do
        let (x,y) = getPosition cell
        Console.SetCursorPosition (x,y)
        cell |> getSymbol |> printf "%s"

let input () = 
    let key = Console.ReadKey(true)
    match key.Key with
    | ConsoleKey.UpArrow -> Some Up
    | ConsoleKey.DownArrow -> Some Down
    | ConsoleKey.LeftArrow -> Some Left
    | ConsoleKey.RightArrow -> Some Right
    | ConsoleKey.Escape -> Some Exit
    | _ -> None


let showWin a =
    if a then
        Console.ForegroundColor <- ConsoleColor.Green
    else () 

let start () =
    Console.CursorVisible <- false
    let zero = loadMap "map.txt" 
    play zero consoleDraw showWin input 
    Console.Clear()
    Console.CursorVisible <- true    