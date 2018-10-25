module Game

type Position = int * int

type Element =
    | Docker of Position
    | Block of Position
    | Brick of Position
    | Target of Position

type Commad = 
    | Up
    | Down
    | Left
    | Right
    | Exit
let size = 20

let state = 
    [for i in [0 .. size] -> Brick (i, 0)] @
    [for i in [0 .. size] -> Brick (i, size)] @
    [for i in [1.. size - 1] -> Brick (0, i)] @
    [for i in [1.. size - 1] -> Brick (size, i)] @ [Docker (1,1); Block (2,2); Target (10,10)]

let getPosition = function 
    | Docker p | Block p | Brick p | Target p -> p 

let getSymbol = function
    | Docker _ -> "D"
    | Block _ -> "#"
    | Brick _ -> "X"
    | Target _ -> "O"

let createElement s pos = 
    match s with
    | "D" -> Some (Docker pos)
    | "#" -> Some (Block pos) 
    | "X" -> Some (Brick pos)
    | "O" -> Some (Target pos)
    | _ -> None

let getDocker state = state |> List.find (function | Docker _ -> true | _ -> false )

let getElement state pos = state |> List.tryFind(fun e -> pos = ( getPosition e))


let getNewPosition cmd old =
        let (x, y) = old
        match cmd with
        | Up -> (x, y - 1)
        | Down -> (x, y + 1)
        | Left -> (x - 1 , y)
        | Right -> (x + 1 , y)
        | _ ->  (x , y)

let move cmd state =
    let docker = state |> getDocker
    let (x, y) = getPosition docker
    let newPos = getNewPosition cmd (x,y)
    let check = getElement state newPos
    
    if (x, y) = newPos then 
        state 
    else
        match check with
        | None |  Some (Target _ ) ->    
            (state |> List.filter (fun e -> e <> docker )) @ [ Docker newPos ]
        | Some (Block p) -> 
                let p1 = getNewPosition cmd p
                let newCell = getElement state p1
                match newCell with
                | None | Some (Target _) ->
                    (state |> List.filter (fun e -> e <> docker && e <> Block p)) @ [ Docker newPos; Block p1 ] 
                | _ -> state                   
        | Some (Brick _) -> state
        | _ -> state              
                      
let win state = 
    let (Target t) = state |> List.find (function | Target _ -> true | _ -> false )    
    let (Block b) = state |> List.find (function | Block _ -> true | _ -> false )    
    t = b

let gamePlay state  cmd =
        move cmd state

let play zero draw showWin input =
    draw zero
    let rec loop state = 
        showWin (win state)
        draw state 
        match input() with
        | None -> loop state
        | Some Exit -> ()
        | Some cmd -> loop (gamePlay state cmd)
        (*showWin (win state)

        draw state 
        
        match input() with
        | None -> loop state
        | Some Exit -> ()
        | Some cmd -> loop (move cmd state) *)

    loop zero

