module App.menu
open System
open App.Utils
open App.Types

type MenuState = Active | Terminated

type State = {
    MenuState: MenuState
    X: int; Y: int
    CurSorSelection: int
    CursorX: int
    Commands: (Command * string) array
    RedrawScreen: bool
}

let initialState = {
    MenuState = Active
    X = 20; Y = 10
    CurSorSelection = 0
    CursorX = 18
    Commands = [|
        NewRockSim,    "Simulacion de Roca"
        NewMonsterSim, "Simulacion de Monstruo"
        NewSaludo,     "Modulo de Saludo"
        Exit,          "Salir"
    |]
    RedrawScreen = true
}

// drawMenu ahora devuelve state para ser compatible con createRedrawScreen
let drawMenu state =
    state.Commands
    |> Array.iteri (fun i (_,legend) ->
        displayMessage state.X (state.Y+i) ConsoleColor.Cyan legend)
    displayMessage state.CursorX (state.Y+state.CurSorSelection) ConsoleColor.Yellow "*"
    state  // ← devuelve state

let updateMenuKeyboard key state =
    let newState =
        match key with 
        | ConsoleKey.UpArrow   -> {state with CurSorSelection = max 0 (state.CurSorSelection-1)}
        | ConsoleKey.DownArrow -> {state with CurSorSelection = min (state.Commands.Length-1) (state.CurSorSelection+1)}
        | ConsoleKey.Enter     -> {state with MenuState = Terminated}
        | _ -> state
    if newState <> state then {newState with RedrawScreen = true}
    else state

// ── Reemplazadas por genéricas ──────────────────────────────────────────
let processKeyboard =
    createProcessKeyboard (fun k state -> updateMenuKeyboard k.Key state)

let redrawScreen =
    createRedrawScreen
        drawMenu
        (fun s -> s.RedrawScreen)
        (fun s -> {s with RedrawScreen = false})

let pipeline = [| processKeyboard; redrawScreen |]

let miLoop = createMainLoop pipeline (fun s -> s.MenuState = Active)

let mostrar() =
    let oldForeground = System.Console.ForegroundColor
    System.Console.CursorVisible <- false
    let state = initialState |> miLoop
    System.Console.CursorVisible <- true
    System.Console.ForegroundColor <- oldForeground
    System.Console.Clear()
    state.Commands.[state.CurSorSelection] |> fst