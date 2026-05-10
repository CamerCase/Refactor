module App.Rock

open System
open System.Threading
open App.Utils

type RockState =
| Falling
| Terminated

type State = {
    RockState: RockState
    X: int
    Y: int
    RedrawScreen: bool
    Tick: int
    StartTime: int
    G: float
}

let initialState = {
    RockState = Falling
    X = Console.BufferWidth/2
    Y = 0
    RedrawScreen = true
    Tick = -1
    StartTime = 0
    G = 9.77
}

let updateTick =
    createUpdateTick (fun s -> s.Tick) (fun t s -> {s with Tick = t})

let updateRock state =
    if state.Y <> Console.BufferHeight-1 then
        let t = float (state.Tick-state.StartTime)*0.025
        let y = state.G/2.0*t**2.0
        let pixelY = min (Console.BufferHeight-1) (int (y*300.0/float Console.BufferHeight))
        if pixelY <> state.Y then
            {state with Y=pixelY; RedrawScreen=true}
        else
            state
    else
        state

let drawRock state =
    displayMessage state.X state.Y ConsoleColor.Red "🪨"
    state

let updateRockKeyboard key state =
    let newState =
        match key with 
        | ConsoleKey.Enter  -> {state with StartTime=state.Tick; Y=0}
        | ConsoleKey.Escape -> {state with RockState = Terminated}
        | _ -> state
    if newState <> state then {newState with RedrawScreen=true}
    else state

let processKeyboard =
    createProcessKeyboard (fun k state -> updateRockKeyboard k.Key state)

let redrawScreen =
    createRedrawScreen
        drawRock
        (fun s -> s.RedrawScreen)
        (fun s -> {s with RedrawScreen = false})

let pipeline = [|
    updateTick
    updateRock
    processKeyboard
    redrawScreen
|]

let miLoop = createMainLoop pipeline (fun s -> s.RockState = Falling)

let mostrar() =
    let oldForeground = Console.ForegroundColor
    Console.CursorVisible <- false
    initialState |> miLoop |> ignore
    Console.CursorVisible <- true
    Console.ForegroundColor <- oldForeground
    Console.Clear()