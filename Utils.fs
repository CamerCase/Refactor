module App.Utils

open System
open System.Threading
let displayMessage x y color (msg:string) =
    Console.SetCursorPosition(x,y)
    Console.ForegroundColor <- color
    msg |> Console.Write

let displayMessageRight y color (msg:string) =
    let start = Console.BufferWidth-msg.Length
    displayMessage start y color msg




let createMainLoop (pipeline: ('state -> 'state) array)  (isRunning: ('state -> bool)) =
    let rec mainLoop (state:'state) =
        pipeline
        |> Array.fold (fun acc f -> f acc) state
        |> fun newState ->
            if isRunning newState then 
                Thread.Sleep 25
                newState |> mainLoop
            else
                newState
    
    mainLoop

let createProcessKeyboard (handleKey: ConsoleKeyInfo -> 'state -> 'state) state =
    if Console.KeyAvailable then
        let k = Console.ReadKey true
        state |> handleKey k
    else
        state

let createRedrawScreen (drawFn: 'state -> 'state) (getRedraw: 'state -> bool) (setRedrawn: 'state -> 'state) state =
    if getRedraw state then
        Console.Clear()
        state |> drawFn |> setRedrawn
    else
        state

let createUpdateTick (getTick: 'state -> int) (setTick: int -> 'state -> 'state) state =
    setTick (getTick state + 1) state