module Z80.Mode exposing (..)

{-| Type representing the current mode of the CPU. `Stoped` means the CPU
should stop and wait until a button is pressed.
-}


type Mode
    = Running
    | Stopped
