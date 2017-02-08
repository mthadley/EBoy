module Z80.Cycles exposing (..)

{-| Represents how many cycles an intruction will take. Some
have two values since they are branching (`JR`, `JRNZ`, etc.).
-}


type Cycles
    = Always Int
    | Branching Int Int
