module BFTypes exposing (BFCommand(..), BFParseError(..), BFRunningState, BFTape(..), BFToken, BFTokenKind(..), BFTokenTable)

import Array exposing (Array)


type BFTokenKind
    = NoOp
    | LoopStart
    | LoopEnd
    | IncreaseValue
    | DecreaseValue
    | IncreasePointer
    | DecreasePointer
    | FetchInput
    | PrintCharacter


type alias BFToken =
    { kind : BFTokenKind
    , value : String
    , index : Maybe Int
    , error : BFParseError
    }


type BFCommand
    = BFCommand BFToken
    | BFLoopFunc (Array BFCommand)


type alias BFTokenTable =
    ( List ( BFTokenKind, String ), String )


type BFParseError
    = NoError
    | TooManyLoopEnd
    | InsufficientLoopEnd


type alias BFRunningState =
    { commands : Array BFCommand
    , currentIndices : List Int
    , stepIndex : Maybe Int
    , tape : BFTape
    , tapePointer : Int
    , input : String
    , inputPointer : Int
    , output : String
    , error : Maybe String
    }


type BFTape
    = BFTape (Array Int)
