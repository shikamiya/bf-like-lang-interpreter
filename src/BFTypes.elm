module BFTypes exposing (BFCommand(..), BFParseError(..), BFRunningState, BFTape(..), BFToken, BFTokenKind(..), BFTokenTable, bfParseErrorToString)

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
    , error : Maybe BFParseError
    }


type BFCommand
    = BFCommand BFToken
    | BFLoopFunc (Array BFCommand)


type alias BFTokenTable =
    ( List ( BFTokenKind, String ), String )


type BFParseError
    = TooManyLoopEnd
    | InsufficientLoopEnd


type alias BFRunningState =
    { commands : Array BFCommand
    , currentIndices : List Int
    , tape : BFTape
    , tapePointer : Int
    , input : String
    , inputPointer : Int
    , output : String
    , error : Maybe String
    }


type BFTape
    = BFTape (Array Int)


bfParseErrorToString : BFParseError -> String
bfParseErrorToString error =
    case error of
        TooManyLoopEnd ->
            "Loop End shouldn't be here"

        InsufficientLoopEnd ->
            "Loop End should be here"
