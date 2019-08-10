module BFTypes exposing (BFCommand(..), BFParseError(..), BFRunningState, BFTape(..), BFToken, BFTokenKind(..), BFTokenTable, bfParseErrorToString, initialBFTape, initialRunningState, tapeSize)

import Array exposing (Array)


type BFTokenKind
    = NoOp
    | LoopStart
    | LoopEnd
    | IncreaseValue
    | DecreaseValue
    | IncreasePointer
    | DecreasePointer
    | ReadInput
    | PrintOutput


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
    , output : List Char
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


initialRunningState : BFRunningState
initialRunningState =
    { commands = Array.fromList []
    , currentIndices = []
    , tape = initialBFTape
    , tapePointer = 0
    , input = ""
    , inputPointer = 0
    , output = []
    , error = Nothing
    }


tapeSize : Int
tapeSize =
    30000


initialBFTape : BFTape
initialBFTape =
    Array.repeat tapeSize 0
        |> BFTape
