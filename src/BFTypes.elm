module BFTypes exposing (BFCommand(..), BFParseError(..), BFRunningState, BFTape(..), BFToken, BFTokenKind(..), BFTokenTable, bfParseErrorToString, extractBFTape, initialBFTape, initialRunningState, tapePages, tapeSize, tokenKindToString)

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
    , popoverIndices : List Int
    , tape : BFTape
    , tapePointer : Int
    , currentTapePage : Int
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
    , popoverIndices = []
    , tape = initialBFTape
    , tapePointer = 0
    , currentTapePage = 0
    , input = ""
    , inputPointer = 0
    , output = []
    , error = Nothing
    }


tapeSize : Int
tapeSize =
    16 * 16 * tapePages


tapePages : Int
tapePages =
    128


initialBFTape : BFTape
initialBFTape =
    Array.repeat tapeSize 0
        |> BFTape


extractBFTape : BFTape -> Array Int
extractBFTape tape =
    let
        (BFTape tapeArray) =
            tape
    in
    tapeArray


tokenKindToString : BFTokenKind -> String
tokenKindToString kind =
    case kind of
        NoOp ->
            "NoOp"

        LoopStart ->
            "LoopStart"

        LoopEnd ->
            "LoopEnd"

        IncreaseValue ->
            "IncreaseValue"

        DecreaseValue ->
            "DecreaseValue"

        IncreasePointer ->
            "IncreasePointer"

        DecreasePointer ->
            "DecreasePointer"

        ReadInput ->
            "ReadInput"

        PrintOutput ->
            "PrintOutput"
