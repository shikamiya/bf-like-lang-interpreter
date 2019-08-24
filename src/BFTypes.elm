module BFTypes exposing (BFCommand(..), BFExecutorParams, BFParseError(..), BFTape(..), BFToken, BFTokenKind(..), BFTokenTable, PreCommand(..), RunningState(..), bfParseErrorToString, extractBFTape, initialBFTape, initialExecutorParams, tapePages, tapeSize, tokenKindFromInt, tokenKindToInt, tokenKindToString)

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


type RunningState
    = NotRunning
    | Running
    | RunningSlowly
    | RunningStep
    | RunningUntilEndingLoop
    | RunningUntilLeavingLoop
    | Pausing


type PreCommand
    = NextCommand
    | ContinueLoop
    | LeaveLoop


type alias BFExecutorParams =
    { runningState : RunningState
    , nextPreCommand : PreCommand
    , commands : Array BFCommand
    , currentIndices : List Int
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


initialExecutorParams : BFExecutorParams
initialExecutorParams =
    { runningState = NotRunning
    , nextPreCommand = NextCommand
    , commands = Array.fromList []
    , currentIndices = []
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


tokenKindToInt : BFTokenKind -> Int
tokenKindToInt kind =
    case kind of
        NoOp ->
            0

        IncreasePointer ->
            1

        DecreasePointer ->
            2

        IncreaseValue ->
            3

        DecreaseValue ->
            4

        PrintOutput ->
            5

        ReadInput ->
            6

        LoopStart ->
            7

        LoopEnd ->
            8


tokenKindFromInt : Int -> Maybe BFTokenKind
tokenKindFromInt value =
    case value of
        0 ->
            Just NoOp

        1 ->
            Just IncreasePointer

        2 ->
            Just DecreasePointer

        3 ->
            Just IncreaseValue

        4 ->
            Just DecreaseValue

        5 ->
            Just PrintOutput

        6 ->
            Just ReadInput

        7 ->
            Just LoopStart

        8 ->
            Just LoopEnd

        _ ->
            Nothing
