module BFExecutor exposing (getMaybeTapeValue, getTapeValue, runBFCommandByStep, runBFCommands)

import Array exposing (Array)
import BFTypes exposing (BFCommand(..), BFExecutorParams, BFParseError(..), BFTape(..), BFToken, BFTokenKind(..), PreCommand(..), RunningState(..), bfParseErrorToString, extractBFTape, tapeSize)


increaseTapePointer : Int -> Result String Int
increaseTapePointer current =
    if current + 1 < tapeSize then
        Ok (current + 1)

    else
        Err "Tape Pointer Limit Exceed"


decreaseTapePointer : Int -> Result String Int
decreaseTapePointer current =
    if 0 < current then
        Ok (current - 1)

    else
        Err "Tape Pointer Under Limit Exceed"


increaseTapeValue : BFTape -> Int -> BFTape
increaseTapeValue tape pos =
    getTapeValue tape pos
        + 1
        |> setTapeValue tape pos


decreaseTapeValue : BFTape -> Int -> BFTape
decreaseTapeValue tape pos =
    getTapeValue tape pos
        - 1
        |> setTapeValue tape pos


getMaybeTapeValue : BFTape -> Int -> Maybe Int
getMaybeTapeValue tape pos =
    extractBFTape tape
        |> Array.get pos


getTapeValue : BFTape -> Int -> Int
getTapeValue tape pos =
    getMaybeTapeValue tape pos
        |> Maybe.withDefault 0


setTapeValue : BFTape -> Int -> Int -> BFTape
setTapeValue tape pos value =
    let
        modValue =
            modBy 256 value
    in
    extractBFTape tape
        |> Array.set pos modValue
        |> BFTape


runBFCommands : BFExecutorParams -> BFExecutorParams
runBFCommands state =
    let
        newState =
            runBFCommandInternal state
    in
    case newState.runningState of
        Running ->
            runBFCommands newState

        RunningSlowly ->
            runBFCommands newState

        RunningUntilEndingLoop ->
            runBFCommands newState

        RunningUntilLeavingLoop ->
            runBFCommands newState

        _ ->
            newState


runBFCommandInternal : BFExecutorParams -> BFExecutorParams
runBFCommandInternal state =
    case state.runningState of
        NotRunning ->
            state

        Pausing ->
            state

        Running ->
            runBFCommandByStep state

        RunningSlowly ->
            runBFCommandByStep state

        RunningStep ->
            let
                newState =
                    runBFCommandByStep state
            in
            { newState | runningState = Pausing }

        RunningUntilEndingLoop ->
            let
                newState =
                    runBFCommandByStep state
            in
            case newState.nextPreCommand of
                NextCommand ->
                    newState

                _ ->
                    { newState | runningState = Pausing }

        RunningUntilLeavingLoop ->
            let
                newState =
                    runBFCommandByStep state
            in
            case newState.nextPreCommand of
                LeaveLoop ->
                    { newState | runningState = Pausing }

                _ ->
                    newState


runBFCommandByStep : BFExecutorParams -> BFExecutorParams
runBFCommandByStep oldState =
    let
        indices =
            case oldState.nextPreCommand of
                NextCommand ->
                    getNextIndices oldState.commands oldState.currentIndices

                ContinueLoop ->
                    0
                        :: Maybe.withDefault [] (List.tail oldState.currentIndices)
                        |> getNextIndices oldState.commands

                LeaveLoop ->
                    List.tail oldState.currentIndices
                        |> Maybe.withDefault []
                        |> getNextIndices oldState.commands

        cmd =
            getBFCommandByIndices oldState.commands indices

        state =
            { oldState | currentIndices = indices, nextPreCommand = NextCommand }
    in
    case cmd of
        Just (BFCommand token) ->
            case token.error of
                Just err ->
                    { state | runningState = Pausing, error = Just <| "Parse Error: " ++ bfParseErrorToString err }

                Nothing ->
                    runBFCommandByStepInternal token state

        Just (BFLoopFunc _) ->
            -- shouldn't occur, but Enter and Run
            runBFCommandByStep state

        Nothing ->
            -- End of Program
            { state | runningState = NotRunning }


runBFCommandByStepInternal : BFToken -> BFExecutorParams -> BFExecutorParams
runBFCommandByStepInternal token state =
    case token.kind of
        NoOp ->
            -- shouldn't occur, but just Run next command
            runBFCommandByStep state

        LoopStart ->
            case getTapeValue state.tape state.tapePointer of
                0 ->
                    { state | nextPreCommand = LeaveLoop }

                _ ->
                    state

        LoopEnd ->
            case getTapeValue state.tape state.tapePointer of
                0 ->
                    { state | nextPreCommand = LeaveLoop }

                _ ->
                    { state | nextPreCommand = ContinueLoop }

        IncreasePointer ->
            case increaseTapePointer state.tapePointer of
                Ok ptr ->
                    { state
                        | tapePointer = ptr
                        , currentTapePage = ptr // (16 * 16)
                    }

                Err error ->
                    { state
                        | error = Just error
                        , runningState = Pausing
                    }

        DecreasePointer ->
            case decreaseTapePointer state.tapePointer of
                Ok ptr ->
                    { state
                        | tapePointer = ptr
                        , currentTapePage = ptr // (16 * 16)
                    }

                Err error ->
                    { state
                        | error = Just error
                        , runningState = Pausing
                    }

        IncreaseValue ->
            { state | tape = increaseTapeValue state.tape state.tapePointer }

        DecreaseValue ->
            { state | tape = decreaseTapeValue state.tape state.tapePointer }

        ReadInput ->
            let
                maybeInput =
                    String.dropLeft state.inputPointer state.input
                        |> String.uncons
                        |> Maybe.map Tuple.first
            in
            case maybeInput of
                Just input ->
                    { state
                        | tape = setTapeValue state.tape state.tapePointer (Char.toCode input)
                        , inputPointer = state.inputPointer + 1
                    }

                Nothing ->
                    -- Read Nothing : Do Nothing
                    { state | inputPointer = state.inputPointer + 1 }

        PrintOutput ->
            let
                outputChar =
                    getTapeValue state.tape state.tapePointer
                        |> Char.fromCode

                output =
                    outputChar :: state.output
            in
            { state | output = output }


getNextIndices : Array BFCommand -> List Int -> List Int
getNextIndices cmds pos =
    increaseBFCommandIndices pos
        |> skipUntilNextBFCommand cmds


skipUntilNextBFCommand : Array BFCommand -> List Int -> List Int
skipUntilNextBFCommand cmds pos =
    let
        cmd =
            getBFCommandByIndices cmds pos
    in
    case cmd of
        Nothing ->
            -- beyond commands
            case pos of
                (_ :: (_ :: _)) as parentPos ->
                    -- has Parent : End of LoopFunc
                    getNextIndices cmds parentPos

                _ ->
                    -- don't have any parent : End of Program
                    pos

        Just (BFCommand token) ->
            case token.kind of
                NoOp ->
                    -- NoOp : Skip
                    getNextIndices cmds pos

                _ ->
                    -- Normal Command
                    pos

        Just (BFLoopFunc _) ->
            -- LoopFunc : Enter and Skip until LoopStart (should skip nothing)
            0
                :: pos
                |> skipUntilNextBFCommand cmds


getBFCommandByIndices : Array BFCommand -> List Int -> Maybe BFCommand
getBFCommandByIndices cmds pos =
    List.reverse pos
        |> getBFCommandByRevIndices cmds


getBFCommandByRevIndices : Array BFCommand -> List Int -> Maybe BFCommand
getBFCommandByRevIndices cmds pos =
    case pos of
        currentPos :: remainingPos ->
            case Array.get currentPos cmds of
                Just (BFLoopFunc newCmds) ->
                    getBFCommandByRevIndices newCmds remainingPos

                Just ((BFCommand _) as cmd) ->
                    Just cmd

                Nothing ->
                    Nothing

        [] ->
            Just <| BFLoopFunc cmds


increaseBFCommandIndices : List Int -> List Int
increaseBFCommandIndices pos =
    case pos of
        head :: tail ->
            head + 1 :: tail

        [] ->
            [ 0 ]
