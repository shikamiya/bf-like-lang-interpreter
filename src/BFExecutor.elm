module BFExecutor exposing (getMaybeTapeValue, getTapeValue, runBFCommandByStep, runBFCommands)

import Array exposing (Array)
import BFTypes exposing (BFCommand(..), BFParseError(..), BFRunningState, BFTape(..), BFTokenKind(..), extractBFTape, tapeSize)


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


runBFCommands : BFRunningState -> BFRunningState
runBFCommands state =
    let
        newState =
            runBFCommandByStep state
    in
    case newState.error of
        Nothing ->
            runBFCommands newState

        Just _ ->
            newState


runBFCommandByStep : BFRunningState -> BFRunningState
runBFCommandByStep state =
    let
        indices =
            getNextIndices state.commands state.currentIndices

        cmd =
            getBFCommandByIndices state.commands indices
    in
    case cmd of
        Just (BFCommand token) ->
            case token.kind of
                NoOp ->
                    -- shouldn't occur, but just Run next command
                    let
                        nextState =
                            { state | currentIndices = indices }
                    in
                    runBFCommandByStep nextState

                LoopStart ->
                    case getTapeValue state.tape state.tapePointer of
                        0 ->
                            let
                                nextIndices =
                                    List.tail indices
                                        |> Maybe.withDefault []

                                nextState =
                                    { state
                                        | currentIndices = nextIndices
                                    }
                            in
                            -- ! remove executing next command
                            runBFCommandByStep nextState

                        _ ->
                            -- ! remove executing next command
                            runBFCommandByStep
                                { state
                                    | currentIndices = indices
                                }

                LoopEnd ->
                    let
                        -- back to LoopStart, take 'getNextIndices' into account
                        loopStartIndices =
                            -1 :: Maybe.withDefault [] (List.tail indices)

                        nextState =
                            { state | currentIndices = loopStartIndices }
                    in
                    -- ! remove executing next command
                    runBFCommandByStep nextState

                IncreasePointer ->
                    case increaseTapePointer state.tapePointer of
                        Ok ptr ->
                            { state | currentIndices = indices, tapePointer = ptr }

                        Err error ->
                            { state | currentIndices = indices, error = Just error }

                DecreasePointer ->
                    case decreaseTapePointer state.tapePointer of
                        Ok ptr ->
                            { state | currentIndices = indices, tapePointer = ptr }

                        Err error ->
                            { state | currentIndices = indices, error = Just error }

                IncreaseValue ->
                    { state
                        | currentIndices = indices
                        , tape = increaseTapeValue state.tape state.tapePointer
                    }

                DecreaseValue ->
                    { state
                        | currentIndices = indices
                        , tape = decreaseTapeValue state.tape state.tapePointer
                    }

                ReadInput ->
                    let
                        maybeInput =
                            String.dropLeft state.inputPointer state.input
                                |> String.uncons
                    in
                    case maybeInput of
                        Just ( input, _ ) ->
                            { state
                                | currentIndices = indices
                                , tape = setTapeValue state.tape state.tapePointer (Char.toCode input)
                                , inputPointer = state.inputPointer + 1
                            }

                        Nothing ->
                            -- Read Nothing : Do Nothing
                            { state
                                | currentIndices = indices
                                , inputPointer = state.inputPointer + 1
                            }

                PrintOutput ->
                    let
                        outputChar =
                            getTapeValue state.tape state.tapePointer
                                |> Char.fromCode

                        output =
                            outputChar :: state.output
                    in
                    { state
                        | currentIndices = indices
                        , output = output
                    }

        Just (BFLoopFunc _) ->
            -- shouldn't occur, but Enter and Run
            let
                nextState =
                    { state | currentIndices = indices }
            in
            runBFCommandByStep nextState

        Nothing ->
            -- End of Program
            { state | error = Just "[End]" }


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
