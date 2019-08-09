module BFRunner exposing (bfRun, bfStepRun, getTapeValue, initialBFTape, initialRunningState)

import Array exposing (Array)
import BFTypes exposing (BFCommand(..), BFParseError(..), BFRunningState, BFTape(..), BFTokenKind(..))


initialRunningState : BFRunningState
initialRunningState =
    { commands = Array.fromList []
    , currentIndices = []
    , tape = initialBFTape
    , tapePointer = 0
    , input = ""
    , inputPointer = 0
    , output = ""
    , error = Nothing
    }


tapeSize : Int
tapeSize =
    30000


initialBFTape : BFTape
initialBFTape =
    Array.repeat tapeSize 0
        |> BFTape


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
    let
        (BFTape tapeArray) =
            tape

        value =
            Array.get pos tapeArray
                |> Maybe.withDefault 0
    in
    (if 255 < value + 1 then
        Array.set pos 0 tapeArray

     else
        Array.set pos (value + 1) tapeArray
    )
        |> BFTape


decreaseTapeValue : BFTape -> Int -> BFTape
decreaseTapeValue tape pos =
    let
        (BFTape tapeArray) =
            tape

        value =
            Array.get pos tapeArray
                |> Maybe.withDefault 0
    in
    (if value < 1 then
        Array.set pos 255 tapeArray

     else
        Array.set pos (value - 1) tapeArray
    )
        |> BFTape


getTapeValue : BFTape -> Int -> Int
getTapeValue tape pos =
    let
        (BFTape tapeArray) =
            tape

        value =
            Array.get pos tapeArray
                |> Maybe.withDefault 0
    in
    value


setTapeValue : BFTape -> Int -> Int -> BFTape
setTapeValue tape pos value =
    let
        (BFTape tapeArray) =
            tape

        modValue =
            modBy 256 value

        newTape =
            Array.set pos modValue tapeArray
                |> BFTape
    in
    newTape


bfRun : BFRunningState -> BFRunningState
bfRun state =
    let
        newState =
            bfStepRun state
    in
    case newState.error of
        Nothing ->
            bfRun newState

        Just _ ->
            newState


bfStepRun : BFRunningState -> BFRunningState
bfStepRun state =
    let
        cmds =
            state.commands

        inputStr =
            state.input

        indices =
            getNextIndices cmds state.currentIndices

        cmd =
            getBFCommandByIndices cmds indices
    in
    case cmd of
        Just (BFCommand token) ->
            case token.kind of
                NoOp ->
                    -- shouldn't occur
                    let
                        nextState =
                            { state | currentIndices = indices }
                    in
                    bfStepRun nextState

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
                            bfStepRun nextState

                        _ ->
                            bfStepRun
                                { state
                                    | currentIndices = indices
                                }

                LoopEnd ->
                    let
                        loopStartIndices =
                            -1 :: Maybe.withDefault [] (List.tail indices)

                        nextState =
                            { state | currentIndices = loopStartIndices }
                    in
                    bfStepRun nextState

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
                            String.dropLeft state.inputPointer inputStr
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
                            { state
                                | currentIndices = indices
                                , inputPointer = state.inputPointer + 1
                            }

                PrintOutput ->
                    let
                        outputChar =
                            getTapeValue state.tape state.tapePointer
                                |> Char.fromCode
                                |> String.fromChar

                        output =
                            String.append state.output outputChar
                    in
                    { state
                        | currentIndices = indices
                        , output = output
                    }

        Just (BFLoopFunc _) ->
            -- shouldn't occur
            let
                nextState =
                    { state | currentIndices = indices }
            in
            bfStepRun nextState

        Nothing ->
            -- end
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
            case List.tail pos of
                Just ((_ :: _) as parentPos) ->
                    getNextIndices cmds parentPos

                _ ->
                    pos

        Just (BFCommand token) ->
            case token.kind of
                NoOp ->
                    getNextIndices cmds pos

                _ ->
                    pos

        Just (BFLoopFunc _) ->
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
