module BFParser exposing (parseTokens)

import Array exposing (Array)
import BFTypes exposing (BFCommand(..), BFParseError(..), BFTape(..), BFToken, BFTokenKind(..), BFTokenTable)
import Parser exposing ((|.), Parser)


type BFCommandList
    = BFCommandList (List BFCommand)


type BFCommandStack
    = BFCommandStack (List BFCommandList)


parseTokenByTable : BFTokenTable -> Parser BFToken
parseTokenByTable table =
    let
        ( tokenTable, _ ) =
            table
    in
    tokenTable
        |> List.map
            (\x ->
                let
                    kind =
                        Tuple.first x

                    value =
                        Tuple.second x
                in
                Parser.succeed (BFToken kind value Nothing)
                    |. Parser.token value
            )
        |> Parser.oneOf


parseNoOpToken : Parser BFToken
parseNoOpToken =
    Parser.chompIf (always True)
        |> Parser.getChompedString
        |> Parser.map (\x -> BFToken NoOp x Nothing)


addCommandIntoList : BFCommandList -> BFCommand -> BFCommandList
addCommandIntoList list cmd =
    let
        (BFCommandList commands) =
            list
    in
    BFCommandList (cmd :: commands)


addCommandIntoCurrentList : BFCommandStack -> BFCommand -> BFCommandStack
addCommandIntoCurrentList stack cmd =
    let
        (BFCommandStack stackList) =
            stack

        currentList =
            Maybe.withDefault (BFCommandList []) (List.head stackList)

        ancestorsList =
            Maybe.withDefault [] (List.tail stackList)
    in
    BFCommandStack (addCommandIntoList currentList cmd :: ancestorsList)


reverseCommandList : BFCommandList -> Array BFCommand
reverseCommandList list =
    let
        (BFCommandList commands) =
            list
    in
    List.reverse commands
        |> Array.fromList


beginNewLoopCommand : BFCommandStack -> BFCommand -> BFCommandStack
beginNewLoopCommand stack cmd =
    let
        (BFCommandStack stackList) =
            stack
    in
    BFCommandStack (BFCommandList [ cmd ] :: stackList)


finalizeLoopCommand : BFCommandStack -> BFCommand -> BFCommandStack
finalizeLoopCommand stack cmd =
    let
        (BFCommandStack stackList) =
            stack

        currentList =
            Maybe.withDefault (BFCommandList []) (List.head stackList)

        commands =
            reverseCommandList (addCommandIntoList currentList cmd)

        ancestorsList =
            Maybe.withDefault [] (List.tail stackList)
    in
    addCommandIntoCurrentList (BFCommandStack ancestorsList) (BFLoopFunc commands)


finalizeLoopCommandWithFullStack : BFCommandStack -> BFCommand -> BFCommandStack
finalizeLoopCommandWithFullStack stack cmd =
    finalizeLoopCommand stack cmd


parseTokensHelper : BFTokenTable -> Parser (Array BFCommand)
parseTokensHelper cmdTable =
    Parser.loop (BFCommandStack [ BFCommandList [] ])
        (\memo ->
            Parser.oneOf
                [ parseTokenByTable cmdTable
                    |> Parser.map
                        (\token ->
                            let
                                (BFCommandStack stackList) =
                                    memo

                                depth =
                                    List.length stackList

                                stack =
                                    case token.kind of
                                        LoopStart ->
                                            beginNewLoopCommand memo (BFCommand token)

                                        LoopEnd ->
                                            if depth <= 1 then
                                                BFCommand
                                                    { token
                                                        | kind = NoOp
                                                        , error = Just TooManyLoopEnd
                                                    }
                                                    |> addCommandIntoCurrentList memo

                                            else
                                                finalizeLoopCommandWithFullStack memo (BFCommand token)

                                        _ ->
                                            addCommandIntoCurrentList memo (BFCommand token)
                            in
                            Parser.Loop stack
                        )
                , parseNoOpToken
                    |> Parser.map
                        (\token ->
                            let
                                stack =
                                    addCommandIntoCurrentList memo (BFCommand token)
                            in
                            Parser.Loop stack
                        )
                , Parser.end
                    |> Parser.map
                        (\_ ->
                            let
                                finalizer list current =
                                    let
                                        (BFCommandList commands) =
                                            current

                                        commandLength =
                                            List.length commands
                                    in
                                    if commandLength == 0 then
                                        -- first
                                        list

                                    else
                                        let
                                            (BFCommandStack innerStackList) =
                                                BFCommand (BFToken LoopEnd "(Loop wasn't closed)" <| Just InsufficientLoopEnd)
                                                    |> finalizeLoopCommand (BFCommandStack [ current, list ])
                                        in
                                        List.head innerStackList
                                            |> Maybe.withDefault (BFCommandList [])

                                (BFCommandStack stackList) =
                                    memo

                                result =
                                    List.foldl finalizer (BFCommandList []) stackList
                            in
                            Parser.Done (reverseCommandList result)
                        )
                ]
        )


parseTokens : BFTokenTable -> String -> Result (List Parser.DeadEnd) (Array BFCommand)
parseTokens table =
    Parser.run (parseTokensHelper table)
