module BFParser exposing (parseTokens)

import Array exposing (Array)
import BFTypes exposing (BFCommand(..), BFParseError(..), BFTape(..), BFToken, BFTokenKind(..), BFTokenTable)
import Parser exposing ((|.), Parser)


type BFCommandList
    = BFCommandList (List BFCommand)


type BFCommandStack
    = BFCommandStack (List BFCommandList)


type alias BFTokenParseMemo =
    { stack : BFCommandStack
    , index : Int
    }


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
                Parser.succeed (BFToken kind value Nothing Nothing)
                    |. Parser.token value
            )
        |> Parser.oneOf


parseNoOpToken : Parser BFToken
parseNoOpToken =
    Parser.chompIf (always True)
        |> Parser.getChompedString
        |> Parser.map (\x -> BFToken NoOp x Nothing Nothing)


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
    Parser.loop (BFTokenParseMemo (BFCommandStack [ BFCommandList [] ]) 0)
        (\memo ->
            Parser.oneOf
                [ parseTokenByTable cmdTable
                    |> Parser.map
                        (\token ->
                            let
                                index =
                                    memo.index

                                (BFCommandStack stackList) =
                                    memo.stack

                                depth =
                                    List.length stackList

                                stack =
                                    case token.kind of
                                        LoopStart ->
                                            beginNewLoopCommand memo.stack (BFCommand { token | index = Just index })

                                        LoopEnd ->
                                            if depth <= 1 then
                                                addCommandIntoCurrentList memo.stack (BFCommand { token | error = Just TooManyLoopEnd })

                                            else
                                                finalizeLoopCommandWithFullStack memo.stack (BFCommand { token | index = Just index })

                                        _ ->
                                            addCommandIntoCurrentList memo.stack (BFCommand { token | index = Just index })
                            in
                            Parser.Loop { memo | stack = stack, index = index + 1 }
                        )
                , parseNoOpToken
                    |> Parser.map
                        (\token ->
                            let
                                stack =
                                    addCommandIntoCurrentList memo.stack (BFCommand token)
                            in
                            Parser.Loop { memo | stack = stack }
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
                                                BFCommand (BFToken LoopEnd "(Loop wasn't closed)" Nothing <| Just InsufficientLoopEnd)
                                                    |> finalizeLoopCommand (BFCommandStack [ current, list ])
                                        in
                                        List.head innerStackList
                                            |> Maybe.withDefault (BFCommandList [])

                                (BFCommandStack stackList) =
                                    memo.stack

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
