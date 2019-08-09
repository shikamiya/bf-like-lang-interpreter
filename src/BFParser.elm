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
    Tuple.first table
        |> List.map
            (\x ->
                let
                    ( kind, value ) =
                        x
                in
                Parser.succeed (BFToken kind value Nothing)
                    |. Parser.token value
            )
        |> Parser.oneOf


parseNoOpToken : Parser BFToken
parseNoOpToken =
    always True
        |> Parser.chompIf
        |> Parser.getChompedString
        |> Parser.map (\value -> BFToken NoOp value Nothing)


addCommandIntoList : BFCommandList -> BFCommand -> BFCommandList
addCommandIntoList list cmd =
    let
        (BFCommandList commands) =
            list
    in
    cmd
        :: commands
        |> BFCommandList


addCommandIntoCurrentList : BFCommandStack -> BFCommand -> BFCommandStack
addCommandIntoCurrentList stack cmd =
    let
        ( current, ancestors ) =
            case stack of
                BFCommandStack (x :: xs) ->
                    ( x, xs )

                BFCommandStack [] ->
                    ( BFCommandList [], [] )
    in
    addCommandIntoList current cmd
        :: ancestors
        |> BFCommandStack


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
    BFCommandList (List.singleton cmd)
        :: stackList
        |> BFCommandStack


finalizeLoopCommand : BFCommandStack -> BFCommand -> BFCommandStack
finalizeLoopCommand stack cmd =
    let
        ( current, ancestors ) =
            case stack of
                BFCommandStack (x :: xs) ->
                    ( x, xs )

                BFCommandStack [] ->
                    ( BFCommandList [], [] )

        commands =
            reverseCommandList (addCommandIntoList current cmd)
    in
    addCommandIntoCurrentList (BFCommandStack ancestors) (BFLoopFunc commands)


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
                                                finalizeLoopCommand memo (BFCommand token)

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
