module BFParser exposing (convertBFCommandsToString, parseTokens)

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
addCommandIntoList (BFCommandList commands) cmd =
    BFCommandList <| cmd :: commands


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
reverseCommandList (BFCommandList commands) =
    List.reverse commands
        |> Array.fromList


beginNewLoopCommand : BFCommandStack -> BFCommand -> BFCommandStack
beginNewLoopCommand (BFCommandStack stackList) cmd =
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
                            Parser.Loop <|
                                case token.kind of
                                    LoopStart ->
                                        beginNewLoopCommand memo (BFCommand token)

                                    LoopEnd ->
                                        if getBFStackLength memo <= 1 then
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
                        )
                , Parser.map
                    (Parser.Loop
                        << addCommandIntoCurrentList memo
                        << BFCommand
                    )
                    parseNoOpToken
                , Parser.end
                    |> Parser.map
                        (\_ ->
                            let
                                (BFCommandStack stackList) =
                                    memo
                            in
                            Parser.Done (reverseCommandList <| List.foldl finalizer (BFCommandList []) stackList)
                        )
                ]
        )


finalizer : BFCommandList -> BFCommandList -> BFCommandList
finalizer list current =
    if getBFCommandListLength current == 0 then
        list

    else
        let
            (BFCommandStack innerStackList) =
                BFCommand (BFToken LoopEnd "(Loop wasn't closed)" <| Just InsufficientLoopEnd)
                    |> finalizeLoopCommand (BFCommandStack [ current, list ])
        in
        List.head innerStackList
            |> Maybe.withDefault (BFCommandList [])


getBFStackLength : BFCommandStack -> Int
getBFStackLength (BFCommandStack commands) =
    List.length commands


getBFCommandListLength : BFCommandList -> Int
getBFCommandListLength (BFCommandList commands) =
    List.length commands


parseTokens : BFTokenTable -> String -> Result (List Parser.DeadEnd) (Array BFCommand)
parseTokens table =
    Parser.run (parseTokensHelper table)


convertBFCommandsToString : BFTokenTable -> Array BFCommand -> String
convertBFCommandsToString table commands =
    Array.map (convertBFCommandToString table) commands
        |> Array.toList
        |> String.concat


convertBFCommandToString : BFTokenTable -> BFCommand -> String
convertBFCommandToString table command =
    case command of
        BFCommand token ->
            Tuple.first table
                |> List.filter ((==) token.kind << Tuple.first)
                |> List.head
                |> Maybe.withDefault ( NoOp, "" )
                |> Tuple.second

        BFLoopFunc commands ->
            convertBFCommandsToString table commands
