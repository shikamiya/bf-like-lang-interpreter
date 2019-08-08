module Main exposing (Model, Msg(..), bfTokenTableList, init, initialModel, main, subscriptions, update, view, viewOfBFCommand, viewOfBFTokenTableItem)

import Array exposing (Array)
import BFParser exposing (..)
import BFRunner exposing (..)
import BFTypes exposing (..)
import Bootstrap.Button as Button
import Bootstrap.CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Text as Text
import Browser
import Cacher exposing (cache)
import Html exposing (Html, text)
import Html.Attributes
import Html.Events
import Json.Decode
import Json.Encode
import Language.BrainFuck
import Language.HogyLang
import Language.Ook



-- Config


bfTokenTableList =
    [ Language.BrainFuck.table
    , Language.HogyLang.table
    , Language.Ook.table
    ]



-- Main


main : Program (Maybe String) Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Dropdown.subscriptions model.dropdownStates.parserTokenTable UpdateParserTokenTableDropDownState ]



-- Model


type alias Model =
    { programContent : String
    , parserTokenTable : BFTokenTable
    , dropdownStates :
        { parserTokenTable : Dropdown.State
        }
    , state : BFRunningState
    }


init : Maybe String -> ( Model, Cmd mgs )
init maybeCache =
    ( { initialModel
        | programContent =
            Maybe.withDefault "" maybeCache
                |> Json.Decode.decodeString Json.Decode.string
                |> Result.withDefault ""
      }
    , Cmd.none
    )


initialModel : Model
initialModel =
    { programContent = ""
    , parserTokenTable = Language.BrainFuck.table
    , dropdownStates = { parserTokenTable = Dropdown.initialState }
    , state = initialRunningState
    }



-- Update


type Msg
    = UpdateProgramContent String
    | UpdateInput String
    | UpdateParserTokenTable BFTokenTable
    | UpdateParserTokenTableDropDownState Dropdown.State
    | ResetAll
    | ResetRunnningState
    | Run
    | StepRun


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    (\x -> ( x, Cmd.batch [ cache <| Json.Encode.string x.programContent ] )) <|
        case msg of
            UpdateProgramContent programContent ->
                let
                    commands =
                        parseTokens model.parserTokenTable programContent
                            |> Result.withDefault model.state.commands

                    state =
                        model.state
                in
                { model | programContent = programContent, state = { state | commands = commands } }

            UpdateInput input ->
                let
                    state =
                        model.state
                in
                { model | state = { state | input = input } }

            UpdateParserTokenTable table ->
                { model | parserTokenTable = table }

            UpdateParserTokenTableDropDownState state ->
                let
                    dropdownStates =
                        model.dropdownStates

                    newDropdownStates =
                        { dropdownStates | parserTokenTable = state }
                in
                { model | dropdownStates = newDropdownStates }

            ResetAll ->
                initialModel

            ResetRunnningState ->
                { model | state = { initialRunningState | commands = model.state.commands, input = model.state.input } }

            Run ->
                let
                    state =
                        bfRun { initialRunningState | commands = model.state.commands, input = model.state.input }
                in
                { model | state = state }

            StepRun ->
                let
                    state =
                        bfStepRun model.state
                in
                { model | state = state }



-- View


view : Model -> Html Msg
view model =
    Grid.containerFluid []
        [ Bootstrap.CDN.stylesheet
        , Grid.row []
            [ Grid.col [ Col.sm ]
                [ Html.h1 []
                    [ text "Brainfuck/Ook! like language interpreter"
                    ]
                ]
            ]
        , Grid.row []
            [ Grid.col [ Col.sm6 ]
                [ Card.config []
                    |> Card.header []
                        [ Dropdown.dropdown
                            model.dropdownStates.parserTokenTable
                            { options = []
                            , toggleMsg = UpdateParserTokenTableDropDownState
                            , toggleButton =
                                Dropdown.toggle [ Button.primary ] [ text <| Tuple.second model.parserTokenTable ]
                            , items =
                                List.map viewOfBFTokenTableItem bfTokenTableList
                            }
                        ]
                    |> Card.block []
                        [ Block.custom <|
                            Textarea.textarea
                                [ Textarea.rows 15
                                , Textarea.onInput UpdateProgramContent
                                , Textarea.value model.programContent
                                ]
                        ]
                    |> Card.view
                ]
            , Grid.col [ Col.sm6 ]
                [ Card.config [ Card.attrs [ Html.Attributes.class "h-100" ] ]
                    |> Card.header [] [ text "Parsed Brainfuck commands" ]
                    |> Card.block []
                        [ Array.map (viewOfBFCommand model.state.stepIndex 0) model.state.commands
                            |> Array.toList
                            |> List.concat
                            |> Html.p []
                            |> Block.custom
                        ]
                    |> Card.view
                ]
            ]
        , Grid.row [ Row.attrs [ Html.Attributes.class "my-3" ] ]
            [ Grid.col []
                [ Button.button [ Button.onClick Run ] [ text "RunFromStart" ]
                , Button.button [ Button.onClick StepRun ] [ text "StepRun" ]
                , Button.button [ Button.onClick ResetAll ] [ text "ResetAll" ]
                , Button.button [ Button.onClick ResetRunnningState ] [ text "ResetStepRunPosition" ]
                ]
            ]
        , Grid.row []
            [ Grid.col [ Col.sm6 ]
                [ Card.config []
                    |> Card.header [] [ text "Input" ]
                    |> Card.block []
                        [ Block.custom <|
                            Textarea.textarea
                                [ Textarea.rows 15
                                , Textarea.onInput UpdateInput
                                , Textarea.value model.state.input
                                ]
                        ]
                    |> Card.view
                ]
            , Grid.col [ Col.sm6 ]
                [ Card.config []
                    |> Card.header [] [ text "Output" ]
                    |> Card.block []
                        [ Html.p []
                            [ text model.state.output
                            , Html.span [ Html.Attributes.class "text-danger" ] [ text <| Maybe.withDefault "" model.state.error ]
                            ]
                            |> Block.custom
                        ]
                    |> Card.view
                ]
            ]
        ]


viewOfBFTokenTableItem table =
    Dropdown.buttonItem [ Html.Events.onClick <| UpdateParserTokenTable table ] [ text <| Tuple.second table ]


viewOfBFCommand : Maybe Int -> Int -> BFCommand -> List (Html Msg)
viewOfBFCommand stepIndex depth cmd =
    let
        newDepth =
            depth + 1

        br =
            Html.br [] []
                |> List.singleton

        spacing =
            Html.span [ Html.Attributes.class "ml-2" ] []

        nextSpacings =
            List.repeat newDepth spacing

        currentSpacings =
            List.repeat depth spacing
    in
    case cmd of
        BFCommand token ->
            let
                isError =
                    case bfParseErrorToString token.error of
                        Just _ ->
                            True

                        Nothing ->
                            False
            in
            case token.kind of
                NoOp ->
                    if token.value == "\n" then
                        Html.br [] [] :: currentSpacings

                    else
                        Html.span
                            [ Html.Attributes.classList
                                [ ( "text-muted", not isError )
                                , ( "text-danger", isError )
                                , ( "font-weight-bold", isError )
                                ]
                            ]
                            [ text token.value ]
                            |> List.singleton

                _ ->
                    let
                        isCurrentCommand =
                            stepIndex /= Nothing && token.index == stepIndex
                    in
                    Html.span
                        [ Html.Attributes.classList
                            [ ( "text-dark", not isCurrentCommand && not isError )
                            , ( "text-success", isCurrentCommand )
                            , ( "text-danger", isError )
                            , ( "font-weight-bold", True )
                            ]
                        ]
                        [ text token.value ]
                        |> List.singleton

        BFLoopFunc commands ->
            let
                children =
                    Array.map (viewOfBFCommand stepIndex newDepth) commands
                        |> Array.toList
                        |> List.concat
            in
            List.concat [ br, nextSpacings, children, br, currentSpacings ]
