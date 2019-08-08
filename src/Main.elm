module Main exposing (main)

import Array
import BFParser exposing (bfParseErrorToString, parseTokens)
import BFRunner exposing (bfRun, bfStepRun, initialRunningState)
import BFTypes exposing (BFCommand(..), BFParseError(..), BFRunningState, BFTape(..), BFTokenKind(..), BFTokenTable)
import Bootstrap.Button as Button
import Bootstrap.CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
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


bfTokenTableList : List BFTokenTable
bfTokenTableList =
    [ Language.BrainFuck.table
    , Language.HogyLang.table
    , Language.Ook.table
    ]



-- Main


main : Program Json.Decode.Value Model Msg
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
    , dropdownStates : DropdownStates
    , state : BFRunningState
    }


type alias DropdownStates =
    { parserTokenTable : Dropdown.State
    , displayTokenTable : Dropdown.State
    }


init : Json.Decode.Value -> ( Model, Cmd Msg )
init flags =
    decodeModel flags
        |> withCmdNone


initialModel : Model
initialModel =
    { programContent = ""
    , parserTokenTable = Language.BrainFuck.table
    , dropdownStates = initialDropdownStates
    , state = initialRunningState
    }


initialDropdownStates : DropdownStates
initialDropdownStates =
    { parserTokenTable = Dropdown.initialState
    , displayTokenTable = Dropdown.initialState
    }


encodeModel : Model -> Json.Encode.Value
encodeModel model =
    Json.Encode.object
        [ ( "programContent", Json.Encode.string model.programContent ) ]


decodeModel : Json.Decode.Value -> Model
decodeModel value =
    let
        programContent =
            Json.Decode.decodeValue Json.Decode.string value
                |> Result.withDefault ""
                |> Json.Decode.decodeString (Json.Decode.field "programContent" Json.Decode.string)
                |> Result.withDefault ""
    in
    { initialModel | programContent = programContent }


withCmdNone : Model -> ( Model, Cmd msg )
withCmdNone model =
    ( model, Cmd.none )


withCacheCmd : Model -> ( Model, Cmd msg )
withCacheCmd model =
    ( model
    , Cmd.batch
        [ encodeModel model
            |> cache
        ]
    )



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
                |> withCacheCmd

        UpdateInput input ->
            let
                state =
                    model.state
            in
            { model | state = { state | input = input } }
                |> withCmdNone

        UpdateParserTokenTable table ->
            { model | parserTokenTable = table }
                |> withCmdNone

        UpdateParserTokenTableDropDownState state ->
            let
                dropdownStates =
                    model.dropdownStates

                newDropdownStates =
                    { dropdownStates | parserTokenTable = state }
            in
            { model | dropdownStates = newDropdownStates }
                |> withCmdNone

        ResetAll ->
            initialModel
                |> withCmdNone

        ResetRunnningState ->
            { model | state = { initialRunningState | commands = model.state.commands, input = model.state.input } }
                |> withCmdNone

        Run ->
            let
                state =
                    bfRun { initialRunningState | commands = model.state.commands, input = model.state.input }
            in
            { model | state = state }
                |> withCmdNone

        StepRun ->
            let
                state =
                    bfStepRun model.state
            in
            { model | state = state }
                |> withCmdNone



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
            [ Grid.col [ Col.lg6 ]
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
            , Grid.col [ Col.lg6 ]
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
            [ Grid.col [ Col.lg6 ]
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
            , Grid.col [ Col.lg6 ]
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


viewOfBFTokenTableItem : BFTokenTable -> Dropdown.DropdownItem Msg
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
