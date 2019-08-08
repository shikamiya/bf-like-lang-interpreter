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
import Language.BF
import Language.HogyLang
import Language.Ook



-- Config


bfTokenTableList : List BFTokenTable
bfTokenTableList =
    [ Language.BF.table
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
        [ Dropdown.subscriptions model.tokenTableStates.parser.dropdownState UpdateParserTokenTableDropDownState
        , Dropdown.subscriptions model.tokenTableStates.display.dropdownState UpdateDisplayTokenTableDropDownState
        ]



-- Model


type alias Model =
    { programContent : String
    , tokenTableStates : TokenTableDropdowns
    , state : BFRunningState
    }


type alias TokenTableDropdowns =
    { parser : TokenTableDropdown
    , display : TokenTableDropdown
    }


type alias TokenTableDropdown =
    { dropdownState : Dropdown.State
    , tokenTable : BFTokenTable
    }


init : Json.Decode.Value -> ( Model, Cmd Msg )
init flags =
    decodeModel flags
        |> update ParseTokens
        |> Tuple.first
        |> withCmdNone


initialModel : Model
initialModel =
    { programContent = ""
    , tokenTableStates = initialTokenTableStates
    , state = initialRunningState
    }


initialTokenTableStates : TokenTableDropdowns
initialTokenTableStates =
    { parser = initialTokenTableState
    , display = initialTokenTableState
    }


initialTokenTableState : TokenTableDropdown
initialTokenTableState =
    { dropdownState = Dropdown.initialState
    , tokenTable = Language.BF.table
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
    | ParseTokens
    | UpdateInput String
    | UpdateParserTokenTable BFTokenTable
    | UpdateParserTokenTableDropDownState Dropdown.State
    | UpdateDisplayTokenTable BFTokenTable
    | UpdateDisplayTokenTableDropDownState Dropdown.State
    | ResetAll
    | ResetRunnningState
    | Run
    | StepRun


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        UpdateProgramContent programContent ->
            { model | programContent = programContent }
                |> update ParseTokens
                |> Tuple.first
                |> withCacheCmd

        ParseTokens ->
            let
                commands =
                    parseTokens model.tokenTableStates.parser.tokenTable model.programContent
                        |> Result.withDefault model.state.commands

                state =
                    model.state
            in
            { model | state = { state | commands = commands } }
                |> withCmdNone

        UpdateInput input ->
            let
                state =
                    model.state
            in
            { model | state = { state | input = input } }
                |> withCmdNone

        UpdateParserTokenTable table ->
            let
                oldParser =
                    model.tokenTableStates.parser

                parser =
                    { oldParser | tokenTable = table }

                oldTokenTableStates =
                    model.tokenTableStates

                tokenTableStates =
                    { oldTokenTableStates | parser = parser }
            in
            { model | tokenTableStates = tokenTableStates }
                |> update ParseTokens
                |> Tuple.first
                |> withCmdNone

        UpdateParserTokenTableDropDownState state ->
            let
                oldParser =
                    model.tokenTableStates.parser

                parser =
                    { oldParser | dropdownState = state }

                oldTokenTableStates =
                    model.tokenTableStates

                tokenTableStates =
                    { oldTokenTableStates | parser = parser }
            in
            { model | tokenTableStates = tokenTableStates }
                |> withCmdNone

        UpdateDisplayTokenTable table ->
            let
                oldDisplay =
                    model.tokenTableStates.display

                display =
                    { oldDisplay | tokenTable = table }

                oldTokenTableStates =
                    model.tokenTableStates

                tokenTableStates =
                    { oldTokenTableStates | display = display }
            in
            { model | tokenTableStates = tokenTableStates }
                |> withCmdNone

        UpdateDisplayTokenTableDropDownState state ->
            let
                oldDisplay =
                    model.tokenTableStates.display

                display =
                    { oldDisplay | dropdownState = state }

                oldTokenTableStates =
                    model.tokenTableStates

                tokenTableStates =
                    { oldTokenTableStates | display = display }
            in
            { model | tokenTableStates = tokenTableStates }
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
                    [ text "BF/Ook! like language interpreter/transpiler"
                    ]
                ]
            ]
        , Grid.row []
            [ Grid.col [ Col.lg6 ]
                [ Card.config []
                    |> Card.header []
                        [ Dropdown.dropdown
                            model.tokenTableStates.parser.dropdownState
                            { options = []
                            , toggleMsg = UpdateParserTokenTableDropDownState
                            , toggleButton =
                                Dropdown.toggle [ Button.primary ] [ text <| Tuple.second model.tokenTableStates.parser.tokenTable ]
                            , items =
                                List.map (viewOfBFTokenTableItem UpdateParserTokenTable) bfTokenTableList
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
                    |> Card.header []
                        [ text "Parsed commands by: "
                        , Dropdown.dropdown
                            model.tokenTableStates.display.dropdownState
                            { options = []
                            , toggleMsg = UpdateDisplayTokenTableDropDownState
                            , toggleButton =
                                Dropdown.toggle [ Button.primary ] [ text <| Tuple.second model.tokenTableStates.display.tokenTable ]
                            , items =
                                List.map (viewOfBFTokenTableItem UpdateDisplayTokenTable) bfTokenTableList
                            }
                        ]
                    |> Card.block []
                        [ Array.map (viewOfBFCommand model 0) model.state.commands
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


viewOfBFTokenTableItem : (BFTokenTable -> Msg) -> BFTokenTable -> Dropdown.DropdownItem Msg
viewOfBFTokenTableItem msg table =
    Dropdown.buttonItem [ Html.Events.onClick <| msg table ] [ text <| Tuple.second table ]


viewOfBFCommand : Model -> Int -> BFCommand -> List (Html Msg)
viewOfBFCommand model depth cmd =
    let
        stepIndex =
            model.state.stepIndex

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

                        displayValue =
                            List.filter (\table -> token.kind == Tuple.first table) (Tuple.first model.tokenTableStates.display.tokenTable)
                                |> List.head
                                |> Maybe.withDefault ( token.kind, token.value )
                                |> Tuple.second
                    in
                    Html.span
                        [ Html.Attributes.classList
                            [ ( "text-dark", not isCurrentCommand && not isError )
                            , ( "text-success", isCurrentCommand )
                            , ( "text-danger", isError )
                            , ( "font-weight-bold", True )
                            ]
                        ]
                        [ text displayValue ]
                        |> List.singleton

        BFLoopFunc commands ->
            let
                children =
                    Array.map (viewOfBFCommand model newDepth) commands
                        |> Array.toList
                        |> List.concat
            in
            List.concat [ br, nextSpacings, children, br, currentSpacings ]
