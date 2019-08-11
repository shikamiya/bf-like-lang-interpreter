module Main exposing (main)

import Array exposing (Array)
import BFExecutor exposing (runBFCommandByStep, runBFCommands)
import BFParser exposing (parseTokens)
import BFTypes exposing (BFCommand(..), BFParseError(..), BFRunningState, BFTape(..), BFTokenKind(..), BFTokenTable, initialRunningState)
import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Popover as Popover
import Bootstrap.Tab as Tab
import Bootstrap.Utilities.Spacing as Spacing
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
import Task



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
        [ Dropdown.subscriptions model.parserTokenTableState.dropdownState <| UpdateParserTokenTableState << UpdateTokenTableDropdownState
        , Dropdown.subscriptions model.displayTokenTableState.dropdownState <| UpdateDisplayTokenTableState << UpdateTokenTableDropdownState
        ]



-- SubModels / Update


type alias TokenTableState =
    { dropdownState : Dropdown.State
    , tokenTable : BFTokenTable
    }


type TokenTableStateMsg
    = UpdateTokenTableDropdownState Dropdown.State
    | UpdateTokenTable BFTokenTable


updateTokenTableState : TokenTableState -> TokenTableStateMsg -> TokenTableState
updateTokenTableState tokenTableDropdown msg =
    case msg of
        UpdateTokenTableDropdownState state ->
            { tokenTableDropdown | dropdownState = state }

        UpdateTokenTable tokenTable ->
            { tokenTableDropdown | tokenTable = tokenTable }


initialTokenTableState : TokenTableState
initialTokenTableState =
    { dropdownState = Dropdown.initialState
    , tokenTable = Language.BF.table
    }


type BFRunningStateMsg
    = UpdateTokens (Array BFCommand)
    | UpdateInput String
    | UpdatePopoverIndices (List Int)
    | ResetAll
    | ResetRunnningState
    | Run
    | StepRun


updateRunningState : BFRunningState -> BFRunningStateMsg -> BFRunningState
updateRunningState state msg =
    case msg of
        UpdateTokens commands ->
            { state | commands = commands }

        UpdateInput input ->
            { state | input = input }

        UpdatePopoverIndices pos ->
            { state | popoverIndices = pos }

        ResetAll ->
            initialRunningState

        ResetRunnningState ->
            { initialRunningState | commands = state.commands, input = state.input }

        Run ->
            let
                initState =
                    updateRunningState state ResetRunnningState
            in
            runBFCommands initState

        StepRun ->
            runBFCommandByStep state



-- Model


type alias Model =
    { programContent : String
    , tabState : Tab.State
    , displayNoOpCommand : Bool
    , popoverState : Popover.State
    , parserTokenTableState : TokenTableState
    , displayTokenTableState : TokenTableState
    , runningState : BFRunningState
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
    , tabState = Tab.initialState
    , displayNoOpCommand = True
    , popoverState = Popover.initialState
    , parserTokenTableState = initialTokenTableState
    , displayTokenTableState = initialTokenTableState
    , runningState = initialRunningState
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
    = ChangeProgramContent String
    | UpdateProgramContent String
    | UpdateTabState Tab.State
    | UpdatePopoverState Popover.State
    | ChangePopoverState (List Int) Popover.State
    | UpdateParserTokenTableState TokenTableStateMsg
    | UpdateDisplayTokenTableState TokenTableStateMsg
    | ChangeNoOpCommandVisibility Bool
    | ParseTokens
    | UpdateRunningState BFRunningStateMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeProgramContent programContent ->
            update (UpdateProgramContent programContent) model
                |> Tuple.first
                |> update ParseTokens
                |> Tuple.first
                |> withCacheCmd

        UpdateProgramContent programContent ->
            { model | programContent = programContent }
                |> withCacheCmd

        UpdateTabState state ->
            { model | tabState = state }
                |> withCmdNone

        UpdatePopoverState state ->
            { model | popoverState = state }
                |> withCmdNone

        ChangePopoverState pos state ->
            ( update (UpdateRunningState <| UpdatePopoverIndices pos) model |> Tuple.first, Cmd.batch [ Task.perform (always (UpdatePopoverState state)) (Task.succeed ()) ] )

        UpdateParserTokenTableState tokenTableStateMsg ->
            let
                state =
                    updateTokenTableState model.parserTokenTableState tokenTableStateMsg
            in
            { model | parserTokenTableState = state }
                |> withCmdNone

        UpdateDisplayTokenTableState tokenTableStateMsg ->
            let
                state =
                    updateTokenTableState model.displayTokenTableState tokenTableStateMsg
            in
            { model | displayTokenTableState = state }
                |> withCmdNone

        ChangeNoOpCommandVisibility visibility ->
            { model | displayNoOpCommand = visibility }
                |> withCmdNone

        ParseTokens ->
            let
                commands =
                    parseTokens model.parserTokenTableState.tokenTable model.programContent
                        |> Result.withDefault model.runningState.commands
            in
            update (UpdateRunningState <| UpdateTokens commands) model
                |> Tuple.first
                |> withCmdNone

        UpdateRunningState runningStateMsg ->
            let
                state =
                    updateRunningState model.runningState runningStateMsg
            in
            { model | runningState = state }
                |> withCmdNone



-- View


view : Model -> Html Msg
view model =
    Grid.containerFluid []
        [ Bootstrap.CDN.stylesheet
        , Grid.row []
            [ Grid.col [ Col.sm ]
                [ Html.h1 []
                    [ text "BF/Ook! like language interpreter/transpiler" ]
                ]
            ]
        , Tab.config UpdateTabState
            |> Tab.items
                [ viewOfMainTabItem model
                , viewOfDebugTabItem model
                ]
            |> Tab.view model.tabState
        ]


viewOfMainTabItem : Model -> Tab.Item Msg
viewOfMainTabItem model =
    Tab.item
        { id = "mainTabItem"
        , link = Tab.link [] [ text "Executor" ]
        , pane =
            Tab.pane [ Spacing.mt3 ]
                [ Grid.row []
                    [ Grid.col [ Col.lg6 ]
                        [ Card.config []
                            |> Card.header []
                                [ Dropdown.dropdown
                                    model.parserTokenTableState.dropdownState
                                    { options = []
                                    , toggleMsg = UpdateParserTokenTableState << UpdateTokenTableDropdownState
                                    , toggleButton =
                                        Dropdown.toggle [ Button.primary ] [ text <| Tuple.second model.parserTokenTableState.tokenTable ]
                                    , items =
                                        List.map (viewOfBFTokenTableItem <| UpdateParserTokenTableState << UpdateTokenTable) bfTokenTableList
                                    }
                                ]
                            |> Card.block []
                                [ Block.custom <|
                                    Textarea.textarea
                                        [ Textarea.rows 15
                                        , Textarea.onInput ChangeProgramContent
                                        , Textarea.value model.programContent
                                        ]
                                ]
                            |> Card.view
                        ]
                    , Grid.col [ Col.lg6 ]
                        [ Card.config [ Card.attrs [ Html.Attributes.class "h-100" ] ]
                            |> Card.header []
                                [ Grid.row []
                                    [ Grid.col []
                                        [ text "Parsed commands by: "
                                        , Dropdown.dropdown
                                            model.displayTokenTableState.dropdownState
                                            { options = []
                                            , toggleMsg = UpdateDisplayTokenTableState << UpdateTokenTableDropdownState
                                            , toggleButton =
                                                Dropdown.toggle [ Button.primary ] [ text <| Tuple.second model.displayTokenTableState.tokenTable ]
                                            , items =
                                                List.map (viewOfBFTokenTableItem <| UpdateDisplayTokenTableState << UpdateTokenTable) bfTokenTableList
                                            }
                                        ]
                                    , Grid.col []
                                        [ text " Comments: "
                                        , ButtonGroup.radioButtonGroup [ ButtonGroup.small ]
                                            [ ButtonGroup.radioButton
                                                (model.displayNoOpCommand == True)
                                                [ Button.primary, Button.onClick <| ChangeNoOpCommandVisibility True ]
                                                [ text "Show" ]
                                            , ButtonGroup.radioButton
                                                (model.displayNoOpCommand == False)
                                                [ Button.primary, Button.onClick <| ChangeNoOpCommandVisibility False ]
                                                [ text "Hide" ]
                                            ]
                                        ]
                                    ]
                                ]
                            |> Card.block []
                                [ viewOfBFCommands model [] model.runningState.commands
                                    |> Html.p []
                                    |> Block.custom
                                ]
                            |> Card.view
                        ]
                    ]
                , Grid.row [ Row.attrs [ Html.Attributes.class "my-3" ] ]
                    [ Grid.col []
                        [ Button.button [ Button.onClick (UpdateRunningState Run) ] [ text "RunFromStart" ]
                        , Button.button [ Button.onClick (UpdateRunningState StepRun) ] [ text "StepRun" ]
                        , Button.button [ Button.onClick (UpdateRunningState ResetAll) ] [ text "ResetAll" ]
                        , Button.button [ Button.onClick (UpdateRunningState ResetRunnningState) ] [ text "ResetStepRunPosition" ]
                        ]
                    ]
                , Grid.row []
                    [ Grid.col [ Col.lg6 ]
                        [ Card.config []
                            |> Card.header [] [ text "Input" ]
                            |> Card.block []
                                [ Block.custom <|
                                    Textarea.textarea
                                        [ Textarea.rows 5
                                        , Textarea.onInput (UpdateRunningState << UpdateInput)
                                        , Textarea.value model.runningState.input
                                        ]
                                ]
                            |> Card.view
                        ]
                    , Grid.col [ Col.lg6 ]
                        [ Card.config []
                            |> Card.header [] [ text "Output" ]
                            |> Card.block []
                                [ Html.p []
                                    (model.runningState.output
                                        |> List.reverse
                                        |> List.map String.fromChar
                                        |> List.map
                                            (\str ->
                                                if str == "\n" then
                                                    Html.br [] []

                                                else
                                                    text str
                                            )
                                        |> (\x -> List.append x [ Html.span [ Html.Attributes.class "text-danger" ] [ text <| Maybe.withDefault "" model.runningState.error ] ])
                                    )
                                    |> Block.custom
                                ]
                            |> Card.view
                        ]
                    ]
                ]
        }


viewOfDebugTabItem : Model -> Tab.Item Msg
viewOfDebugTabItem _ =
    Tab.item
        { id = "debugTabItem"
        , link = Tab.link [] [ text "Debug" ]
        , pane =
            Tab.pane [ Spacing.mt3 ]
                [ Button.button [ Button.onClick <| ChangeProgramContent "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>." ] [ text "BF Hello world program" ] ]
        }


viewOfBFTokenTableItem : (BFTokenTable -> Msg) -> BFTokenTable -> Dropdown.DropdownItem Msg
viewOfBFTokenTableItem msg table =
    Dropdown.buttonItem [ Html.Events.onClick <| msg table ] [ text <| Tuple.second table ]


viewOfBFCommands : Model -> List Int -> Array BFCommand -> List (Html Msg)
viewOfBFCommands model pos cmds =
    Array.indexedMap (\idx -> viewOfBFCommand model (idx :: pos)) cmds
        |> Array.toList
        |> List.concat


viewOfBFCommand : Model -> List Int -> BFCommand -> List (Html Msg)
viewOfBFCommand model pos cmd =
    let
        isCurrentCommand =
            model.runningState.currentIndices == pos

        isCurrentPopoverCommand =
            model.runningState.popoverIndices == pos

        depth =
            List.length pos - 1

        spacing =
            Html.span [ Html.Attributes.class "ml-2" ] []

        brWithSpacings indent =
            Html.br [] [] :: List.repeat indent spacing
    in
    case cmd of
        BFCommand token ->
            let
                isError =
                    case token.error of
                        Just _ ->
                            True

                        Nothing ->
                            False
            in
            case token.kind of
                NoOp ->
                    let
                        visible =
                            isError || model.displayNoOpCommand
                    in
                    if token.value == "\n" && visible then
                        brWithSpacings depth

                    else
                        Html.span
                            [ Html.Attributes.classList
                                [ ( "text-muted", not isError )
                                , ( "text-danger", isError )
                                , ( "font-weight-bold", isError )
                                , ( "d-none", not visible )
                                ]
                            ]
                            [ text token.value ]
                            |> List.singleton

                _ ->
                    let
                        displayValue =
                            List.filter (\table -> token.kind == Tuple.first table) (Tuple.first model.displayTokenTableState.tokenTable)
                                |> List.head
                                |> Maybe.withDefault ( token.kind, token.value )
                                |> Tuple.second
                    in
                    Html.div [ Html.Attributes.classList [ ( "my-hidden-popover", not isCurrentPopoverCommand ), ( "d-inline-block", True ) ] ]
                        (Popover.config
                            (Html.span
                                (Html.Attributes.classList
                                    [ ( "text-dark", not isCurrentCommand && not isError )
                                    , ( "text-success", isCurrentCommand )
                                    , ( "text-danger", isError )
                                    , ( "font-weight-bold", True )
                                    ]
                                    :: Popover.onClick model.popoverState (ChangePopoverState pos)
                                )
                                [ text displayValue ]
                            )
                            |> Popover.titleH4 [] [ text "Command Information" ]
                            |> Popover.content []
                                [ text "Value: "
                                , text token.value
                                , Html.br [] []
                                , text "Position: "
                                , text (List.reverse pos |> List.map String.fromInt |> String.join ", ")
                                ]
                            |> Popover.view model.popoverState
                            |> List.singleton
                        )
                        |> List.singleton

        BFLoopFunc commands ->
            let
                children =
                    viewOfBFCommands model pos commands
            in
            List.concat [ brWithSpacings <| depth + 1, children, brWithSpacings depth ]
