module Main exposing (main)

import Array exposing (Array)
import BFExecutor exposing (getMaybeTapeValue, runBFCommandByStep, runBFCommands)
import BFParser exposing (parseTokens)
import BFTypes exposing (BFCommand(..), BFParseError(..), BFRunningState, BFTape(..), BFToken, BFTokenKind(..), BFTokenTable, bfParseErrorToString, initialRunningState, tapePages, tapeSize, tokenKindToString)
import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Popover as Popover
import Bootstrap.Tab as Tab
import Bootstrap.Table as Table
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


updateTokenTableState : TokenTableStateMsg -> TokenTableState -> TokenTableState
updateTokenTableState msg tokenTableDropdown =
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
    | ChangeCurrentTapePage
    | UpdateCurrentTapePage Int
    | ResetAll
    | ResetRunnningState
    | Run
    | StepRun


updateRunningState : BFRunningStateMsg -> BFRunningState -> BFRunningState
updateRunningState msg state =
    case msg of
        UpdateTokens commands ->
            { state | commands = commands }

        UpdateInput input ->
            { state | input = input }

        UpdatePopoverIndices pos ->
            { state | popoverIndices = pos }

        ChangeCurrentTapePage ->
            let
                currentPage =
                    state.tapePointer // (16 * 16)
            in
            updateRunningState (UpdateCurrentTapePage currentPage) state

        UpdateCurrentTapePage page ->
            let
                newPage =
                    modBy tapePages page
            in
            { state | currentTapePage = newPage }

        ResetAll ->
            initialRunningState

        ResetRunnningState ->
            { initialRunningState | commands = state.commands, input = state.input }

        Run ->
            let
                initState =
                    updateRunningState ResetRunnningState state
            in
            runBFCommands initState
                |> updateRunningState ChangeCurrentTapePage

        StepRun ->
            runBFCommandByStep state



-- Model


type alias Model =
    { programContent : String
    , tabState : Tab.State
    , displayNoOpCommand : Bool
    , popoverState : Popover.State
    , showBFTapeAs : ShowBFTapeAs
    , parserTokenTableState : TokenTableState
    , displayTokenTableState : TokenTableState
    , runningState : BFRunningState
    }


type ShowBFTapeAs
    = ShowBFTapeAsInt
    | ShowBFTapeAsHex
    | ShowBFTapeAsStr


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
    , showBFTapeAs = ShowBFTapeAsInt
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
    | ChangeNoOpCommandVisibility Bool
    | UpdatePopoverState Popover.State
    | ChangePopoverState (List Int) Popover.State
    | UpdateHowShowBFTapeAs ShowBFTapeAs
    | UpdateParserTokenTableState TokenTableStateMsg
    | UpdateDisplayTokenTableState TokenTableStateMsg
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

        ChangeNoOpCommandVisibility visibility ->
            { model | displayNoOpCommand = visibility }
                |> withCmdNone

        UpdatePopoverState state ->
            { model | popoverState = state }
                |> withCmdNone

        ChangePopoverState pos state ->
            ( update (UpdateRunningState <| UpdatePopoverIndices pos) model |> Tuple.first, Cmd.batch [ Task.perform (always (UpdatePopoverState state)) (Task.succeed ()) ] )

        UpdateHowShowBFTapeAs state ->
            { model | showBFTapeAs = state }
                |> withCmdNone

        UpdateParserTokenTableState tokenTableStateMsg ->
            let
                state =
                    updateTokenTableState tokenTableStateMsg model.parserTokenTableState
            in
            { model | parserTokenTableState = state }
                |> update ParseTokens
                |> Tuple.first
                |> withCmdNone

        UpdateDisplayTokenTableState tokenTableStateMsg ->
            let
                state =
                    updateTokenTableState tokenTableStateMsg model.displayTokenTableState
            in
            { model | displayTokenTableState = state }
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
                    updateRunningState runningStateMsg model.runningState
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
                                [ text "Program Input : Parse as "
                                , Dropdown.dropdown
                                    model.parserTokenTableState.dropdownState
                                    { options = []
                                    , toggleMsg = UpdateParserTokenTableState << UpdateTokenTableDropdownState
                                    , toggleButton =
                                        Dropdown.toggle [ Button.primary, Button.small ] [ text <| Tuple.second model.parserTokenTableState.tokenTable ]
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
                        [ Grid.row []
                            [ Grid.col []
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
                            ]
                        , Grid.row []
                            [ Grid.col []
                                [ Card.config [ Card.attrs [ Spacing.mt2 ] ]
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
                        [ Card.config [ Card.attrs [ Html.Attributes.class "h-100" ] ]
                            |> Card.header []
                                [ Grid.row []
                                    [ Grid.col []
                                        [ text "Parsed Program : Display as "
                                        , Dropdown.dropdown
                                            model.displayTokenTableState.dropdownState
                                            { options = []
                                            , toggleMsg = UpdateDisplayTokenTableState << UpdateTokenTableDropdownState
                                            , toggleButton =
                                                Dropdown.toggle [ Button.primary, Button.small ] [ text <| Tuple.second model.displayTokenTableState.tokenTable ]
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
                    , Grid.col [ Col.lg6 ]
                        [ Card.config []
                            |> Card.header []
                                [ Grid.row []
                                    [ Grid.col [ Col.sm2 ] [ text "Tape Status" ]
                                    , Grid.col [ Col.sm6 ]
                                        [ text " Display value as: "
                                        , ButtonGroup.radioButtonGroup [ ButtonGroup.small ]
                                            [ ButtonGroup.radioButton
                                                (model.showBFTapeAs == ShowBFTapeAsInt)
                                                [ Button.primary, Button.onClick <| UpdateHowShowBFTapeAs ShowBFTapeAsInt ]
                                                [ text "Int" ]
                                            , ButtonGroup.radioButton
                                                (model.showBFTapeAs == ShowBFTapeAsHex)
                                                [ Button.primary, Button.onClick <| UpdateHowShowBFTapeAs ShowBFTapeAsHex ]
                                                [ text "Hex" ]
                                            , ButtonGroup.radioButton
                                                (model.showBFTapeAs == ShowBFTapeAsStr)
                                                [ Button.primary, Button.onClick <| UpdateHowShowBFTapeAs ShowBFTapeAsStr ]
                                                [ text "Char" ]
                                            ]
                                        ]
                                    , Grid.col [ Col.sm4 ]
                                        [ InputGroup.config
                                            (InputGroup.text [ Input.value <| String.fromInt model.runningState.currentTapePage, Input.onInput (String.toInt >> Maybe.withDefault 0 >> UpdateCurrentTapePage >> UpdateRunningState) ])
                                            |> InputGroup.small
                                            |> InputGroup.predecessors
                                                [ InputGroup.button [ Button.secondary, Button.onClick <| UpdateRunningState <| UpdateCurrentTapePage <| model.runningState.currentTapePage - 1 ] [ text "<" ] ]
                                            |> InputGroup.successors
                                                [ InputGroup.span [] [ text "/ ", text <| String.fromInt <| tapePages - 1 ]
                                                , InputGroup.button [ Button.secondary, Button.onClick <| UpdateRunningState <| UpdateCurrentTapePage <| model.runningState.currentTapePage + 1 ] [ text ">" ]
                                                ]
                                            |> InputGroup.view
                                        ]
                                    ]
                                ]
                            |> Card.block []
                                [ Html.div []
                                    (List.map
                                        (\idx ->
                                            let
                                                line =
                                                    model.runningState.currentTapePage * 16 + idx
                                            in
                                            Grid.row []
                                                [ Grid.col []
                                                    [ tableViewOfTapeLine model line
                                                    ]
                                                ]
                                        )
                                     <|
                                        List.range 0 15
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


tableViewOfTapeLine : Model -> Int -> Html msg
tableViewOfTapeLine model line =
    let
        list =
            List.range 0 15

        header =
            List.map
                (\idx ->
                    let
                        address =
                            16 * line + idx

                        isCurrentAddress =
                            model.runningState.tapePointer == address

                        addressStr =
                            if address < tapeSize then
                                String.fromInt address

                            else
                                ""
                    in
                    Table.th [ Table.cellAttr <| Html.Attributes.classList [ ( "text-success", isCurrentAddress ) ] ] [ text addressStr ]
                )
                list

        body =
            List.map
                (\idx ->
                    let
                        address =
                            16 * line + idx

                        value =
                            getMaybeTapeValue model.runningState.tape address
                                |> Maybe.map (convertTapeValue model.showBFTapeAs)
                                |> Maybe.withDefault ""
                    in
                    Table.td [] [ text value ]
                )
                list
    in
    Table.table
        { options = [ Table.small ]
        , thead = Table.simpleThead header
        , tbody = Table.tbody [] [ Table.tr [] body ]
        }


convertTapeValue : ShowBFTapeAs -> Int -> String
convertTapeValue showAs value =
    case showAs of
        ShowBFTapeAsInt ->
            String.fromInt value

        ShowBFTapeAsHex ->
            convertCharIntoHexString value

        ShowBFTapeAsStr ->
            Char.fromCode value
                |> String.fromChar


convertIntIntoHexChar : Int -> Char
convertIntIntoHexChar value =
    if 0 <= value && value < 10 then
        String.fromInt value |> String.toList |> List.head |> Maybe.withDefault '0'

    else
        Char.fromCode (Char.toCode 'A' + value - 10)


convertIntIntoHexString : Int -> String
convertIntIntoHexString value =
    let
        upperValue =
            value // 16

        lowerValue =
            modBy 16 value

        upperStr =
            if upperValue == 0 then
                ""

            else
                convertIntIntoHexString upperValue

        lowerStr =
            convertIntIntoHexChar lowerValue
                |> String.fromChar
    in
    upperStr ++ lowerStr


convertCharIntoHexString : Int -> String
convertCharIntoHexString value =
    String.padLeft 2 '0' <| convertIntIntoHexString value


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
                        Html.div [ Html.Attributes.classList [ ( "my-hidden-popover", not isCurrentPopoverCommand ), ( "d-inline-block", True ) ] ]
                            (Popover.config
                                (Html.span
                                    (Html.Attributes.classList
                                        [ ( "text-muted", not isError )
                                        , ( "text-danger", isError )
                                        , ( "font-weight-bold", isError )
                                        , ( "d-none", not visible )
                                        ]
                                        :: Popover.onClick model.popoverState (ChangePopoverState pos)
                                    )
                                    [ text token.value ]
                                )
                                |> commandPopoverView token model.popoverState pos
                            )
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
                            |> commandPopoverView token model.popoverState pos
                        )
                        |> List.singleton

        BFLoopFunc commands ->
            let
                children =
                    viewOfBFCommands model pos commands
            in
            List.concat [ brWithSpacings <| depth + 1, children, brWithSpacings depth ]


commandPopoverView : BFToken -> Popover.State -> List Int -> Popover.Config msg -> List (Html msg)
commandPopoverView token state pos config =
    Popover.titleH4 [] [ text "Command: ", text <| tokenKindToString token.kind ] config
        |> Popover.content []
            (List.concat
                [ [ text "Value: "
                  , text token.value
                  , Html.br [] []
                  , text "Position: "
                  , text (List.reverse pos |> List.map String.fromInt |> String.join ", ")
                  ]
                , case token.error of
                    Nothing ->
                        []

                    Just error ->
                        [ Html.br [] []
                        , text "Parse Error: "
                        , text <| bfParseErrorToString error
                        ]
                ]
            )
        |> Popover.view state
        |> List.singleton
