module Main exposing (main)

import Array exposing (Array)
import BFExecutor exposing (getMaybeTapeValue, runBFCommands)
import BFParser exposing (convertBFCommandsToString, parseTokens)
import BFTypes exposing (BFCommand(..), BFExecutorParams, BFParseError(..), BFTape(..), BFToken, BFTokenKind(..), BFTokenTable, RunningState(..), bfParseErrorToString, initialExecutorParams, tapePages, tapeSize, tokenKindFromInt, tokenKindToInt, tokenKindToString)
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
import Bootstrap.Modal as Modal
import Bootstrap.Popover as Popover
import Bootstrap.Tab as Tab
import Bootstrap.Table as Table
import Bootstrap.Utilities.Spacing as Spacing
import Browser
import Cacher exposing (cache)
import Html exposing (Html, text)
import Html.Attributes
import Html.Events
import Json.Decode as JD
import Json.Encode as JE
import Language.BF
import Language.HogyLang
import Language.Ook



-- Config


bfDefaultTokenTableList : List BFTokenTable
bfDefaultTokenTableList =
    [ Language.BF.table
    , Language.Ook.table
    ]


bfTokenTableList : Model -> List BFTokenTable
bfTokenTableList model =
    List.concat [ bfDefaultTokenTableList, model.customTokenTableList ]


defaultTokenTable : BFTokenTable
defaultTokenTable =
    Language.BF.table



-- Main


main : Program JD.Value Model Msg
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
        , Dropdown.subscriptions model.upComingCustomTokenTableDropdown <| UpdateUpComingCustomTokenTableDropdown
        ]



-- SubModels / Update


type alias CommandPopoverState =
    { popoverState : Popover.State
    , popoverIndices : List Int
    }


type CommandPopoverStateMsg
    = UpdateCommandPopoversState Popover.State
    | UpdateCommandPopoverIndices (List Int)


updateCommandPopoverState : CommandPopoverStateMsg -> CommandPopoverState -> CommandPopoverState
updateCommandPopoverState msg popoverState =
    case msg of
        UpdateCommandPopoversState state ->
            { popoverState | popoverState = state }

        UpdateCommandPopoverIndices pos ->
            { popoverState | popoverIndices = pos }


initialCommandPopoverState : CommandPopoverState
initialCommandPopoverState =
    { popoverState = Popover.initialState
    , popoverIndices = []
    }


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
    , tokenTable = defaultTokenTable
    }


type BFExecutorParamsMsg
    = UpdateTokens (Array BFCommand)
    | UpdateInput String
    | UpdateCurrentTapePage Int
    | ExecuteWithNewRunningState RunningState
    | StopExecution


updateExecutorParams : BFExecutorParamsMsg -> BFExecutorParams -> BFExecutorParams
updateExecutorParams msg state =
    case msg of
        UpdateTokens commands ->
            { state | commands = commands }

        UpdateInput input ->
            { state | input = input }

        UpdateCurrentTapePage page ->
            let
                newPage =
                    modBy tapePages page
            in
            { state | currentTapePage = newPage }

        ExecuteWithNewRunningState runningState ->
            runBFCommands { state | runningState = runningState }

        StopExecution ->
            { initialExecutorParams | commands = state.commands, input = state.input }


createTokenTable : Model -> List BFTokenTable
createTokenTable model =
    let
        tokenTable =
            model.upComingCustomTokenTable
                |> Array.toList
    in
    ( tokenTable, model.upComingCustomTokenTableName )
        |> List.singleton



-- Model


type alias Model =
    { programContent : String
    , tabState : Tab.State
    , displayNoOpCommand : Bool
    , customTokenTableList : List BFTokenTable
    , addCustomTokenTableModalState : Modal.Visibility
    , upComingCustomTokenTable : Array ( BFTokenKind, String )
    , upComingCustomTokenTableName : String
    , upComingCustomTokenTableDropdown : Dropdown.State
    , commandPopoverState : CommandPopoverState
    , showBFTapeAs : ShowBFTapeAs
    , resetConfirmationModalState : Modal.Visibility
    , parserTokenTableState : TokenTableState
    , displayTokenTableState : TokenTableState
    , executorParams : BFExecutorParams
    }


type ShowBFTapeAs
    = ShowBFTapeAsInt
    | ShowBFTapeAsHex
    | ShowBFTapeAsStr


init : JD.Value -> ( Model, Cmd Msg )
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
    , customTokenTableList = [ Language.HogyLang.table ]
    , addCustomTokenTableModalState = Modal.hidden
    , upComingCustomTokenTable = Array.fromList [ ( LoopStart, "[" ), ( LoopEnd, "]" ), ( IncreaseValue, "+" ), ( DecreaseValue, "-" ), ( IncreasePointer, ">" ), ( DecreasePointer, "<" ), ( ReadInput, "," ), ( PrintOutput, "." ) ]
    , upComingCustomTokenTableName = "New Language"
    , upComingCustomTokenTableDropdown = Dropdown.initialState
    , commandPopoverState = initialCommandPopoverState
    , showBFTapeAs = ShowBFTapeAsInt
    , resetConfirmationModalState = Modal.hidden
    , parserTokenTableState = initialTokenTableState
    , displayTokenTableState = initialTokenTableState
    , executorParams = initialExecutorParams
    }


encodeModel : Model -> String
encodeModel model =
    JE.encode 0 <|
        JE.object
            [ ( "programContent", JE.string model.programContent )
            , ( "customLanguages"
              , JE.list encodeTokenTable model.customTokenTableList
              )
            , ( "parserTokenTable", encodeTokenTable model.parserTokenTableState.tokenTable )
            , ( "displayTokenTable", encodeTokenTable model.displayTokenTableState.tokenTable )
            ]


decodeModel : JD.Value -> Model
decodeModel value =
    let
        cacheStr =
            JD.decodeValue JD.string value
                |> Result.withDefault ""

        programContent =
            JD.decodeString (JD.field "programContent" JD.string) cacheStr
                |> Result.withDefault ""

        customTokenTableList =
            JD.decodeString
                (JD.field "customLanguages" <|
                    JD.list decodeTokenTable
                )
                cacheStr
                |> Result.withDefault initialModel.customTokenTableList

        parserTokenTable =
            JD.decodeString
                (JD.field "parserTokenTable" decodeTokenTable)
                cacheStr
                |> Result.withDefault defaultTokenTable

        displayTokenTable =
            JD.decodeString
                (JD.field "displayTokenTable" decodeTokenTable)
                cacheStr
                |> Result.withDefault defaultTokenTable
    in
    { initialModel
        | programContent = programContent
        , customTokenTableList = customTokenTableList
        , parserTokenTableState = { initialTokenTableState | tokenTable = parserTokenTable }
        , displayTokenTableState = { initialTokenTableState | tokenTable = displayTokenTable }
    }


encodeTokenTable : BFTokenTable -> JE.Value
encodeTokenTable ( tableList, name ) =
    JE.object
        [ ( "tokens"
          , JE.list
                (\( kind, value ) ->
                    JE.object
                        [ ( "kind", JE.int <| tokenKindToInt kind )
                        , ( "value", JE.string value )
                        ]
                )
                tableList
          )
        , ( "name", JE.string name )
        ]


decodeTokenTable : JD.Decoder BFTokenTable
decodeTokenTable =
    JD.map2 Tuple.pair
        (JD.field "tokens" <|
            JD.list <|
                JD.map2 Tuple.pair
                    (JD.field "kind" <|
                        JD.map
                            (Maybe.withDefault NoOp << tokenKindFromInt)
                            -- shouldn't become NoOp, it should be Error
                            JD.int
                    )
                    (JD.field "value" JD.string)
        )
        (JD.field "name" JD.string)


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
    | AddCustomTokenTable
    | UpdateAddCustomTokenTableModalState Modal.Visibility
    | UpdateUpComingCustomTokenTable Int String
    | PushUpComingCustomTokenTable BFTokenKind
    | RemoveUpComingCustomTokenTable Int
    | UpdateUpComingCustomTokenTableName String
    | UpdateUpComingCustomTokenTableDropdown Dropdown.State
    | UpdateHowShowBFTapeAs ShowBFTapeAs
    | CopyConvertedProgram
    | ResetAll
    | UpdateResetConfirmationModalState Modal.Visibility
    | UpdateCommandPopoverState CommandPopoverStateMsg
    | ChangeCommandPopoverState (List Int) Popover.State
    | UpdateParserTokenTableState TokenTableStateMsg
    | UpdateDisplayTokenTableState TokenTableStateMsg
    | ParseTokens
    | UpdateExecutorParams BFExecutorParamsMsg


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

        AddCustomTokenTable ->
            let
                customTokenTableList =
                    List.append model.customTokenTableList <| createTokenTable model
            in
            { model | customTokenTableList = customTokenTableList }
                |> update (UpdateAddCustomTokenTableModalState Modal.hidden)
                |> Tuple.first
                |> withCacheCmd

        UpdateAddCustomTokenTableModalState state ->
            { model | addCustomTokenTableModalState = state }
                |> withCmdNone

        UpdateUpComingCustomTokenTable pos value ->
            let
                kind =
                    Array.get pos model.upComingCustomTokenTable
                        |> Maybe.map Tuple.first
                        |> Maybe.withDefault NoOp

                upComingCustomTokenTable =
                    Array.set pos ( kind, value ) model.upComingCustomTokenTable
            in
            { model | upComingCustomTokenTable = upComingCustomTokenTable }
                |> withCmdNone

        PushUpComingCustomTokenTable kind ->
            let
                upComingCustomTokenTable =
                    Array.push ( kind, "" ) model.upComingCustomTokenTable
            in
            { model | upComingCustomTokenTable = upComingCustomTokenTable }
                |> withCmdNone

        RemoveUpComingCustomTokenTable pos ->
            let
                upComingCustomTokenTable =
                    Array.set pos ( NoOp, "" ) model.upComingCustomTokenTable
                        |> Array.filter (\( kind, _ ) -> kind /= NoOp)
            in
            { model | upComingCustomTokenTable = upComingCustomTokenTable }
                |> withCmdNone

        UpdateUpComingCustomTokenTableName name ->
            { model | upComingCustomTokenTableName = name }
                |> withCmdNone

        UpdateUpComingCustomTokenTableDropdown state ->
            { model | upComingCustomTokenTableDropdown = state }
                |> withCmdNone

        UpdateHowShowBFTapeAs state ->
            { model | showBFTapeAs = state }
                |> withCmdNone

        CopyConvertedProgram ->
            update (ChangeProgramContent <| convertBFCommandsToString model.displayTokenTableState.tokenTable model.executorParams.commands) model

        ResetAll ->
            initialModel
                |> withCacheCmd

        UpdateResetConfirmationModalState state ->
            { model | resetConfirmationModalState = state }
                |> withCmdNone

        UpdateCommandPopoverState popoverStateMsg ->
            let
                state =
                    updateCommandPopoverState popoverStateMsg model.commandPopoverState
            in
            { model | commandPopoverState = state }
                |> withCmdNone

        ChangeCommandPopoverState pos state ->
            update (UpdateCommandPopoverState <| UpdateCommandPopoverIndices pos) model
                |> Tuple.first
                |> update (UpdateCommandPopoverState <| UpdateCommandPopoversState state)
                |> Tuple.first
                |> withCmdNone

        UpdateParserTokenTableState tokenTableStateMsg ->
            let
                state =
                    updateTokenTableState tokenTableStateMsg model.parserTokenTableState
            in
            { model | parserTokenTableState = state }
                |> update ParseTokens
                |> Tuple.first
                |> withCacheCmd

        UpdateDisplayTokenTableState tokenTableStateMsg ->
            let
                state =
                    updateTokenTableState tokenTableStateMsg model.displayTokenTableState
            in
            { model | displayTokenTableState = state }
                |> withCacheCmd

        ParseTokens ->
            let
                commands =
                    parseTokens model.parserTokenTableState.tokenTable model.programContent
                        |> Result.withDefault model.executorParams.commands
            in
            update (UpdateExecutorParams <| UpdateTokens commands) model
                |> Tuple.first
                |> withCmdNone

        UpdateExecutorParams executorParamsMsg ->
            let
                state =
                    updateExecutorParams executorParamsMsg model.executorParams
            in
            { model | executorParams = state }
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
                                        List.map (viewOfBFTokenTableItem <| UpdateParserTokenTableState << UpdateTokenTable) <| bfTokenTableList model
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
                                                , Textarea.onInput (UpdateExecutorParams << UpdateInput)
                                                , Textarea.value model.executorParams.input
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
                                            (model.executorParams.output
                                                |> List.reverse
                                                |> List.map String.fromChar
                                                |> List.map
                                                    (\str ->
                                                        if str == "\n" then
                                                            Html.br [] []

                                                        else
                                                            text str
                                                    )
                                                |> (\x -> List.append x [ Html.span [ Html.Attributes.class "text-danger" ] [ text <| Maybe.withDefault "" model.executorParams.error ] ])
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
                        [ Button.button [ Button.onClick (UpdateExecutorParams <| ExecuteWithNewRunningState Running) ] [ text "Run" ]
                        , Button.button [ Button.onClick (UpdateExecutorParams <| ExecuteWithNewRunningState RunningStep) ] [ text "StepRun" ]
                        , Button.button [ Button.onClick (UpdateExecutorParams <| ExecuteWithNewRunningState RunningUntilEndingLoop) ] [ text "SkipLoopOnce" ]
                        , Button.button [ Button.onClick (UpdateExecutorParams <| ExecuteWithNewRunningState RunningUntilLeavingLoop) ] [ text "SkipEntireLoop" ]
                        , Button.button [ Button.onClick (UpdateExecutorParams StopExecution) ] [ text "ResetStepRunPosition" ]
                        , Button.button [ Button.onClick <| UpdateResetConfirmationModalState Modal.shown ] [ text "ResetAll" ]
                        , Button.button [ Button.onClick CopyConvertedProgram ] [ text "CopyParsedProgramIntoProgramInput" ]
                        , Button.button
                            [ Button.attrs [ Html.Events.onClick <| UpdateAddCustomTokenTableModalState Modal.shown ] ]
                            [ text "Add new Language" ]
                        , Modal.config (UpdateResetConfirmationModalState Modal.hidden)
                            |> Modal.hideOnBackdropClick True
                            |> Modal.h3 [] [ text "Are you sure?" ]
                            |> Modal.body []
                                [ text "Resetting everything can't be undone." ]
                            |> Modal.footer []
                                [ Button.button [ Button.onClick <| UpdateResetConfirmationModalState Modal.hidden ]
                                    [ text "Cancel" ]
                                , Button.button [ Button.onClick ResetAll, Button.danger ]
                                    [ text "Reset EVERYTHING" ]
                                ]
                            |> Modal.view model.resetConfirmationModalState
                        , Modal.config (UpdateAddCustomTokenTableModalState Modal.hidden)
                            |> Modal.hideOnBackdropClick True
                            |> Modal.h3 [] [ text "New Language" ]
                            |> Modal.body []
                                ((Array.toList <|
                                    Array.indexedMap
                                        (\idx ( kind, value ) ->
                                            Grid.row []
                                                [ Grid.col []
                                                    [ InputGroup.config
                                                        (InputGroup.text
                                                            [ Input.attrs
                                                                [ Html.Events.onInput <| UpdateUpComingCustomTokenTable idx
                                                                , Html.Attributes.value value
                                                                ]
                                                            ]
                                                        )
                                                        |> InputGroup.predecessors [ InputGroup.span [] [ text <| tokenKindToString kind ] ]
                                                        |> InputGroup.successors
                                                            [ InputGroup.button
                                                                [ Button.danger
                                                                , Button.attrs [ Html.Events.onClick <| RemoveUpComingCustomTokenTable idx ]
                                                                ]
                                                                [ text "Delete" ]
                                                            ]
                                                        |> InputGroup.view
                                                    ]
                                                ]
                                        )
                                        model.upComingCustomTokenTable
                                 )
                                    ++ [ Grid.row []
                                            [ Grid.col []
                                                [ Dropdown.dropdown
                                                    model.upComingCustomTokenTableDropdown
                                                    { options = []
                                                    , toggleMsg = UpdateUpComingCustomTokenTableDropdown
                                                    , toggleButton =
                                                        Dropdown.toggle [ Button.outlineSecondary ] [ text "Add new" ]
                                                    , items =
                                                        [ LoopStart, LoopEnd, IncreaseValue, DecreaseValue, IncreasePointer, DecreasePointer, ReadInput, PrintOutput ]
                                                            |> List.map
                                                                (\k ->
                                                                    Dropdown.buttonItem [ Html.Events.onClick <| PushUpComingCustomTokenTable k ] [ text <| tokenKindToString k ]
                                                                )
                                                    }
                                                ]
                                            ]
                                       ]
                                )
                            |> Modal.footer []
                                [ InputGroup.config
                                    (InputGroup.text
                                        [ Input.attrs
                                            [ Html.Events.onInput UpdateUpComingCustomTokenTableName
                                            , Html.Attributes.value model.upComingCustomTokenTableName
                                            ]
                                        ]
                                    )
                                    |> InputGroup.predecessors
                                        [ InputGroup.span [] [ text "Language Name" ] ]
                                    |> InputGroup.successors
                                        [ InputGroup.button
                                            [ Button.outlinePrimary
                                            , Button.attrs [ Html.Events.onClick AddCustomTokenTable ]
                                            ]
                                            [ text "Add" ]
                                        ]
                                    |> InputGroup.view
                                ]
                            |> Modal.view model.addCustomTokenTableModalState
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
                                                List.map (viewOfBFTokenTableItem <| UpdateDisplayTokenTableState << UpdateTokenTable) <| bfTokenTableList model
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
                                [ viewOfBFCommands model [] model.executorParams.commands
                                    |> Html.p []
                                    |> Block.custom
                                ]
                            |> Card.view
                        ]
                    , Grid.col [ Col.lg6 ]
                        [ Card.config []
                            |> Card.header []
                                [ Grid.row []
                                    [ Grid.col [ Col.sm2, Col.lg3 ] [ text "Tape Status" ]
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
                                    , Grid.col [ Col.sm4, Col.lg3 ]
                                        [ InputGroup.config
                                            (InputGroup.text [ Input.value <| String.fromInt model.executorParams.currentTapePage, Input.onInput (String.toInt >> Maybe.withDefault 0 >> UpdateCurrentTapePage >> UpdateExecutorParams) ])
                                            |> InputGroup.small
                                            |> InputGroup.predecessors
                                                [ InputGroup.button [ Button.secondary, Button.onClick <| UpdateExecutorParams <| UpdateCurrentTapePage <| model.executorParams.currentTapePage - 1 ] [ text "<" ] ]
                                            |> InputGroup.successors
                                                [ InputGroup.span [] [ text "/ ", text <| String.fromInt <| tapePages - 1 ]
                                                , InputGroup.button [ Button.secondary, Button.onClick <| UpdateExecutorParams <| UpdateCurrentTapePage <| model.executorParams.currentTapePage + 1 ] [ text ">" ]
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
                                                    model.executorParams.currentTapePage * 16 + idx
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
                [ Button.button [ Button.onClick <| ChangeProgramContent "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>." ] [ text "Set BF Hello world program" ] ]
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
                            model.executorParams.tapePointer == address

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
                            getMaybeTapeValue model.executorParams.tape address
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
            model.executorParams.currentIndices == pos

        isCurrentPopoverCommand =
            model.commandPopoverState.popoverIndices == pos

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
                                        :: Popover.onClick model.commandPopoverState.popoverState (ChangeCommandPopoverState pos)
                                    )
                                    [ text token.value ]
                                )
                                |> commandPopoverView token model.commandPopoverState.popoverState pos
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
                                    :: Popover.onClick model.commandPopoverState.popoverState (ChangeCommandPopoverState pos)
                                )
                                [ text displayValue ]
                            )
                            |> commandPopoverView token model.commandPopoverState.popoverState pos
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
