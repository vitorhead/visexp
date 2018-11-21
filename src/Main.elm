module Main exposing (..)

import Color
import Style.Border as Border
import Style.Color as Color
import Html 
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Input as Input
import Element.Events exposing (..)
import Style exposing (..)
import Style.Font as Font
import Json.Decode 

import PlanParsers.Json exposing (..)

type Page
    = InputPage
    | DisplayPage


type Msg
    = Dummy
    | ChangePlanText String
    | SubmitPlan
    | MouseEnteredPlanNode Plan
    | MouseLeftPlanNode Plan
    
type alias Model =
    { currentPage : Page
    , currPlanText : String
    , selectedNode : Maybe Plan
    }


init : ( Model, Cmd Msg )
init =
    ({ currentPage = InputPage
     , currPlanText = ""
     , selectedNode = Nothing
     }, Cmd.none )

type Styles
    = None
    | NavBar
    | Button
    | InputField
    | InputLabel
    | PlanNodeBox
    | SectionHeader
    | DetailPanel

stylesheet : StyleSheet Styles variation
stylesheet = 
    styleSheet
        [ style None []
        , style NavBar
            [ Border.bottom 1
            , Color.border Color.blue
            ]
        , style Button 
            [ Border.rounded 3
            , Border.left 5
            , Border.right 5
            , Color.background Color.darkBlue
            , Color.border Color.blue
            , Color.text Color.white
            , Font.bold
            ]
        , style InputField 
            [ Border.all 1
            , Border.rounded 3
            , Color.border Color.lightCharcoal
            ]
        , style InputLabel 
            [ Color.text Color.darkCharcoal
            ]
        , style PlanNodeBox
            [ Border.bottom 1
            , Color.border Color.lightBlue
            , hover
                [ Color.background Color.lightYellow
                ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of 
        ChangePlanText s -> ( { model | currPlanText = s }, Cmd.none)
        SubmitPlan -> ( { model | currentPage = DisplayPage }, Cmd.none)
        MouseEnteredPlanNode p -> ( { model | selectedNode = Just p }, Cmd.none )
        MouseLeftPlanNode p -> ( { model | selectedNode = Nothing }, Cmd.none )
        _ -> ( model, Cmd.none )



navBar = 
    row NavBar
        [ spread, paddingXY 60 10, width (percent 100)]
        [ el None [] <| text "V I S E X P"
        , el None [] <| text "menu"
        ]

childNodesToHtml : Plans -> Element Styles variation Msg
childNodesToHtml (Plans plans) =
    column None
        [ paddingLeft 20 ]
        (List.concatMap planNodeToHTML plans)

generalPlanNodeToHtml : Plan -> CommonFields -> List (Element Styles variation Msg) -> List (Element Styles variation Msg)
generalPlanNodeToHtml plan common relationNameEls =
    let 
        content =
            paragraph None
                []
                ( [bold common.nodeType] ++ relationNameEls )
    in
        [ el PlanNodeBox 
            [ padding 4
            , onMouseEnter <| MouseEnteredPlanNode plan
            , onMouseLeave <| MouseLeftPlanNode plan
            ]
            content
        ]
             ++ [ childNodesToHtml common.plans ]

planNodeToHTML : Plan -> List (Element Styles variation Msg)
planNodeToHTML plan =
    case plan of
        PCte cteNode ->
            generalPlanNodeToHtml plan
                cteNode.common
                [ text " on ", italic cteNode.cteName, text <| " ("++cteNode.alias_++")" ]
        
        PResult resultNode ->
            generalPlanNodeToHtml plan
                resultNode.common
                []
        
        PSeqScan seqScanNode ->
            generalPlanNodeToHtml plan
                seqScanNode.common
                [ text " on ", italic seqScanNode.relationName, text <| " ("++seqScanNode.alias_++")" ]
        
        PSort sortNode ->
            generalPlanNodeToHtml plan
                sortNode.common
                [ text " on ", italic (toString sortNode.sortKey) ]
            
        PGeneric genericNode ->
            generalPlanNodeToHtml plan
                genericNode
                []

inputPage : Element Styles variation Msg
inputPage = 
    column None
        [ width (px 800), height (fillPortion 1), paddingTop 10, paddingBottom 50, spacingXY 0 10 ]
        [ Input.multiline InputField
            [ height (px 300) ]
            { onChange = ChangePlanText
            , value = ""
            , label = 
                Input.labelAbove <| 
                    el InputLabel [] <|
                        text "Paste the EXPLAN (.json):"
            , options = []
            }
        , button Button [ alignRight, width (px 200), height (px 40), padding 10, onClick SubmitPlan ] <| text "Start!"  
        ]


detailEls : Plan -> Element Styles variation Msg
detailEls plan = 
    let
        attr name value = 
            row None
                [ width (percent 100) ]
                [ el None [ width (percent 50) ] <| text name
                , el None [ width (percent 50) ] <| bold <| toString value
                ]
        header name =
            el None [ paddingTop 10, paddingBottom 5 ] <| el SectionHeader [] <| bold name
        
        commonAttrs common =
            [ attr "Startup cost" common.startupCost
            , attr "Total cost" common.totalCost
            , attr "Schema" common.schema
            , attr "Parallel aware" common.parallelAware
            , header "Actual"
            , attr "Loops" common.actualLoops
            , attr "Rows" common.actualRows
            , attr "Startup time, ms" common.actualStartupTime
            , attr "Total time, ms" common.actualTotalTime
            , header "Plan"
            , attr "Rows" common.planRows
            , attr "Width" common.planWidth
            ]
        
        attributes = 
            case plan of
                PCte node ->
                    commonAttrs node.common
                
                PGeneric node ->
                    commonAttrs node
                
                PResult node ->
                    commonAttrs node.common
                
                PSeqScan node ->
                    commonAttrs node.common
                        ++ [ header "Filter"
                           , attr "Filter" node.filter
                           , attr "Width" node.rowsRemovedByFilter
                           ]
                          
                PSort node ->
                    commonAttrs node.common
                        ++ [ header "Sort"
                           , attr "Sort key" node.sortKey
                           , attr "Sort method" node.sortMethod
                           , attr "Sort space type" node.sortSpaceType
                           , attr "Sort space used" node.sortSpaceUsed
                           ]
    in
        column None [] attributes

displayPage : Model -> Element Styles variation Msg
displayPage model =
    let 
        tree = 
            case Json.Decode.decodeString decodePlanJson model.currPlanText of
                Ok planJson ->
                    column None [] <| planNodeToHTML planJson.plan
                
                Err err ->
                    el None [] <| text err
        
        detailContent = 
            case model.selectedNode of
                Nothing ->
                    text ""
                
                Just plan ->
                    detailEls plan
    in
        row None
            [ width (percent 100), paddingTop 20 ]
            [ el None [width (fillPortion 1) ] tree
            , el DetailPanel [ width (percent 30), padding 5 ] <| detailContent
            ]

view : Model -> Html.Html Msg
view model = 
    let
        content = 
            case model.currentPage of
                DisplayPage -> 
                    displayPage model
                
                InputPage -> 
                    inputPage
    in
        viewport stylesheet <|
            column None 
                [ center, width (percent 100)]
                [ navBar
                , content
                ]


main = Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
