module PlanParsers.Json exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)

type Plan 
    = PCte CteNode
    | PGeneric CommonFields
    | PResult ResultNode
    | PSort SortNode
    | PSeqScan SeqScanNode

type Plans 
    = Plans (List Plan)
    
type alias CommonFields =
    { actualLoops : Int
    , actualRows : Int
    , actualStartupTime : Float
    , actualTotalTime : Float
    , nodeType : String
    , output : List String
    , parallelAware : Bool
    , planRows : Int
    , plans : Plans
    , planWidth : Int
    , relationName : String
    , schema : String
    , startupCost : Float
    , totalCost : Float
    }
    
type alias ResultNode =
    { common : CommonFields
    , parentRelationship : String
    }    
    
type alias SeqScanNode =
    { common : CommonFields
    , alias_ : String
    , filter : String
    , relationName : String
    , rowsRemovedByFilter : Int
    } 
    
type alias CteNode =
    { common : CommonFields
    , alias_ : String
    , cteName : String
    }
    
type alias SortNode =
    { common : CommonFields
    , sortKey : List String
    , sortMethod : String
    , sortSpaceUsed : Int
    , sortSpaceType : String
    }
    
type alias PlanJson =
    { executionTime : Float
    , plan : Plan
    , planningTime : Float
    , triggers : List String
    }
    

decodePlanJson : Decode.Decoder PlanJson
decodePlanJson =
    decode PlanJson
        |> optional "Execution Time" Decode.float 0
        |> required "Plan" decodePlan
        |> optional "Planning Time" Decode.float 0
        |> optional "Triggers" (Decode.list Decode.string) []

decodeGenericNode : Decode.Decoder Plan
decodeGenericNode =
    Decode.map PGeneric decodeCommonFields

decodeNode : String -> Decode.Decoder Plan
decodeNode nodeType = 
    case nodeType of
        "CTE Scan" -> decodeCteNode
        "Result" -> decodeResultNode
        "Sort" -> decodeSortNode
        "Seq Scan" -> decodeSeqScanNode
        _ -> decodeGenericNode


decodePlan : Decode.Decoder Plan
decodePlan =
    Decode.lazy 
        (\_ -> 
            Decode.field "Node Type" Decode.string
                |> Decode.andThen decodeNode
        )

decodePlans : Decode.Decoder Plans
decodePlans =
    Decode.map Plans <| Decode.list (Decode.lazy (\_ -> decodePlan))

decodeCommonFields : Decode.Decoder CommonFields
decodeCommonFields =
    decode CommonFields
        |> required "Actual Loops" Decode.int
        |> required "Actual Rows" Decode.int
        |> required "Actual Startup Time" Decode.float
        |> required "Actual Total Time" Decode.float
        |> required "Node Type" Decode.string
        |> optional "Output" (Decode.list Decode.string) []
        |> required "Parallel Aware" Decode.bool
        |> required "Plan Rows" Decode.int
        |> optional "Plans" (Decode.lazy (\_ -> decodePlans)) (Plans [])
        |> required "Plan Width" Decode.int
        |> optional "Relation Name" Decode.string ""
        |> optional "Schema" Decode.string ""
        |> required "Startup Cost" Decode.float
        |> required "Total Cost" Decode.float
       
decodeResultNode : Decode.Decoder Plan       
decodeResultNode =
    let
        innerDecoder =
            decode ResultNode
                |> custom decodeCommonFields
                |> required "Parent Relationship" Decode.string
    in
        Decode.map PResult innerDecoder
        
decodeCteNode : Decode.Decoder Plan       
decodeCteNode =
    let
        innerDecoder =
            decode CteNode
                |> custom decodeCommonFields
                |> required "Alias" Decode.string
                |> required "CTE Name" Decode.string
    in
        Decode.map PCte innerDecoder        
        
decodeSortNode : Decode.Decoder Plan       
decodeSortNode =
    let
        innerDecoder =
            decode SortNode
                |> custom decodeCommonFields
                |> required "Sort Key" (Decode.list Decode.string)
                |> required "Sort Method" Decode.string
                |> required "Sort Space Used" Decode.int
                |> required "Sort Space Type" Decode.string
    in
        Decode.map PSort innerDecoder      
        
decodeSeqScanNode : Decode.Decoder Plan       
decodeSeqScanNode =
    let
        innerDecoder =
            decode SeqScanNode
                |> custom decodeCommonFields
                |> required "Alias" Decode.string
                |> required "Filter" Decode.string
                |> required "Relation Name" Decode.string
                |> required "Rows Removed by Filter" Decode.int
    in
        Decode.map PSeqScan innerDecoder             