module Xml exposing
    ( Value(..)
    , foldl, map, xmlToJson, jsonToXml
    )

{-| The main data structure along with some trivial helpers.

@docs Value

@docs foldl, map, xmlToJson, jsonToXml

-}

import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as Json


{-| Representation of the XML tree
-}
type Value
    = Tag String (Dict String Value) Value
    | StrNode String
    | IntNode Int
    | FloatNode Float
    | BoolNode Bool
    | Object (List Value)
    | DocType String (Dict String Value)


{-|

    Standard mapping of a value to another value

-}
map : (Value -> Value) -> Value -> Value
map fn value =
    case value of
        Tag name dict nextValue ->
            map fn nextValue
                |> Tag name dict
                |> fn

        Object vals ->
            List.map (map fn) vals
                |> Object
                |> fn

        thing ->
            fn thing


{-| Standard foldl
-}
foldl : (Value -> a -> a) -> a -> Value -> a
foldl fn init value =
    case value of
        Tag name dict nextValue ->
            foldl fn (fn value init) nextValue

        Object values ->
            List.foldl (\elm b -> foldl fn b elm) (fn value init) values

        anything ->
            fn anything init


{-| Convert an `Xml.Value` to a `Json.Value`

    import Dict
    import Json.Encode as Json
    import Xml exposing (jsonToXml, Value(..))

    xmlToJson (StrNode "hello")
    --> Json.string "hello"

    xmlToJson (IntNode 5)
    --> Json.int 5

    xmlToJson (FloatNode 5)
    --> Json.float 5

    xmlToJson (BoolNode True)
    --> Json.bool True

    xmlToJson (Object [ IntNode 5, BoolNode True ])
    --> Json.list identity [Json.int 5, Json.bool True]

    xmlToJson (DocType "" Dict.empty)
    --> Json.null

-}
xmlToJson : Value -> Json.Value
xmlToJson xml =
    case xml of
        Tag name attributes nextValue ->
            let
                jsonAttrs =
                    Dict.toList attributes
                        |> List.map (\( nam, value ) -> ( nam, xmlToJson value ))
                        |> (\list -> list ++ [ ( "value", xmlToJson nextValue ) ])
                        |> Json.object
            in
            Json.object [ ( name, jsonAttrs ) ]

        StrNode str ->
            Json.string str

        IntNode int ->
            Json.int int

        FloatNode float ->
            Json.float float

        BoolNode bool ->
            Json.bool bool

        Object values ->
            Json.list xmlToJson values

        DocType _ _ ->
            Json.null


{-| A decoder for XML
-}
xmlDecoder : JD.Decoder Value
xmlDecoder =
    JD.oneOf
        [ JD.map StrNode JD.string
        , JD.map IntNode JD.int
        , JD.map FloatNode JD.float
        , JD.map BoolNode JD.bool
        , JD.map Object (JD.list (JD.lazy (\_ -> xmlDecoder)))
        , JD.map
            (Dict.toList >> List.map (\( name, val ) -> Tag name Dict.empty val) >> Object)
            (JD.dict (JD.lazy (\_ -> xmlDecoder)))
        ]


{-| Convert a `Json.Value` into an `Xml.Value`

    import Dict
    import Json.Encode as Json
    import Xml exposing (jsonToXml, Value(..))

    jsonToXml (Json.string "hello")
    --> StrNode "hello"

    jsonToXml (Json.int 5)
    --> IntNode 5

    jsonToXml (Json.float 10.5)
    --> FloatNode 10.5

    jsonToXml (Json.bool True)
    --> BoolNode True

    jsonToXml (Json.object [("name", Json.string "hello")])
    --> Object [ Tag "name" Dict.empty (StrNode "hello") ]

    jsonToXml (Json.list identity [Json.string "name", Json.string "hello"])
    --> Object [ StrNode "name", StrNode "hello" ]

-}
jsonToXml : Json.Value -> Value
jsonToXml json =
    JD.decodeValue xmlDecoder json
        |> Result.withDefault (Object [])
