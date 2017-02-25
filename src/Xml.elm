module Xml exposing (Value(..), map, foldl, xmlToJson)

{-|

The main data structure along with some trivial helpers.

@docs Value

@docs foldl, map, xmlToJson
-}

import Dict exposing (Dict)
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

    >>> import Dict
    >>> import Json.Encode as Json
    >>> xmlToJson (StrNode "hello")
    Json.string "hello"

    >>> xmlToJson (IntNode 5)
    Json.int 5

    >>> xmlToJson (FloatNode 5)
    Json.float 5

    >>> xmlToJson (BoolNode True)
    Json.bool True

    >>> xmlToJson (Object [ IntNode 5, BoolNode True ])
    Json.list [Json.int 5, Json.bool True]

    >>> xmlToJson (DocType "" Dict.empty)
    Json.null
-}
xmlToJson : Value -> Json.Value
xmlToJson xml =
    case xml of
        Tag name attributes nextValue ->
            let
                jsonAttrs =
                    Dict.toList attributes
                        |> List.map (\( name, value ) -> ( name, xmlToJson value ))
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
            List.map xmlToJson values
                |> Json.list

        DocType _ _ ->
            Json.null
