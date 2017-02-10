module Xml exposing (Value(..), map, foldl)

{-|

The main data structure along with some trivial helpers.

@docs Value

@docs foldl, map
-}

import Dict exposing (Dict)


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
