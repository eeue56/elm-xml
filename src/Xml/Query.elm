module Xml.Query exposing (tags, contains)

{-|

# Search the parsed XML

@docs tags, contains
-}

import Xml.Encode exposing (Value(..))


{-| Search an XML value for any tags matching

    >>> import Xml.Encode exposing (object, null, encode, Value(Tag, Object))
    >>> import Dict
    >>> object [ ("name", Dict.empty, null)] |> tags "name"
    [Tag "name" Dict.empty (Object []) ]

-}
tags : String -> Value -> List Value
tags tagName node =
    case node of
        StrNode _ ->
            []

        BoolNode _ ->
            []

        IntNode _ ->
            []

        FloatNode _ ->
            []

        DocType _ _ ->
            []

        Tag name _ value ->
            (if name == tagName then
                [ node ]
             else
                []
            )
                ++ (tags tagName value)

        Object nodes ->
            List.map (tags tagName) nodes
                |> List.concat


{-| check if a XML value contains another XML value
-}
contains : Value -> Value -> Bool
contains contents node =
    if node == contents then
        True
    else
        case node of
            StrNode _ ->
                False

            BoolNode _ ->
                False

            IntNode _ ->
                False

            FloatNode _ ->
                False

            DocType _ _ ->
                False

            Tag name _ value ->
                contains contents value

            Object nodes ->
                List.any (contains contents) nodes
