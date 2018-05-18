module Xml.Encode exposing (encode, string, int, float, bool, object, null, list)

{-|
    Use this module for turning your Elm data into an `Xml` representation that can be either
    queried or decoded, or turned into a string.

@docs encode

@docs string, int, float, bool, object, null, list
-}

import String
import Dict exposing (Dict)
import Xml exposing (Value(..))


propToString : Value -> String
propToString value =
    case value of
        StrNode str ->
            str

        IntNode n ->
            toString n

        BoolNode b ->
            toString b
                |> String.toLower

        FloatNode f ->
            toString f

        _ ->
            ""


propsToString : Dict String Value -> String
propsToString props =
    Dict.toList props
        |> List.map (\( key, value ) -> key ++ "=\"" ++ (propToString value) ++ "\"")
        |> String.join " "
        |> (\x ->
                if String.length x > 0 then
                    " " ++ x
                else
                    ""
           )


needsIndent : Value -> Bool
needsIndent nextValue =
    case nextValue of
        Object [] ->
            False

        Object _ ->
            True

        Tag _ _ _ ->
            True

        _ ->
            False


valueToString : Int -> Int -> Value -> String
valueToString level indent value =
    case value of
        Tag name props nextValue ->
            let
                indentation =
                    String.repeat (indent * level // 2) " "

                newlineNecessary =
                    needsIndent nextValue

                maybeNewline =
                    if newlineNecessary then
                        "\n"
                    else
                        ""
            in
                indentation
                    ++ "<"
                    ++ name
                    ++ (propsToString props)
                    ++ ">"
                    ++ maybeNewline
                    ++ (valueToString (level + 1) indent nextValue)
                    ++ maybeNewline
                    ++ (if newlineNecessary then
                            indentation
                        else
                            ""
                       )
                    ++ "</"
                    ++ name
                    ++ ">"

        StrNode str ->
            str

        IntNode n ->
            toString n

        FloatNode n ->
            toString n

        BoolNode b ->
            toString b |> String.toLower

        Object xs ->
            List.map (valueToString (level + 1) indent) xs
                |> String.join "\n"

        DocType name props ->
            "<?"
                ++ name
                ++ (propsToString props)
                ++ "?>"


{-| Take a value, then generate a string from it
-}
encode : Int -> Value -> String
encode indent value =
    valueToString -1 indent value


{-| Encode a string

    string "hello" |> encode 0
    --> "hello"
-}
string : String -> Value
string str =
    StrNode str


{-| Encode an int

    int 15 |> encode 0
    --> "15"
-}
int : Int -> Value
int n =
    IntNode n


{-| Encode a float

    float 1.576 |> encode 0
    --> "1.576"
-}
float : Float -> Value
float n =
    FloatNode n


{-| Encode a bool

    bool True |> encode 0
    --> "true"

    bool True |> encode 0
    --> "true"
-}
bool : Bool -> Value
bool b =
    BoolNode b


{-| Encode an "object" (a tag)
-}
object : List ( String, Dict String Value, Value ) -> Value
object values =
    List.map (\( name, props, value ) -> Tag name props value) values
        |> Object


{-| Encode a list of nodes, e.g
    import Dict

    list [ object [ ("Root", Dict.empty, null) ], int 5 ] |> encode 0
    --> "<Root></Root>\n5"
-}
list : List Value -> Value
list values =
    Object values


{-| Empty contents

    null |> encode 0
    --> ""
-}
null : Value
null =
    object []
