module Xml.Encode exposing (Value(..), encode, string, int, float, bool, object, null)

import String
import Dict exposing (Dict)


type Value
    = Tag String (Dict String Value) Value
    | StrNode String
    | IntNode Int
    | FloatNode Float
    | BoolNode Bool
    | Object (List Value)
    | DocType String (Dict String Value)


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
                indentString =
                    if needsIndent nextValue then
                        "\n"
                    else
                        ""
            in
                "<"
                    ++ name
                    ++ (propsToString props)
                    ++ ">"
                    ++ indentString
                    ++ (valueToString (level + 1) indent nextValue)
                    ++ indentString
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
            toString b

        Object xs ->
            List.map (valueToString (level + 1) indent) xs
                |> List.map ((++) (String.repeat (level * indent) " "))
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


{-| Encode an XML string
-}
string : String -> Value
string str =
    StrNode str


{-| Encode an int
-}
int : Int -> Value
int n =
    IntNode n


{-| Encode an int
-}
float : Float -> Value
float n =
    FloatNode n


{-| Encode an int
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


{-| Empty contents
-}
null : Value
null =
    object []
