module Xml.Encode exposing (..)

import String
import Dict exposing (Dict)


type Value
    = Tag String (Dict String Value) Value
    | StrNode String
    | IntNode Int
    | FloatNode Float
    | BoolNode Bool
    | Object (List Value)


propToString : Value -> String
propToString value =
    case value of
        StrNode str ->
            "\"" ++ str ++ "\""

        IntNode n ->
            toString n

        BoolNode b ->
            toString b
                |> String.toLower

        FloatNode f ->
            toString f

        _ ->
            ""


valueToString : Int -> Int -> Value -> String
valueToString level indent value =
    case value of
        Tag name props nextValue ->
            let
                needsIndent =
                    case nextValue of
                        Object _ ->
                            True

                        Tag _ _ _ ->
                            True

                        _ ->
                            False

                indentString =
                    if needsIndent then
                        "\n"
                    else
                        ""

                propsToString =
                    Dict.toList props
                        |> List.map (\( key, value ) -> key ++ "=" ++ (propToString value))
                        |> String.join " "
                        |> (\x ->
                                if String.length x > 0 then
                                    " " ++ x
                                else
                                    ""
                           )
            in
                "<"
                    ++ name
                    ++ propsToString
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


encode : Int -> Value -> String
encode indent value =
    valueToString -1 indent value


string : String -> Value
string str =
    StrNode str


int : Int -> Value
int n =
    IntNode n


float : Float -> Value
float n =
    FloatNode n


bool : Bool -> Value
bool b =
    BoolNode b


object : List ( String, Dict String Value, Value ) -> Value
object values =
    List.map (\( name, props, value ) -> Tag name props value) values
        |> Object
