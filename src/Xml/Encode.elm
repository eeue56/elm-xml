module Xml.Encode exposing (..)

import String


type Value
    = Tag String Value
    | StrNode String
    | IntNode Int
    | FloatNode Float
    | BoolNode Bool
    | Object (List Value)


valueToString : Int -> Int -> Value -> String
valueToString level indent value =
    case value of
        Tag name nextValue ->
            let
                needsIndent =
                    case nextValue of
                        Object _ ->
                            True

                        Tag _ _ ->
                            True

                        _ ->
                            False

                indentString =
                    if needsIndent then
                        "\n"
                    else
                        ""
            in
                "<"
                    ++ name
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


object : List ( String, Value ) -> Value
object values =
    List.map (\( name, value ) -> Tag name value) values
        |> Object
