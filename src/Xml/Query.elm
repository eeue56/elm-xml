module Xml.Query exposing
    ( tags, contains
    , tag, collect, default
    , string, int, float, bool
    )

{-|


# Search the parsed XML

@docs tags, contains

@docs tag, collect, default

@docs string, int, float, bool

-}

import Xml exposing (Value(..))


{-| Try to get a given tag name out from an XML value, then grab the value from that
Grabs the first tag that matches in the object

    import Dict
    import Xml exposing (Value(..))

    tag "name" string (Tag "name" Dict.empty (StrNode "noah"))
    --> Ok "noah"

    tag "name" string (Tag "name" Dict.empty (IntNode 5))
    --> Err "Not a string."

    tag "name" string (StrNode "noah")
    --> Err "No tag called 'name' found."

-}
tag : String -> (Value -> Result String a) -> Value -> Result String a
tag name converter value =
    case tags name value of
        [] ->
            "No tag called '"
                ++ name
                ++ "' found."
                |> Err

        firstTag :: _ ->
            case firstTag of
                Tag _ _ nextValue ->
                    converter nextValue

                Object (x :: _) ->
                    tag name converter x

                _ ->
                    Err "Not a tag"


{-| Collect as many values that pass the given converter

    import Dict
    import Xml exposing (Value(..))

    collect (tag "name" string) [Tag "name" Dict.empty (StrNode "noah")]
    --> [ "noah" ]

    collect (tag "name" string) [Tag "name" Dict.empty (IntNode 5)]
    --> [ ]

-}
collect : (Value -> Result String a) -> List Value -> List a
collect fn values =
    List.filterMap (fn >> Result.toMaybe) values


{-| Try to turn a value into a string

    import Xml exposing (Value(..))

    string (IntNode 5)
    --> Err "Not a string."

    string (StrNode "hello")
    --> Ok "hello"

-}
string : Value -> Result String String
string value =
    case value of
        StrNode s ->
            Ok s

        _ ->
            Err "Not a string."


{-| Try to turn a value into an int

    import Xml exposing (Value(..))

    int (IntNode 5)
    --> Ok 5

    int (StrNode "hello")
    --> Err "Not an int"

-}
int : Value -> Result String Int
int value =
    case value of
        IntNode n ->
            Ok n

        _ ->
            Err "Not an int"


{-| Try to turn a value into an int

    import Xml exposing (Value(..))

    float (FloatNode 5.5)
    --> Ok 5.5

    float (StrNode "hello")
    --> Err "Not a float"

-}
float : Value -> Result String Float
float value =
    case value of
        FloatNode n ->
            Ok n

        _ ->
            Err "Not a float"


{-| Try to turn a value into an int

    import Xml exposing (Value(..))

    bool (BoolNode True)
    --> Ok True

    bool (StrNode "hello")
    --> Err "Not a bool"

-}
bool : Value -> Result String Bool
bool value =
    case value of
        BoolNode b ->
            Ok b

        _ ->
            Err "Not a bool"


{-|

    Set a default for a result
        >> default "Cat" (Ok "Fish")

    Ok "Fish"
        >> default "Dog" (Err "flip")

    Ok "Dog"

-}
default : b -> Result a b -> Result a b
default b res =
    case res of
        Ok _ ->
            res

        Err _ ->
            Ok b


{-| Search an XML value for any tags matching

    import Xml exposing (Value(..))
    import Xml.Encode exposing (object, null, encode)
    import Dict

    object [ ("name", Dict.empty, null)] |> tags "name"
    --> [Tag "name" Dict.empty (Object []) ]

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
                ++ tags tagName value

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
