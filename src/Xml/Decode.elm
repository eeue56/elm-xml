module Xml.Decode exposing
    ( decode
    , decodeInt, decodeFloat, decodeString, decodeBool
    , decodeChildren
    )

{-|

@docs decode

@docs decodeInt, decodeFloat, decodeString, decodeBool

@docs decodeChildren

-}

import Dict
import Regex exposing (Regex)
import Xml exposing (Value(..))
import Xml.Encode as Encode


{-| Try and decode the props from a string
-}
decodeProps : String -> Result String Value
decodeProps str =
    List.foldl
        (\decoder val ->
            case val of
                Ok _ ->
                    val

                Err _ ->
                    decoder str
        )
        (Err "")
        [ decodeBool, decodeInt, decodeFloat, decodeString ]


parseProps : List String -> List ( String, Value )
parseProps =
    List.filterMap
        (\n ->
            case String.split "=" n of
                [ name, value ] ->
                    let
                        withoutQuotes =
                            value
                                |> String.dropLeft 1
                                |> String.dropRight 1
                    in
                    case decodeProps withoutQuotes of
                        Err _ ->
                            Nothing

                        Ok v ->
                            Just ( name, v )

                _ ->
                    Nothing
        )


propRegex : Maybe Regex.Regex
propRegex =
    Regex.fromString " .+?=\".+?\""


findProps : List String -> Dict.Dict String Value
findProps =
    case propRegex of
        Nothing ->
            \_ -> Dict.empty

        Just regex ->
            List.tail
                >> Maybe.withDefault []
                >> String.join " "
                >> (\s -> " " ++ s)
                >> Regex.find regex
                >> List.map (.match >> String.trim)
                >> parseProps
                >> Dict.fromList


parseSlice : Int -> Int -> String -> Result String ( Value, Int )
parseSlice first firstClose trimmed =
    let
        beforeClose =
            String.slice (first + 1) firstClose trimmed

        words =
            beforeClose
                |> String.words

        tagName =
            words
                |> List.head
                |> Maybe.withDefault ""

        props =
            findProps words

        closeTag =
            "</" ++ tagName ++ ">"
    in
    case String.indexes closeTag trimmed of
        [] ->
            if String.startsWith "?" tagName then
                Ok ( DocType tagName props, firstClose + 1 )

            else if String.endsWith "/" beforeClose then
                let
                    tag =
                        if String.endsWith "/" tagName then
                            String.slice 0 -1 tagName

                        else
                            tagName
                in
                Ok ( Tag tag props (Object []), firstClose + 1 )

            else
                "Failed to find close tag for "
                    ++ tagName
                    |> Err

        firstCloseTag :: _ ->
            let
                contents =
                    String.slice (firstClose + 1) firstCloseTag trimmed
            in
            case decodeChildren contents of
                Err s ->
                    Err s

                Ok v ->
                    Ok ( Tag tagName props v, firstCloseTag + String.length closeTag )


actualDecode : String -> Result String (List Value)
actualDecode text =
    let
        openIndexes =
            String.indexes "<" text

        closeIndexes =
            String.indexes ">" text
    in
    case ( openIndexes, closeIndexes ) of
        ( first :: restFirst, firstClose :: restFirstClose ) ->
            parseSlice first firstClose text
                |> Result.andThen
                    (\( foundValue, firstCloseTag ) ->
                        case actualDecode (String.slice firstCloseTag (String.length text + 1) text) of
                            Err err ->
                                if err == "Nothing left" then
                                    Ok [ foundValue ]

                                else
                                    Err ("Parsed to " ++ Encode.encode 0 foundValue ++ ", but then hit " ++ err)

                            Ok thing ->
                                [ foundValue ]
                                    ++ thing
                                    |> Ok
                    )

        _ ->
            Err "Nothing left"


{-| Try to decode a string and turn it into an XML value

    import Xml exposing(Value(..))
    import Xml.Encode exposing (null)
    import Dict

    decode "<name></name>"
    --> Ok (Object [Tag "name" Dict.empty null])

-}
decode : String -> Result String Value
decode text =
    case String.trim text of
        "" ->
            Ok (Object [])

        trimmed ->
            actualDecode trimmed
                |> Result.map Object


{-| Decode a string

    import Xml exposing (Value(..))

    decodeString "hello"
    --> Ok (StrNode "hello")

-}
decodeString : String -> Result String Value
decodeString str =
    StrNode str
        |> Ok


{-| Decode a int

    import Xml exposing (Value(..))

    decodeInt "hello"
    --> Err "could not convert string 'hello' to an Int"

    decodeInt "5"
    --> Ok (IntNode 5)

-}
decodeInt : String -> Result String Value
decodeInt str =
    case String.toInt str of
        Nothing ->
            Err <| "could not convert string '" ++ str ++ "' to an Int"

        Just v ->
            IntNode v
                |> Ok


{-| Decode a float

    import Xml exposing (Value(..))

    decodeFloat "hello"
    --> Err "could not convert string 'hello' to a Float"

    decodeFloat "5"
    --> Ok (FloatNode 5.0)

    decodeFloat "5.5"
    --> Ok (FloatNode 5.5)

-}
decodeFloat : String -> Result String Value
decodeFloat str =
    case String.toFloat str of
        Nothing ->
            Err <| "could not convert string '" ++ str ++ "' to a Float"

        Just v ->
            FloatNode v
                |> Ok


{-| Decode a bool
-}
decodeBool : String -> Result String Value
decodeBool str =
    if str == "true" then
        BoolNode True
            |> Ok

    else if str == "false" then
        BoolNode False
            |> Ok

    else
        Err "Not a bool"


{-| Decode children from a string

    import Dict
    import Xml exposing (Value(..))

    decodeChildren "<name>hello</name>"
    --> Ok (Object [Tag "name" Dict.empty (StrNode "hello")] )

-}
decodeChildren : String -> Result String Value
decodeChildren str =
    List.foldl
        (\decoder val ->
            case val of
                Ok _ ->
                    val

                Err _ ->
                    decoder str
        )
        (Err "")
        [ decode, decodeInt, decodeFloat, decodeString ]
