module Xml.Decode exposing (decode, decodeInt, decodeString, decodeFloat, decodeBool, decodeChildren)

import Dict
import Xml.Encode exposing (Value(..))


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
                    case decodeProps <| String.dropLeft 1 <| String.dropRight 1 <| value of
                        Err _ ->
                            Nothing

                        Ok v ->
                            Just ( name, v )

                _ ->
                    Nothing
        )


parseSlice : Int -> Int -> String -> Result String ( Value, Int )
parseSlice first firstClose trimmed =
    let
        beforeClose =
            String.slice (first + 1) firstClose trimmed
                |> String.words

        tagName =
            beforeClose
                |> List.head
                |> Maybe.withDefault ""

        props =
            List.drop 1 beforeClose
                |> parseProps
                |> Dict.fromList

        closeTag =
            "</" ++ tagName ++ ">"
    in
        case String.indexes closeTag trimmed of
            [] ->
                if String.startsWith "?" tagName then
                    Ok ( DocType tagName props, firstClose + 1 )
                else
                    "Failed to find close tag for "
                        ++ tagName
                        |> Err

            firstCloseTag :: _ ->
                let
                    contents =
                        String.slice (firstClose + 1) (firstCloseTag) trimmed
                in
                    case decodeChildren contents of
                        Err s ->
                            Err s

                        Ok v ->
                            Ok ( Tag tagName props v, firstCloseTag + (String.length closeTag) )


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
                                        Err ("Parsed to " ++ toString foundValue ++ ", but then hit " ++ err)

                                Ok thing ->
                                    [ foundValue ]
                                        ++ thing
                                        |> Ok
                        )

            _ ->
                Err "Nothing left"


{-| Try to decode a string and turn it into an XML value
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
-}
decodeString : String -> Result String Value
decodeString str =
    StrNode str
        |> Ok


{-| Decode a int
-}
decodeInt : String -> Result String Value
decodeInt str =
    case String.toInt str of
        Err s ->
            Err s

        Ok v ->
            IntNode v
                |> Ok


{-| Decode a float
-}
decodeFloat : String -> Result String Value
decodeFloat str =
    case String.toFloat str of
        Err s ->
            Err s

        Ok v ->
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
