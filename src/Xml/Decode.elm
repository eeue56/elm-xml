module Xml.Decode exposing (..)

import Dict
import Xml.Encode exposing (Value(..))


parseSlice : Int -> Int -> String -> Result String ( Value, Int )
parseSlice first firstClose trimmed =
    let
        tagName =
            String.slice (first + 1) firstClose trimmed

        closeTag =
            "</" ++ tagName ++ ">"
    in
        case String.indexes closeTag trimmed of
            [] ->
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
                            Ok ( Tag tagName Dict.empty v, firstCloseTag + (String.length closeTag) )


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


decode : String -> Result String Value
decode text =
    case String.trim text of
        "" ->
            Ok (Object [])

        trimmed ->
            actualDecode trimmed
                |> Result.map Object


decodeString : String -> Result String Value
decodeString str =
    StrNode str
        |> Ok


decodeInt : String -> Result String Value
decodeInt str =
    case String.toInt str of
        Err s ->
            Err s

        Ok v ->
            IntNode v
                |> Ok


decodeFloat : String -> Result String Value
decodeFloat str =
    case String.toFloat str of
        Err s ->
            Err s

        Ok v ->
            FloatNode v
                |> Ok


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
