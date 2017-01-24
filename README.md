# elm-xml
xml parser for elm

First bring XML into Elm as a `Value`. Once imported as a Value, you can then either query the values with `Xml.Query`.

Or you can turn it back to a string using `Xml.Encode.encode`. Or pull it apart using `Xml.Encode.Value`.

```elm

import Xml.Encode exposing (Value, null)
import Xml.Decode exposing (decode)
import Xml.Query exposing (tags)

decodedXml : Value
decodedXml = 
	"""
<person>
	<name>noah</name>
	<age max="100">50</age>
</person>
<person>
	<name>josh</name>
	<age max="100">57</age>
</person>
	"""
		|> decode
		|> Result.toMaybe
		|> Maybe.withDefault null


people : List Value
people =
	tags "person" decodedXml

```