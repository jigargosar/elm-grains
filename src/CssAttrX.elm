module CssAttrX exposing (attrIf, noAttr)

import Html.Styled.Attributes as SA exposing (attribute, property)
import Json.Encode


noAttr =
    property "" Json.Encode.null


attrIf cond attr =
    if cond then
        attr

    else
        noAttr
