module LabelLayerItem exposing (LabelLayerItem(..))

import Label


type LabelLayerItem
    = Label Label.Label
    | CreateNew
