module MoveGrainPopupView exposing (view)

import BasicsX exposing (ter)
import Css exposing (zero)
import CssElements
import CssIcons
import CssLayout exposing (flexCol, flexRow)
import CssProto
import CssShorthand as CS
import CssTheme exposing (space2)
import Grain exposing (Grain)
import Html.Styled exposing (Html, text)
import Html.Styled.Events exposing (onClick)


view vm =
    let
        viewGrainItem g =
            flexCol
                [ CS.pointer
                , CS.styleIf (vm.isSelected g) CS.bold
                , Css.hover
                    [ Css.property "background-color" "lightgray"
                    ]
                ]
                [ onClick <|
                    vm.setParentMsg (Grain.idAsParentId g)
                ]
                [ flexRow [ CS.ellipsis ]
                    []
                    [ text <| Grain.titleOrEmpty g
                    ]
                ]

        viewRootItem =
            let
                isRoot =
                    Grain.parentIdEq Grain.rootParentId vm.grain
            in
            flexCol
                [ CS.pointer
                , CS.styleIf isRoot CS.bold
                , Css.hover
                    [ Css.property "background-color" "lightgray"
                    ]
                ]
                [ onClick vm.setParentToRootMsg
                ]
                [ flexRow [ CS.ellipsis ]
                    []
                    [ text "<Root>"
                    ]
                ]
    in
    CssProto.modal
        { content =
            [ flexCol []
                []
                [ flexRow [ CS.justifyCenter ] [] [ text "Move Grain" ]
                , flexCol []
                    []
                    (viewRootItem
                        :: List.map viewGrainItem vm.otherGrains
                    )
                ]
            ]
        , onDismiss = vm.dismissMsg
        }
