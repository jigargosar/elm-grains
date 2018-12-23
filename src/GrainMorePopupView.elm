module GrainMorePopupView exposing (GrainMorePopupView, view)

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


type alias GrainMorePopupView msg =
    { editMsg : msg
    , moveUpMsg : msg
    , moveDownMsg : msg
    , moveToMsg : msg
    , toggleDeleteMsg : msg
    , dismissMsg : msg
    , deleted : Bool
    }


view : GrainMorePopupView msg -> Html msg
view vm =
    let
        viewEdit =
            flexRow [ CS.pointer, CS.p2 space2 zero ]
                [ onClick vm.editMsg ]
                [ flexCol [] [] [ text "Edit" ]
                , CssElements.iconBtnWithStyles [ CS.selfCenter ]
                    []
                    [ CssIcons.view CssIcons.modeEdit
                    ]
                ]

        viewMoveUp =
            flexRow [ CS.pointer, CS.p2 space2 zero ]
                [ onClick vm.moveUpMsg ]
                [ flexCol [] [] [ text "Move Up" ]
                , CssElements.iconBtnWithStyles [ CS.selfCenter ]
                    []
                    [ CssIcons.view CssIcons.arrowUp
                    ]
                ]

        viewMoveDown =
            flexRow [ CS.pointer, CS.p2 space2 zero ]
                [ onClick vm.moveDownMsg ]
                [ flexCol [] [] [ text "Move Down" ]
                , CssElements.iconBtnWithStyles [ CS.selfCenter ]
                    []
                    [ CssIcons.view CssIcons.arrowDown
                    ]
                ]

        viewNestUnder =
            flexRow [ CS.pointer, CS.p2 space2 zero ]
                [ onClick vm.moveToMsg ]
                [ flexCol [] [] [ text "Nest Under..." ]
                , CssElements.iconBtnWithStyles [ CS.selfCenter ]
                    []
                    [ CssIcons.view CssIcons.dragHandle
                    ]
                ]

        viewDelete =
            let
                deleted =
                    vm.deleted

                action =
                    vm.toggleDeleteMsg

                actionTitle =
                    ter deleted "Restore" "Trash"

                icon =
                    ter deleted CssIcons.restore CssIcons.delete
            in
            flexRow [ CS.pointer, CS.p2 space2 zero ]
                [ onClick action ]
                [ flexCol [] [] [ text actionTitle ]
                , CssElements.iconBtnWithStyles [ CS.selfCenter ]
                    []
                    [ CssIcons.view icon
                    ]
                ]
    in
    CssProto.modal
        { content =
            [ flexCol []
                []
                [ flexRow [ CS.justifyCenter ] [] [ text "Grain Menu" ]
                , viewEdit
                , viewMoveUp
                , viewMoveDown
                , viewNestUnder
                , viewDelete
                ]
            ]
        , onDismiss = vm.dismissMsg
        }
