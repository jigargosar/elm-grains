module GrainMorePopupView exposing (viewGrainMorePopup)

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


viewGrainMorePopup grain messages =
    let
        viewEdit : Grain -> Html msg
        viewEdit g =
            flexRow [ CS.pointer, CS.p2 space2 zero ]
                [ onClick (GrainMoreAction <| routeToGrain g) ]
                [ flexCol [] [] [ text "Edit" ]
                , CssElements.iconBtnWithStyles [ CS.selfCenter ]
                    []
                    [ CssIcons.view CssIcons.modeEdit
                    ]
                ]

        viewMoveUp g =
            flexRow [ CS.pointer, CS.p2 space2 zero ]
                [ onClick (PopupActionMoveGrainUp g) ]
                [ flexCol [] [] [ text "Move Up" ]
                , CssElements.iconBtnWithStyles [ CS.selfCenter ]
                    []
                    [ CssIcons.view CssIcons.arrowUp
                    ]
                ]

        viewMoveDown g =
            flexRow [ CS.pointer, CS.p2 space2 zero ]
                [ onClick (PopupActionMoveGrainDown g) ]
                [ flexCol [] [] [ text "Move Down" ]
                , CssElements.iconBtnWithStyles [ CS.selfCenter ]
                    []
                    [ CssIcons.view CssIcons.arrowDown
                    ]
                ]

        viewNestUnder g =
            flexRow [ CS.pointer, CS.p2 space2 zero ]
                [ onClick (ShowMoveToPopup g) ]
                [ flexCol [] [] [ text "Nest Under..." ]
                , CssElements.iconBtnWithStyles [ CS.selfCenter ]
                    []
                    [ CssIcons.view CssIcons.dragHandle
                    ]
                ]

        viewDelete g =
            let
                deleted =
                    Grain.deleted g

                action =
                    (ter deleted RestoreGrain DeleteGrain <|
                        g
                    )
                        |> GrainMoreAction

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
                , viewEdit grain
                , viewMoveUp grain
                , viewMoveDown grain
                , viewNestUnder grain
                , viewDelete grain
                ]
            ]
        , onDismiss = DismissPopup
        }
