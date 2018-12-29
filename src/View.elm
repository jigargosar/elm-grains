module View exposing (AppBarView, ViewModel, view)

import BasicsX exposing (ter)
import Css exposing (num, pct, px, rem, vh, vw, zero)
import CssAttrX exposing (attrIf)
import CssElements
import CssEventX
import CssHtml
import CssIcons
import CssLayout exposing (flexCol, flexRow)
import CssShorthand as CS
import CssTheme
    exposing
        ( black80
        , blackAlpha
        , space1
        , space2
        , space4
        , white
        )
import DatGui
import EventX
import Firebase
import GrainId exposing (GrainId)
import GrainMorePopupView exposing (GrainMorePopupView)
import GrainTreeView exposing (GrainTreeView)
import Html.Styled
    exposing
        ( Html
        , button
        , div
        , dt
        , input
        , styled
        , text
        , textarea
        )
import Html.Styled.Attributes
    exposing
        ( autocomplete
        , class
        , css
        , id
        , rows
        , tabindex
        , value
        )
import Html.Styled.Events
    exposing
        ( onBlur
        , onClick
        , onDoubleClick
        , onFocus
        , onInput
        )
import Maybe.Extra as Maybe
import MoveGrainPopupView exposing (MoveGrainPopupView)
import NotFoundView
import Popup exposing (Popup)
import Route exposing (Route)
import Skeleton
import Toast exposing (Toast)


type alias ViewModel msg =
    { route : Route
    , popup : Popup
    , appBarVM : AppBarView msg
    , createGrainTreeVM :
        GrainId -> Maybe (GrainTreeView msg)
    , createGrainMorePopupVM :
        GrainId -> Maybe (GrainMorePopupView msg)
    , createGrainMovePopupVM :
        GrainId -> Maybe (MoveGrainPopupView msg)
    , toastVM :
        { dismissMsg : msg
        , toast : Toast
        }
    }


view vm =
    Html.Styled.toUnstyled <|
        Skeleton.view
            { children =
                [ viewAppBar vm.appBarVM ]
                    ++ viewRouteChildren vm
                    ++ [ viewToast vm.toastVM
                       , viewPopup vm
                       , DatGui.view
                            [ DatGui.boolean "Debug" False
                            , DatGui.integer "Counter" 100
                            ]
                       ]
            }


viewToast vm =
    Toast.view vm.dismissMsg vm.toast


viewPopup vm =
    case vm.popup of
        Popup.GrainMorePopup gid ->
            vm.createGrainMorePopupVM gid
                |> CssHtml.viewMaybe GrainMorePopupView.view

        Popup.GrainMovePopup gid ->
            vm.createGrainMovePopupVM gid
                |> CssHtml.viewMaybe MoveGrainPopupView.view

        Popup.NoPopup ->
            CssHtml.noView


viewRouteChildren vm =
    case vm.route of
        Route.GrainTree gid ->
            vm.createGrainTreeVM gid
                |> Maybe.unwrap NotFoundView.view GrainTreeView.view

        Route.NotFound string ->
            NotFoundView.view


type alias AppBarView msg =
    { title : String
    , onBack : Maybe msg
    , authState : Firebase.AuthState
    , signOutMsg : msg
    , signInMsg : msg
    }


viewAppBar : AppBarView msg -> Html msg
viewAppBar vm =
    let
        viewTitle =
            styled div
                [ CS.p2 space2 zero
                , CS.flex11Auto
                , Css.textAlign Css.center
                ]
                []
                [ text vm.title ]

        viewBackBtn onBack =
            button [ class "btn", onClick onBack ] [ text "Back" ]

        viewAuthState =
            case vm.authState of
                Firebase.AuthStateLoading ->
                    button [ class "btn loading" ]
                        [ text "SignIn" ]

                Firebase.AuthStateUser user ->
                    button [ class "btn", onClick vm.signOutMsg ]
                        [ text "SignOut" ]

                Firebase.AuthStateNoUser ->
                    button [ class "btn", onClick vm.signInMsg ]
                        [ text "SignIn" ]
    in
    CssLayout.flexRow
        [ CS.sticky
        , Css.top <| px 0
        , CS.p2 zero space2
        , CS.itemsCenter
        ]
        [ class "bg-dark" ]
        [ CssHtml.viewMaybe viewBackBtn vm.onBack
        , viewTitle
        , viewAuthState
        ]
