module View exposing (view)

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
import EventX
import Firebase
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
import Skeleton


type alias AppBarView msg =
    { title : String
    , maybeBackButtonMsg : Maybe msg
    , authState : Firebase.AuthState
    , signOutMsg : msg
    , signInMsg : msg
    }


view vm =
    Skeleton.view
        { children =
            [ viewAppBar vm.appBar ]

        --                    ++ viewRouteChildren model
        --                    ++ [ viewToast model.toast
        --                       , viewPopup model
        --                       , DatGui.view
        --                            [ DatGui.boolean "Debug" False
        --                            , DatGui.integer "Counter" 100
        --                            ]
        --                       ]
        }


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

        viewBackBtn backMsg =
            button [ class "btn", onClick backMsg ] [ text "Back" ]

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
        [ CssHtml.viewMaybe viewBackBtn vm.maybeBackButtonMsg
        , viewTitle
        , viewAuthState
        ]
