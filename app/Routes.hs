{-# LANGUAGE OverloadedStrings #-}

module Routes where

import Htmx.Event (HtmxEvent (..))
import Htmx.Lucid.Core (OnEvent (..), hxGet_, hxOn_, hxPost_, hxPushUrl_, hxSwapOob_, hxSwap_, hxTarget_, hxTrigger_)
import Htmx.Lucid.Extra (hxConfirm_, hxDelete_, hxPut_)
import Lucid
import Web.Route.Invertible
import Web.Route.Invertible.Common

-- 1. Endpoint specification
-- https://hackage.haskell.org/package/web-inv-route-0.1.3.2/docs/Web-Route-Invertible-Common.html
--
getHomeR :: RouteAction () (Html ())
getHomeR = routeMethod GET *< routePath ("/") `RouteAction` \() -> testForm

myRoutes :: RouteMap (Html ())
myRoutes =
    routes
        [routeCase getHomeR]

testForm :: Html ()
testForm =
    doctypehtml_ $ do
        head_ $
            do
                title_
                    "goblins"
                script_ [defer_ "", src_ "/htmx"] ("" :: Html ())
        -- useHtmx
        body_ $
            do
                form_
                    [hxPost_ "/form", hxTarget_ "#todos", hxSwap_ "beforeend", hxOn_ (HtmxOnEvent AfterRequest) "this.reset()"]
                    $ do
                        input_ [type_ "text", name_ "msg"]
                        input_ [type_ "submit", value_ "Add Todo"]
                div_ [hxGet_ "/todos", hxTrigger_ "load", hxSwap_ "outerHTML"] ""
                div_ [hxPost_ "/mouse_entered", hxTrigger_ "mouseenter"] "[Here Mouse, Mouse!]"

                template_
                    $ do
                        tr_ [id_ "message", hxSwapOob_ "true"]
                    $ do
                        td_ "Joe"
                        td_ "Smith"
                        ""

                -- I added this randomly as html exploration
                button_
                    [ hxDelete_ "/account"
                    , hxConfirm_ "Are you sure you wish to delete your account?"
                    ]
                    "Delete My Account"
                div_ [hxConfirm_ "Are you sure?"] $
                    do
                        button_ [hxDelete_ "/account"] "Delete My Account"
                        button_ [hxPut_ "/account"] "Update My Account"
                        button_ [hxConfirm_ "unset", hxPushUrl_ "true", hxGet_ "/fake_path_FOO"] "Cancel"
                        ""
